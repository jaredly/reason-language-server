open Infix;
open Result;

open Log;

let extend = (obj, items) => Json.obj(obj) |?>> current => Json.Object(current @ items);

/**
 * we get initialized, with a rootUri
 * if the rootPath doesn't have a bsconfig.json, show that as a notification, and be quiet afterwards
 *
 * load up dependencies, probably
 * although maybe I only want to load them on a case by case?
 * yeah that could get memory hoggy quick
 * also it would be nice to clean house occasionally, have a LRU cache or something.
 *
 * Anyway, let's talk about completion.
 * I'll do my hacky parser to detect "open"s, so I know what things to load up
 * We'll defer as much as possible, but:
 *
 * First, have a listing of all accessible modules (name, (cmti, sourcefile))
 *
 * then, if it gets requested via autocomplete, load the cmti to get infos
 *
 * also, if it's "open"d, we'll load it up n show it.
 *
 * And I'll do the thing where I assume pervasives is open.
 *
 *
 * Probably punt on documentation for the moment.
*/;

let capabilities =
  Rpc.J.(
    o([
      ("textDocumentSync", i(1)),
      ("hoverProvider", t),
      /* TODO list # and . as trigger characters */
      ("completionProvider", o([
        ("resolveProvider", t),
        ("triggerCharacters", l([s(".")]))
      ])),
      ("signatureHelpProvider", t),
      ("definitionProvider", t),
      ("typeDefinitionProvider", t),
      ("referencesProvider", t),
      ("documentSymbolProvider", t),
      /* ("codeActionProvider", t), */
      ("codeLensProvider", t),
      ("documentHighlightProvider", t),
      ("renameProvider", t),
      ("documentRangeFormattingProvider", t),
      ("documentFormattingProvider", t),
      /*
       * Found how to do the showReferences thing
       * https://github.com/Microsoft/vscode/blob/c6b1114292288e76e2901e05e860faf3a08b4b5a/extensions/typescript-language-features/src/features/implementationsCodeLensProvider.ts
       * but it seems I need to instantiate the object from javascript
       */
      /* ("executeCommandProvider", o([

      ])) */
      /* ("executeCommandOptions", t), */
      ("documentFormattingProvider", t),
      ("renameProvider", t)
    ])
);

let parseUri = uri => {
  if (Utils.startsWith(uri, "file://")) {
    Some(Utils.sliceToEnd(uri, String.length("file://")))
  } else {
    None
  }
};

open State;

let maybeHash = (h, k) => if (Hashtbl.mem(h, k)) { Some(Hashtbl.find(h, k)) } else { None };

let getInitialState = (params) => {
  let uri = Json.get("rootUri", params) |?> Json.string |?> parseUri |> resultOfOption("No root uri");
  open InfixResult;
  uri |?> uri => {
    Files.readFile(uri /+ "bsconfig.json") |> orError("No bsconfig.json found") |?>> Json.parse |?>> config => {
      let compiledBase = FindFiles.getCompiledBase(uri, config);
      let localModules = FindFiles.findProjectFiles(~debug=false, None, uri, config, compiledBase) |> List.map(((full, rel)) => (FindFiles.getName(rel), (full, rel)));
      let dependencyModules = FindFiles.findDependencyFiles(~debug=false, uri, config);
      let cmtCache = Hashtbl.create(30);
      let documentText = Hashtbl.create(5);

      let pathsForModule = Hashtbl.create(30);
      localModules |> List.iter(((modName, (cmt, source))) => {
        Hashtbl.replace(pathsForModule, modName, (cmt, source))
      });

      dependencyModules |> List.iter(((modName, (cmt, source))) => {
        switch (modName) {
        | FindFiles.Plain(name) =>
        Hashtbl.replace(pathsForModule, name, (cmt, source))
        | _ => ()
        }
      });

      {
        rootPath: uri,
        documentText,
        localCompiledBase: compiledBase,
        localModules,
        localCompiledMap: localModules |> List.map(((_, (cmt, src))) => (src, cmt)),
        dependencyModules,
        cmtCache,
        compilationFlags: MerlinFile.getFlags(uri) |> Result.withDefault([""]) |> String.concat(" "),
        pathsForModule,
        compiledDocuments: Hashtbl.create(10),
        lastDefinitions: Hashtbl.create(10),
        documentTimers: Hashtbl.create(10),
        includeDirectories: [
          uri /+ "node_modules/bs-platform/lib/ocaml",
          uri /+ "lib/bs/src"
        ]
        /* docs, */
      }
    };
  }
};

let getTextDocument = doc => Json.get("uri", doc) |?> Json.string
    |?> uri => Json.get("version", doc) |?> Json.number
    |?> version => Json.get("text", doc) |?> Json.string
    |?>> text => (uri, version, text);

let runDiagnostics = (uri, state) => {
  /* let (text, _, _) =  */
  let result = State.getCompilationResult(uri, state);
  open Rpc.J;
  Rpc.sendNotification(log, stdout, "textDocument/publishDiagnostics", o([
    ("uri", s(uri)),
    ("diagnostics", switch result {
    | AsYouType.ParseError(text) => {
      let pos = AsYouType.parseTypeError(text);
      let (l0, c0, l1, c1) = switch pos {
      | None => (0, 0, 0, 0)
      | Some((line, c0, c1)) => (line, c0, line, c1)
      };
      l([o([
        ("range", Protocol.rangeOfInts(l0, c0, l1, c1)),
        ("message", s("Parse error: " ++ text)),
        ("severity", i(1)),
      ])])
    }
    | Success(lines, _, _) => {
      if (lines == [] || lines == [""]) {
        l([])
      } else {
        let rec loop = lines => switch lines {
        | [loc, warning, ...rest] => switch (AsYouType.parseTypeError(loc)) {
          | None => loop([warning, ...rest])
          | Some((line, c0, c1)) => {
            [o([
              ("range", Protocol.rangeOfInts(line, c0, line, c1)),
              ("message", s(warning)),
              ("severity", i(2)),
            ]), ...loop(rest)]
          }
        }
        | _ => []
        };
        let warnings = loop(lines);
        l(warnings)
      }
    }
    | TypeError(text, _, _) => {
      let pos = AsYouType.parseTypeError(text);
      let (l0, c0, l1, c1) = switch pos {
      | None => (0, 0, 0, 0)
      | Some((line, c0, c1)) => (line, c0, line, c1)
      };
      l([o([
        ("range", o([
          ("start", Protocol.pos(~line=l0, ~character=c0)),
          ("end", Protocol.pos(~line=l1, ~character=c1)),
        ])),
        ("message", s("Type error! " ++ text)),
        ("severity", i(1)),
      ])])
    }
    })
  ]));

};

let checkDocumentTimers = state => {
  let now = Unix.gettimeofday();
  let removed = Hashtbl.fold((uri, timer, removed) => {
    if (now > timer) {
      runDiagnostics(uri, state);
      [uri, ...removed]
    } else {
      removed
    }
  }, state.documentTimers, []);
  List.iter(uri => Hashtbl.remove(state.documentTimers, uri), removed);
  state
};

let tick = state => {
  checkDocumentTimers(state);
};

let recompileDebounceTime = 0.5; /* seconds */

let notificationHandlers: list((string, (state, Json.t) => result(state, string))) = [
  ("textDocument/didOpen", (state, params) => {
    (params |> Json.get("textDocument") |?> getTextDocument |?>> ((uri, version, text))  => {
      Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), true));
      Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() +. recompileDebounceTime);
      state
    }) |> orError("Invalid params")
  }),
  ("textDocument/didChange", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> doc => RJson.get("uri", doc) |?> RJson.string
    |?> uri => RJson.get("version", doc) |?> RJson.number
    |?> version => RJson.get("contentChanges", params) |?> RJson.array
    |?> changes => List.nth(changes, List.length(changes) - 1) |> RJson.get("text") |?> RJson.string
    |?>> text => {
      /* Hmm how do I know if it's modified? */
      let state = State.updateContents(uri, text, version, state);
      Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() +. recompileDebounceTime);
      state
    }
  }),
];

let markup = text => Json.Object([("kind", Json.String("markdown")), ("value", Json.String(text))]);

type handler = Handler(string, Json.t => result('a, string), (state, 'a) => result((state, Json.t), string)) : handler;

let messageHandlers: list((string, (state, Json.t) => result((state, Json.t), string))) = [
  ("textDocument/definition", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string
    |?> uri => RJson.get("position", params) |?> Protocol.rgetPosition
    |?> position => {
      switch (State.getDefinitionData(uri, state)) {
      | None => Error("Parse error, can't find definition")
      | Some(data) => switch (State.definitionForPos(uri, position, data, state)) {
      | None => Ok((state, Json.Null))
      | Some((loc, docs, uri)) => Ok((state, Json.Object([
        ("uri", Json.String(uri)),
        ("range", Protocol.rangeOfLoc(loc)),
      ])))
      }
      }
    }
  }),

  ("textDocument/completion", (state, params) => {
    open InfixResult;
    (Protocol.rPositionParams(params) |?> ((uri, pos)) => (maybeHash(state.documentText, uri) |> orError("No document text found"))
    |?> ((text, version, isClean)) => (PartialParser.positionToOffset(text, pos) |> orError("invalid offset")) |?>> offset => {
        open Rpc.J;
        let completions = switch (PartialParser.findCompletable(text, offset)) {
        | Nothing => []
        | Labeled(string) => []
        | Lident(string) => {
          log("Completing for string " ++ string);
          let parts = Str.split(Str.regexp_string("."), string);
          let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
          let currentModuleName = String.capitalize(Filename.chop_extension(Filename.basename(uri)));
          let opens = PartialParser.findOpens(text, offset);
          /* */
          let localData = State.getLastDefinitions(uri, state);
          Completions.get(currentModuleName, opens, parts, state, localData, pos) |> List.map(({Completions.kind, uri, label, detail, documentation}) => o([
            ("label", s(label)),
            ("kind", i(Completions.kindToInt(kind))),
            ("detail", Infix.(detail |?>> s |? null)),
            ("documentation", Infix.((documentation |?>> d => d ++ "\n\n*" ++ (
              Utils.startsWith(uri, state.rootPath ++ "/") ? Utils.sliceToEnd(uri, String.length(state.rootPath ++ "/")) : uri
              ) ++ "*") |?>> markup |? null)),
            ("data", switch kind {
              | RootModule(cmt, src) => o([("cmt", s(cmt)), ("src", s(src)), ("name", s(label))])
              | _ => null
              })
          ]))
        }
        };
        (state, l(completions))
      }
    );
  }),
  ("completionItem/resolve", (state, params) => {
    switch (params |> Json.get("documentation") |?> Json.string) {
    | Some(_) => Ok((state, params))
    | None =>
      let result = (params |> Json.get("data")
      |?> data => Json.get("cmt", data) |?> Json.string
      |?> cmt => Json.get("src", data) |?> Json.string
      |?> src => Json.get("name", data) |?> Json.string
      |?>> name => {
        let (detail, docs) = Completions.getModuleResults(name, state, cmt, src);

        open Rpc.J;
        extend(params, [
          ("detail", detail |?>> s |? null),
          ("documentation", docs |?>> markup |? null),
        ]) |? params
      }) |? params;
      Ok((state, result))
    }
  }),

  ("textDocument/documentHighlight", (state, params) => {
    open InfixResult;
    Protocol.rPositionParams(params) |?>> ((uri, pos)) => {
      open Infix;
      let highlights = (State.getDefinitionData(uri, state) |?> data => Definition.highlights(pos, data)) |? [];
      open Rpc.J;
      (state, l(highlights |> List.map(((t, loc)) => o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("kind", i(switch t {
        | `Read => 2
        | `Write => 3
        }))
        ]))))
    };
  }),

  ("textDocument/rename", (state, params) => {
    open InfixResult;
    Protocol.rPositionParams(params) |?> ((uri, pos)) => RJson.get("newName", params) |?> RJson.string
    |?> newName => {
      open Infix;
      (State.getDefinitionData(uri, state)
      |?> data => Definition.stampAtPos(pos, data)
      |?> stamp => Definition.isStampExported(stamp, data)
      ? Some(Error("Cannot yet rename items that might be exported"))
      : {
        Definition.highlightsForStamp(stamp, data) |?>> positions => {

        open Rpc.J;
        Ok((state, o([
          ("changes", o([
            (uri, l(positions |> List.map(((_, loc)) => o([
              ("range", Protocol.rangeOfLoc(loc)),
              ("newText", s(newName)),
            ]))))
          ]))
        ])))
        }
      }) |? Ok((state, Json.Null))
    };
  }),

  ("textDocument/codeLens", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string
    |?> uri => {
      open Infix;
      /* let items = */
      let items = State.getCompilationResult(uri, state) |> AsYouType.getResult |?>> snd
      |?# lazy(State.getLastDefinitions(uri, state))
      |?>> (((moduleData)) => {

        let lenses = false ? Definition.listTopLevel(moduleData) |> Utils.filterMap(((name, loc, item, docs, scope)) => {
          switch item {
          | Definition.Module(_) => None
          | Type(t) => None
          /* TODO maybe types are useful? but maybe it's redundant. */
          /* | Type(t) => PrintType.default.decl(PrintType.default, name, name, t) |> PrintType.prettyString |> s => Some((s, loc)) */
          | Value(t) => PrintType.default.expr(PrintType.default, t) |> PrintType.prettyString |> s => Some((s, loc))
          }
        }) : [];

        let lenses = true ? lenses @ Definition.opens(moduleData) : [];

        lenses
      });
      switch items {
      | None => Error("Could not get compilation data")
      | Some(items) => {

      open Rpc.J;
      Ok((state, l(items |> List.map(((text, loc)) => o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("command", o([
          ("title", s(text)),
          ("command", s(""))
        ]))
      ])))))
      }
      }
    };
  }),
  ("textDocument/hover", (state, params) => {
    open InfixResult;
    Protocol.rPositionParams(params) |?>> ((uri, (line, character))) => {
      open Rpc.J;
      switch (Hover.getHover(uri, line, character, state)) {
      | None => (state, Json.Null)
      | Some((text, loc)) =>
      (state, o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("contents", o([
          ("kind", s("markdown")),
          ("value", s(text))
        ]))
      ]))
      }
    }
  }),

  ("textDocument/rangeFormatting", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string
    |?> uri => RJson.get("range", params) |?> Protocol.rgetRange
    |?> ((start, end_)) => {
      let text = State.getContents(uri, state);
      open Infix;
      (PartialParser.positionToOffset(text, start)
      |?> startPos => PartialParser.positionToOffset(text, end_)
      |?>> endPos => {
        let substring = String.sub(text, startPos, endPos - startPos);
        open InfixResult;
        AsYouType.format(substring, state.rootPath) |?>> text => {
          open Rpc.J;
          (state, l([o([
            ("range", Infix.(|!)(Json.get("range", params), "what")),
            ("newText", s(text))
          ])]))
        }
      }) |? Error("Invalid position")
    }
  }),

  ("textDocument/formatting", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string
    |?> uri => {
      let text = State.getContents(uri, state);
      AsYouType.format(text, state.rootPath) |?>> newText => {
        open Rpc.J;
        (state, text == newText ? Json.Null : l([o([
          ("range", Protocol.rangeOfInts(
            0, 0,
            List.length(Str.split(Str.regexp_string("\n"), text)) + 1, 0
          )),
          ("newText", s(newText))
        ])]))
      }
    }
  })
];



let main = () => {
  log("Booting up");
  BasicServer.run(
    ~tick,
    ~log,
    ~messageHandlers,
    ~notificationHandlers,
    ~capabilities=_params => capabilities,
    ~getInitialState
  );
  log("Finished");
  close_out(out);
};