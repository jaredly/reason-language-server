
open Infix;
open Result;

open Log;

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
      ("completionProvider", o([
        ("resolveProvider", t),
        /* TODO list # as trigger character */
        ("triggerCharacters", l([s(".")]))
      ])),
      ("signatureHelpProvider", o([
        ("triggerCharacters", l([s("(")]))
      ])),
      ("definitionProvider", t),
      ("typeDefinitionProvider", t),
      ("referencesProvider", t),
      ("documentSymbolProvider", t),
      /* ("codeActionProvider", t), */
      ("codeLensProvider", o([
        ("resolveProvider", t)
      ])),
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

let getInitialState = (params) => {
  let uri = Json.get("rootUri", params) |?> Json.string |! "Must have a rootUri";
  let path = uri |> Utils.parseUri |> resultOfOption("No root uri");
  open InfixResult;
  path |?> rootPath => {
    Files.mkdirp(rootPath /+ "node_modules");
    Files.mkdirp(rootPath /+ "node_modules" /+ ".lsp");
    Log.setLocation(rootPath /+ "node_modules" /+ ".lsp" /+ "debug.log");
    Files.readFile(rootPath /+ "bsconfig.json") |> orError("No bsconfig.json found") |?>> Json.parse |?>> config => {
      let compiledBase = FindFiles.getCompiledBase(rootPath, config);
      let compiledBase = switch compiledBase {
        | None => {
          raise(BasicServer.Exit("You need to run bsb first so that reason-language-server can access the compiled artifacts.\nOnce you've run bsb, restart the language server."));
        }
        | Some(x) => x
      };
      let localSourceDirs = FindFiles.getSourceDirectories(~includeDev=true, rootPath, config);
      let localCompiledDirs = localSourceDirs |> List.map(Infix.fileConcat(compiledBase));
      let localModules = FindFiles.findProjectFiles(~debug=false, None, rootPath, localSourceDirs, compiledBase) |> List.map(((full, rel)) => (FindFiles.getName(rel), (full, rel)));
      let (dependencyDirectories, dependencyModules) = FindFiles.findDependencyFiles(~debug=false, rootPath, config);
      let cmtCache = Hashtbl.create(30);
      let documentText = Hashtbl.create(5);

      let pathsForModule = Hashtbl.create(30);
      localModules |> List.iter(((modName, (cmt, source))) => {
        Log.log("> Local " ++ cmt ++ " - " ++ source);
        Hashtbl.replace(pathsForModule, modName, (cmt, Some(source)))
      });
      Log.log("Depedency dirs " ++ String.concat(" ", dependencyDirectories));

      dependencyModules |> List.iter(((modName, (cmt, source))) => {
        Log.log("Dependency " ++ cmt ++ " - " ++ Infix.(source |? ""));
        switch (modName) {
        | FindFiles.Plain(name) =>
        Hashtbl.replace(pathsForModule, name, (cmt, source))
        | _ => ()
        }
      });

      let clientCapabilities = Infix.(State.{
        hoverMarkdown:
          Json.getPath("capabilities.textDocument.hover.contentFormat", params) |?> Protocol.hasMarkdownCap |? true,
        completionMarkdown:
          Json.getPath("capabilities.textDocument.completion.completionItem.documentationFormat", params)
            |?> Protocol.hasMarkdownCap |? true,
      });

      State.{
        rootPath: rootPath,
        rootUri: uri,
        compilerPath: FindFiles.isNative(config) ?
          rootPath /+ "node_modules" /+ "bs-platform" /+ "vendor" /+ "ocaml" /+ "ocamlopt.opt -c"
          : rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "bsc.exe",
        refmtPath: FindFiles.oneShouldExist("Can't find refmt", [
          rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "refmt3.exe",
          rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "refmt.exe",
        ]),
        documentText,
        localCompiledBase: compiledBase,
        localModules,
        localCompiledMap: localModules |> List.map(((_, (cmt, src))) => (src, cmt)),
        dependencyModules,
        cmtCache,
        compilationFlags: MerlinFile.getFlags(rootPath) |> Result.withDefault([""]) |> String.concat(" "),
        pathsForModule,
        compiledDocuments: Hashtbl.create(10),
        lastDefinitions: Hashtbl.create(10),
        documentTimers: Hashtbl.create(10),
        includeDirectories: [
          FindFiles.isNative(config)
          ? rootPath /+ "node_modules" /+ "bs-platform/" /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"
          : rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "ocaml",
          ...dependencyDirectories
        ] @ localCompiledDirs,
        clientCapabilities,
        /* docs, */
      }
    };
  }
};

open State;

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



let main = () => {
  log("Booting up");
  BasicServer.run(
    ~tick,
    ~log,
    ~messageHandlers=MessageHandlers.handlers,
    ~notificationHandlers,
    ~capabilities=_params => capabilities,
    ~getInitialState
  );
  log("Finished");
  out^ |?< close_out;
  /*  close_out(out^); */
};
