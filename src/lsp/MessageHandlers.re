
open Result;
open TopTypes;
open Infix;

let extend = (obj, items) => Json.obj(obj) |?>> current => Json.Object(current @ items);

let log = Log.log;

let maybeHash = (h, k) => if (Hashtbl.mem(h, k)) { Some(Hashtbl.find(h, k)) } else { None };
type handler = Handler(string, Json.t => result('a, string), (state, 'a) => result((state, Json.t), string)) : handler;

let handlers: list((string, (state, Json.t) => result((state, Json.t), string))) = [
  ("textDocument/definition", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    let%try position = RJson.get("position", params) |?> Protocol.rgetPosition;
    let%try package = State.getPackage(uri, state);
    let%try data = State.getDefinitionData(uri, state, ~package) |> Result.orError("Parse error, can't find definition");

    let position = Utils.cmtLocFromVscode(position);

    open Infix;
    {
      let%opt (uri, loc) = References.definitionForPos(
        ~file=data.file,
        ~extra=data.extra,
        ~getModule=State.fileForModule(state, ~package),
        position
      );
      Some(Ok((state, Json.Object([
        ("uri", Json.String(uri)),
        ("range", Protocol.rangeOfLoc(loc)),
      ]))))
    } |? Ok((state, Json.Null))
  }),

  /** TODO implement */
  ("textDocument/signatureHelp", (state, params) => {
    Ok((state, Json.Null))
  }),

  ("textDocument/completion", (state, params) => {
    open InfixResult;
    (Protocol.rPositionParams(params) |?> ((uri, pos)) => (maybeHash(state.documentText, uri) |> orError("No document text found"))
    |?> ((text, version, isClean)) => State.getPackage(uri, state) |?> package => (PartialParser.positionToOffset(text, pos) |> orError("invalid offset")) |?>> offset => {
        open Rpc.J;
        let completions = switch (PartialParser.findCompletable(text, offset)) {
        | Nothing => {
          Log.log("Nothing completable found :/");
          []
        }
        | Labeled(string) => {
          Log.log("don't yet support completion for argument labels, but I hope to soon!");
          []
        }
        | Lident(string) => {
          log("Completing for string " ++ string);
          let parts = Str.split(Str.regexp_string("."), string);
          let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
          let currentModuleName = String.capitalize(Filename.chop_extension(Filename.basename(uri)));
          let opens = PartialParser.findOpens(text, offset);
          /* */
          let localData = State.getLastDefinitions(uri, state);
          let useMarkdown = !state.settings.clientNeedsPlainText;
          Completions.get(~currentPath=Infix.(Utils.parseUri(uri) |? "current file"), currentModuleName, opens, parts, state, localData, pos, ~package) |> List.map(({Completions.kind, path, label, detail, documentation}) => o([
            ("label", s(label)),
            ("kind", i(Completions.kindToInt(kind))),
            ("detail", Infix.(detail |?>> s |? null)),
            ("documentation", Infix.((documentation |?>> d => d ++ "\n\n" ++ fold(path, "", path => (useMarkdown ? "*" : "") ++ (
              Utils.startsWith(path, state.rootPath ++ "/") ? Utils.sliceToEnd(path, String.length(state.rootPath ++ "/")) : path
              ) ++ (useMarkdown ? "*" : ""))) |?>> Protocol.contentKind(useMarkdown) |? null)),
            ("data", switch kind {
              | RootModule(cmt, src) => o([("cmt", s(cmt)), ("name", s(label)), ...(fold(src, [], src => [("src", s(src))]))])
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
        let (detail, docs) = Completions.getModuleResults(name, state, cmt, Some(src));

        open Rpc.J;
        extend(params, [
          ("detail", detail |?>> s |? null),
          ("documentation", docs |?>> Protocol.contentKind(!state.settings.clientNeedsPlainText) |? null),
        ]) |? params
      }) |? params;
      Ok((state, result))
    }
  }),

  ("textDocument/documentHighlight", (state, params) => {
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);

    let res = {
      let pos = Utils.cmtLocFromVscode(pos);
      let%opt (file, extra) = State.fileForUri(state, ~package, uri);

      let%opt_wrap refs = References.forPos(~file, ~extra, pos);
      open Rpc.J;
      (state, l(refs |> List.map((loc) => o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("kind", i(2))
      ]))));
    } |? (state, Json.Null);

    Ok(res)
  }),

  ("textDocument/references", (state, params) => {
    open InfixResult;
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);
    let%try_wrap (file, extra) = State.fileForUri(state, ~package, uri) |> Result.orError("Could not compile " ++ uri);

    open Infix;

    {
      let%opt (_, loc) = References.locForPos(~extra, Utils.cmtLocFromVscode(pos));
      let allModules = package.localModules |> List.map(fst);
      let%opt allReferences = References.forLoc(
        ~file,
        ~extra,
        ~allModules,
        ~getModule=State.fileForModule(state, ~package),
        ~getExtra=State.extraForModule(state, ~package),
        loc
      ) |> Result.toOptionAndLog;

      open Rpc.J;
      Some((
        state,
        l(allReferences |> List.map(
          ((fname, references)) => (
            fname == uri
            ? List.filter(((loc)) => !Protocol.locationContains(loc, pos), references)
            : references
          ) |> List.map(((loc)) => Protocol.locationOfLoc(~fname, loc))
        ) |> List.concat)
      ));

    } |? (state, Json.Null);
  }),

  ("textDocument/rename", (state, params) => {
    open InfixResult;
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);
    let%try (file, extra) = State.fileForUri(state, ~package, uri) |> Result.orError("Could not compile " ++ uri);
    let%try newName = RJson.get("newName", params);

    open Infix;
    {
      let%opt (_, loc) = References.locForPos(~extra, Utils.cmtLocFromVscode(pos));
      let allModules = package.localModules |> List.map(fst);
      let%opt allReferences = References.forLoc(
        ~file,
        ~extra,
        ~allModules,
        ~getModule=State.fileForModule(state, ~package),
        ~getExtra=State.extraForModule(state, ~package),
        loc
      ) |> Result.toOptionAndLog;

      open Rpc.J;
      Some(Ok((
        state,
        o([
          ("changes", o(
            allReferences |> List.map(((fname, references)) => 

              (fname,
                l(references |> List.map(((loc)) => o([
                  ("range", Protocol.rangeOfLoc(loc)),
                  ("newText", newName),
                ])))
              )

            )
          ))
        ])
      )));
    } |? Ok((state, Json.Null)) 
  }),

  ("textDocument/codeLens", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    /* let%try package = State.getPackage(uri, state); */
    switch (State.getPackage(uri, state)) {
      | Result.Error(message) => {
        let items = [("Unable to load compilation data: " ++ message, {
          Location.loc_start: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
          Location.loc_end: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
          loc_ghost: false,
        })];
        open Rpc.J;
        Ok((state, l(items |> List.map(((text, loc)) => o([
          ("range", Protocol.rangeOfLoc(loc)),
          ("command", o([
            ("title", s(text)),
            ("command", s(""))
          ]))
        ])))))
      }
      | Ok(package) =>
      open Infix;
    let items = {
      let%opt moduleData = {
        let%opt (_, moduleData) = State.getCompilationResult(uri, state, ~package) |> AsYouType.getResult;
        Some(moduleData)
      } |?# lazy(State.getLastDefinitions(uri, state));

      let showToplevelTypes = state.settings.perValueCodelens; /* TODO config option */
      let lenses = showToplevelTypes ? moduleData.Definition.topLevel |> Utils.filterMap(({Docs.T.name, loc, kind, docstring}) => {
        switch kind {
        | Docs.T.Value(t) => PrintType.default.expr(PrintType.default, t) |> PrintType.prettyString |> s => Some((s, loc))
        | _ => None
        }
      }) : [];

      let showOpens = state.settings.opensCodelens;
      let lenses = showOpens ? lenses @ Definition.opens(moduleData) : lenses;

      let showDependencies = state.settings.dependenciesCodelens;
      let lenses = showDependencies ? [("Dependencies: " ++ String.concat(", ", Definition.dependencyList(moduleData)), {
        Location.loc_start: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
        Location.loc_end: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
        loc_ghost: false,
      }), ...lenses] : lenses;

      Some(lenses)
    };
    switch items {
    | None => Error("Could not get compilation data")
    | Some(items) =>
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
  }),

  ("textDocument/hover", (state, params) => {
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);
    let%try (file, extra) = State.fileForUri(state, ~package, uri) |> Result.orError("Could not compile " ++ uri);

    {
      let pos = Utils.cmtLocFromVscode(pos);
      let%opt (location, loc) = References.locForPos(~extra, pos);
      let%opt text = Hover.newHover(
        ~file,
        ~extra,
        ~getModule=State.fileForModule(state, ~package),
        ~markdown=!state.settings.clientNeedsPlainText,
        loc
      );

      open Rpc.J;
      Some(Ok((state, o([
        ("range", Protocol.rangeOfLoc(location)), 
        ("contents", text |> Protocol.contentKind(!state.settings.clientNeedsPlainText))
      ]))))
    } |? Ok((state, Json.Null));
  }),

  ("textDocument/rangeFormatting", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string
    |?> uri => State.getPackage(uri, state)
    |?> package => RJson.get("range", params) |?> Protocol.rgetRange
    |?> ((start, end_)) => {
      let text = State.getContents(uri, state);
      open Infix;
      (PartialParser.positionToOffset(text, start)
      |?> startPos => PartialParser.positionToOffset(text, end_)
      |?>> endPos => {
        let substring = String.sub(text, startPos, endPos - startPos);
        open InfixResult;
        AsYouType.format(~formatWidth=state.settings.formatWidth, substring, package.refmtPath) |?>> text => {
          open Rpc.J;
          (state, l([o([
            ("range", Infix.(|!)(Json.get("range", params), "what")),
            ("newText", s(text))
          ])]))
        }
      }) |? Error("Invalid position")
    }
  }),

  ("textDocument/documentSymbol", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    let%try package = State.getPackage(uri, state);

    let%try (file, extra) = State.fileForUri(state, ~package, uri) |> Result.orError("Could not compile " ++ uri);

    open SharedTypes;

    let rec getItems = ({Module.topLevel}) => {
      let fn = ({name: {txt, loc}, extentLoc, contents}) => {
        let (item, siblings) = switch contents {
          | Module.Value(v) => (Protocol.variableKind(v.typ), [])
          | Type(t) => (Protocol.typeKind(t.typ), [])
          | Module(Structure(contents)) => (`Module, getItems(contents))
          | Module(Ident(_)) => (`Module, [])
          | ModuleType(_) => (`ModuleType, [])
        };
        [(txt, extentLoc, item), ...siblings]
      };
      let x = topLevel |. Belt.List.map(fn) |. List.concat;
      x
    };

    (getItems(file.contents) |> items => {
      open Rpc.J;
      Ok((state, l(items |> List.map(((name, loc, typ)) => o([
        ("name", s(name)),
        ("kind", i(Protocol.symbolKind(typ))),
        ("location", Protocol.locationOfLoc(loc)),
        /* ("containerName", s(String.concat(".", path))) */
      ])))))
    })

    /* open Infix;
    let items = State.getCompilationResult(uri, state, ~package) |> AsYouType.getResult |?>> snd
    |?# lazy(State.getLastDefinitions(uri, state))
    |?>> ((({Definition.topLevel})) => {
      let rec getItems = (path, item) => {
        let res = switch (item.Docs.T.kind) {
          | Docs.T.Module(contents) => {
            let children = contents |> List.map(getItems(path @ [item.name])) |> List.concat;
            Some((`Module, children))
          }
          | Type(t, _extra) => Some((Protocol.typeKind(t), []))
          | Value(v) => Some((Protocol.variableKind(v), []))
          | _ => None
        };
        res |?>> ((((typ, children))) => [(item.name, typ, item.loc, path), ...children]) |? []
      };
      topLevel |> List.map(getItems([])) |> List.concat;
    }); */

    /* (items |?>> items => {
      open Rpc.J;
      Ok((state, l(items |> List.map(((name, typ, loc, path)) => o([
        ("name", s(name)),
        ("kind", i(Protocol.symbolKind(typ))),
        ("location", Protocol.locationOfLoc(loc)),
        ("containerName", s(String.concat(".", path)))
      ]))))) */

    /* }) |? Ok((state, Json.Null)) */

  }),

  ("textDocument/formatting", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string
    |?> uri => State.getPackage(uri, state)
    |?> package => {
      let text = State.getContents(uri, state);
      AsYouType.format(~formatWidth=state.settings.formatWidth, text, package.refmtPath) |?>> newText => {
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

