
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
  let%try rootPath = uri |> Utils.parseUri |> resultOfOption("No root uri");

  Files.mkdirp(rootPath /+ "node_modules" /+ ".lsp");
  Log.setLocation(rootPath /+ "node_modules" /+ ".lsp" /+ "debug.log");

  open InfixResult;

  let packagesByRoot = Hashtbl.create(1);

  let package = {
    let%try_consume package = State.newBsPackage(rootPath);
    Hashtbl.replace(packagesByRoot, rootPath, package)
  };

  /* if client needs plain text in any place, we disable markdown everywhere */
  let clientNeedsPlainText = ! Infix.(
      Json.getPath("capabilities.textDocument.hover.contentFormat", params) |?> Protocol.hasMarkdownCap |? true
      && Json.getPath("capabilities.textDocument.completion.completionItem.documentationFormat", params) |?> Protocol.hasMarkdownCap |? true,
  );

  let state = TopTypes.{
    rootPath: rootPath,
    rootUri: uri,
    documentText: Hashtbl.create(5),
    documentTimers: Hashtbl.create(10),
    packagesByRoot,
    rootForUri: Hashtbl.create(30),
    cmtCache: Hashtbl.create(30),
    cmiCache: Hashtbl.create(30),
    compiledDocuments: Hashtbl.create(10),
    lastDefinitions: Hashtbl.create(10),
    settings: {
      formatWidth: None,
      perValueCodelens: false,
      opensCodelens: true,
      dependenciesCodelens: true,
      clientNeedsPlainText,
    },
  };

  Ok(state)
};

open TopTypes;

let getTextDocument = doc => {
  let%opt uri = Json.get("uri", doc) |?> Json.string;
  let%opt version = Json.get("version", doc) |?> Json.number;
  let%opt text = Json.get("text", doc) |?> Json.string;
  Some((uri, version, text))
};

let runDiagnostics = (uri, state, ~package) => {
  Log.log("Running diagnostics for " ++ uri);
  let result = State.getCompilationResult(uri, state, ~package);
  open Rpc.J;
  Rpc.sendNotification(log, stdout, "textDocument/publishDiagnostics", o([
    ("uri", s(uri)),
    ("diagnostics", switch result {
    /* | AsYouType.ParseError(text) => {
      let pos = AsYouType.parseTypeError(text);
      let (l0, c0, l1, c1, text) = switch pos {
      | None => (0, 0, 0, 0, text)
      | Some((line, c0, c1, text)) => (line, c0, line, c1, text)
      };
      l([o([
        ("range", Protocol.rangeOfInts(l0, c0, l1, c1)),
        ("message", s("Parse error:\n" ++ text)),
        ("severity", i(1)),
      ])])
    } */
    | AsYouType.Success(lines, _, _) => {
      if (lines == [] || lines == [""]) {
        l([])
      } else {
        let rec loop = lines => switch lines {
        | [loc, warning, ...rest] => switch (AsYouType.parseTypeError(Utils.stripAnsii(loc))) {
          | None => loop([warning, ...rest])
          | Some((line, c0, c1, text)) => {
            [o([
              ("range", Protocol.rangeOfInts(line, c0, line, c1)),
              ("message", s(Utils.stripAnsii(warning))),
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
      let plain = Utils.stripAnsii(text);
      let pos = AsYouType.parseTypeError(plain);
      let (l0, c0, l1, c1, plain) = switch pos {
      | None => (0, 0, 0, 0, plain)
      | Some((line, c0, c1, plain)) => (line, c0, line, c1, plain)
      };
      /* This is to catch the recovering parser's stuff */
      let message = List.length(Str.split(Str.regexp_string("merlin"), plain)) == 2
      ? "Syntax error"
      : plain;
      l([o([
        ("range", o([
          ("start", Protocol.pos(~line=l0, ~character=c0)),
          ("end", Protocol.pos(~line=l1, ~character=c1)),
        ])),
        ("message", s(message)),
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
      switch (State.getPackage(uri, state)) {
        | Ok(package) => runDiagnostics(uri, state, ~package);
        | Error(_) => () /* ignore... TODO should I do something */
      };
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
  ("workspace/didChangeConfiguration", (state, params) => {
    let settings = params |> Json.get("settings") |?> Json.get("reason_language_server");
    let perValueCodelens = (settings |?> Json.get("per_value_codelens") |?> Json.bool) |? false;
    let opensCodelens = (settings |?> Json.get("opens_codelens") |?> Json.bool) |? true;
    let dependenciesCodelens = (settings |?> Json.get("dependencies_codelens") |?> Json.bool) |? true;
    let formatWidth = (settings |?> Json.get("format_width") |?> Json.number) |?>> int_of_float;
    Ok({...state, settings: {...state.settings, perValueCodelens, opensCodelens, formatWidth, dependenciesCodelens}})
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

let mmm = () => {
  module Let_syntax = Monads.Option;
  let%opt value = Some(10);
  let%opt_wrap first = Some(10);
  first + value
};

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
