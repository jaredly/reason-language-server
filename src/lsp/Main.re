
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
  Log.log("Hello from " ++ Sys.executable_name);
  Log.log("Previous log location: " ++ Log.initial_dest);

  Rpc.sendNotification(
    Log.log,
    stdout,
    "client/registerCapability",
    Rpc.J.(
      o([
        (
          "registrations",
          l([
            o([
              ("id", s("watching")),
              ("method", s("workspace/didChangeWatchedFiles")),
              (
                "registerOptions",
                o([
                  (
                    "watchers",
                    l([
                      o([
                        ("globPattern", s("**/bsconfig.json")),
                        ("globPattern", s("**/.merlin")),
                      ]),
                    ]),
                  ),
                ]),
              ),
            ]),
          ]),
        ),
      ])
    ),
  );

  open InfixResult;

  let packagesByRoot = Hashtbl.create(1);

  /* let package = {
    let%try_consume package = State.newBsPackage(rootPath);
    let package = {...package, refmtPath: state.settings.refmtLocation |? package.refmtPath};
    Hashtbl.replace(packagesByRoot, rootPath, package)
  }; */

  /* if client needs plain text in any place, we disable markdown everywhere */
  let clientNeedsPlainText = ! Infix.(
      Json.getPath("capabilities.textDocument.hover.contentFormat", params) |?> Protocol.hasMarkdownCap |? true
      && Json.getPath("capabilities.textDocument.completion.completionItem.documentationFormat", params) |?> Protocol.hasMarkdownCap |? true,
  );

  let state = {
    ...TopTypes.empty(),
    rootPath,
    rootUri: uri,
  };

  Ok({...state, settings: {...state.settings, clientNeedsPlainText}})
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
  let%try_consume result = State.getCompilationResult(uri, state, ~package);

  let makeDiagnostic = (((line, c1, c2), message)) => {
    open Rpc.J;
          let text = String.concat("\n", message);
          o([
            ("range", Protocol.rangeOfInts(line, c1, line, c2)),
            ("message", s(text)),
            ("severity", i(Utils.startsWith(text, "Warning") ? 2 : 1))
          ])
  };

  open Rpc.J;
  Rpc.sendNotification(log, stdout, "textDocument/publishDiagnostics", o([
    ("uri", s(uri)),
    ("diagnostics", switch result {
    | AsYouType.SyntaxError(text, otherText, _) => {
      let errors = AsYouType.parseErrors(Utils.splitLines(Utils.stripAnsii(otherText)));
      let errors = errors |. Belt.List.keep(((loc, message)) => message != ["Error: Uninterpreted extension 'merlin.syntax-error'."]);
      let errors = AsYouType.parseErrors(Utils.splitLines(Utils.stripAnsii(text))) @ errors;
      l(errors |. Belt.List.map(makeDiagnostic))
    }
    | Success(text, _) => {
      if (String.trim(text) == "") {
        l([])
      } else {
        let errors = AsYouType.parseErrors(Utils.splitLines(Utils.stripAnsii(text)));
        l(errors |. Belt.List.map(makeDiagnostic))
      }
    }
    | TypeError(text, _) => {
      Log.log("type error here " ++ text);
      let errors = AsYouType.parseErrors(Utils.splitLines(Utils.stripAnsii(text)))
      |. Belt.List.keep(((loc, message)) => {
        !Str.string_match(Str.regexp({|.*Missing dependency [a-zA-Z]+ in search path|}), String.concat(" ", message), 0)
      })
      ;
      l(errors |. Belt.List.map(makeDiagnostic))
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
    let nullIfEmpty = item => item == "" ? None : Some(item);
    let settings = params |> Json.get("settings") |?> Json.get("reason_language_server");
    let refmtLocation = (settings |?> Json.get("refmt") |?> Json.string) |?> nullIfEmpty;
    let lispRefmtLocation = (settings |?> Json.get("lispRefmt") |?> Json.string |?> nullIfEmpty);
    let perValueCodelens = (settings |?> Json.get("per_value_codelens") |?> Json.bool) |? false;
    let opensCodelens = (settings |?> Json.get("opens_codelens") |?> Json.bool) |? true;
    let dependenciesCodelens = (settings |?> Json.get("dependencies_codelens") |?> Json.bool) |? true;
    let formatWidth = (settings |?> Json.get("format_width") |?> Json.number) |?>> int_of_float;
    /* let crossFileAsYouType = (settings |?> Json.get("cross_file_as_you_type") |?> Json.bool) |? false; */
    /* Disabling this -- too finnicky :/ */
    let crossFileAsYouType = false;
    Ok({
      ...state,
      settings: {
        ...state.settings,
        perValueCodelens,
        refmtLocation,
        lispRefmtLocation,
        opensCodelens,
        formatWidth,
        dependenciesCodelens,
        crossFileAsYouType,
      },
    });
  }),
  ("textDocument/didSave", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> doc => RJson.get("uri", doc) |?> RJson.string;
    let%try package = State.getPackage(uri, state);
    let moduleName = FindFiles.getName(uri);

    package.localModules |. Belt.List.forEach(((mname, (cmt, src))) => {
      let otherUri = Utils.toUri(src);
      open Infix;
      if (mname != moduleName
          && List.mem(
                moduleName,
                Query.hashFind(package.interModuleDependencies, mname) |? [],
              )) {
        Hashtbl.remove(state.compiledDocuments, otherUri);
        Hashtbl.replace(
          state.documentTimers,
          otherUri,
          Unix.gettimeofday() +. 0.01,
        );
      };
    });

    Ok(state)
    /* failwith("A") */
  }),
  ("textDocument/didChange", (state, params) => {
    open InfixResult;
    let%try doc = params |> RJson.get("textDocument");
    let%try uri = RJson.get("uri", doc) |?> RJson.string;
    let%try version = RJson.get("version", doc) |?> RJson.number;
    let%try changes = RJson.get("contentChanges", params) |?> RJson.array;
    let%try text = List.nth(changes, List.length(changes) - 1) |> RJson.get("text") |?> RJson.string;
    /* Hmm how do I know if it's modified? */
    let state = State.updateContents(uri, text, version, state);
    Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() +. recompileDebounceTime);
    Ok(state)
  }),
  ("workspace/didChangeWatchedFiles", (state, params) => {
    Log.log("Got a watched file change");
    let%try changes = RJson.get("changes", params);
    let%try changes = RJson.array(changes);
    open InfixResult;
    let shouldReload = Belt.List.some(changes, change => {
      let%try t = RJson.get("type", change) |?> RJson.number;
      let%try uri = RJson.get("uri", change) |?> RJson.string;
      Ok(Utils.endsWith(uri, "bsconfig.json") || Utils.endsWith(uri, ".merlin"))
    } |? false);

    if (shouldReload) {
      Log.log("RELOADING ALL STATE");
      Hashtbl.iter((uri, _) =>
        Hashtbl.replace(
          state.documentTimers,
          uri,
          Unix.gettimeofday() +. recompileDebounceTime,
        ), state.documentText
      );
      Ok({
        ...TopTypes.empty(),
        documentText: state.documentText,
        documentTimers: state.documentTimers,
        settings: state.settings,
      })
    } else {
      Ok(state)
    }
  })
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