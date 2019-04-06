
open Infix;
open RResult;
open Log;

let capabilities =
  Util.JsonShort.(
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
      /*
       * Found how to do the showReferences thing
       * https://github.com/Microsoft/vscode/blob/c6b1114292288e76e2901e05e860faf3a08b4b5a/extensions/typescript-language-features/src/features/implementationsCodeLensProvider.ts
       * but it seems I need to instantiate the object from javascript
       */
      ("codeActionProvider", t),
      ("executeCommandProvider", o([
        ("commands", l([
          s("reason-language-server.add_to_interface_inner")
        ]))
      ])),

      ("codeLensProvider", o([
        ("resolveProvider", t)
      ])),
      ("documentHighlightProvider", t),
      ("documentRangeFormattingProvider", t),
      ("documentFormattingProvider", t),
      ("documentFormattingProvider", t),
      ("renameProvider", t)
    ])
);

let getInitialState = (params) => {
  let uri = Json.get("rootUri", params) |?> Json.string |! "Must have a rootUri";
  let%try rootPath = uri |> Utils.parseUri |> resultOfOption("No root uri");

  Files.mkdirp(rootPath /+ "node_modules" /+ ".lsp");
  Log.setLocation(rootPath /+ "node_modules" /+ ".lsp" /+ "debug.log");
  Log.log("Hello - from " ++ Sys.executable_name);
  Log.log("Previous log location: " ++ Log.initial_dest);

  Rpc.sendNotification(
    Log.log,
    stdout,
    "client/registerCapability",
    Util.JsonShort.(
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

  /* if client needs plain text in any place, we disable markdown everywhere */
  let clientNeedsPlainText = ! Infix.(
      Json.getPath("capabilities.textDocument.hover.contentFormat", params) |?> Protocol.hasMarkdownCap |? true
      && Json.getPath("capabilities.textDocument.completion.completionItem.documentationFormat", params) |?> Protocol.hasMarkdownCap |? true,
  );

  /* Check the editor was started with e.g. `esy @myalias code .` or `esy code.`.
   * We can't support auto rebuild in this case yet because Esy doesn't provide
   * enough information on which named sandbox we're in.
   */
  let state =
    if (BuildSystem.isRunningInEsyNamedSandbox()) {
      let empty = TopTypes.empty();
      {
        ...empty,
        settings: {
          ...empty.settings,
          autoRebuild: false,
        },
        rootPath,
        rootUri: uri,
      };
    } else {
      {
        ...TopTypes.empty(),
        rootPath,
        rootUri: uri
      };
    };

  Ok({...state, settings: {...state.settings, clientNeedsPlainText}})
};

let tick = state => {
  NotificationHandlers.checkPackageTimers(state);
  Diagnostics.checkDocumentTimers(state);
};

let orLog = (message, v) => switch v {
  | None => print_endline(message); None
  | Some(x) => Some(x)
};

let singleDefinition = (~verbose=false, rootPath, filePath, line, col) => {
  log("# Reason Langauge Server - checking individual files to ensure they load & process correctly");
  let rootPath = rootPath == "." ? Unix.getcwd() : maybeConcat(Unix.getcwd(), rootPath);
  let state = {
    ...Analyze.TopTypes.empty(),
    rootPath,
    rootUri: Util.Utils.toUri(rootPath)
  };
  let filePath = maybeConcat(Unix.getcwd(), filePath);

  let uri = Utils.toUri(filePath);
  switch (Packages.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
    | Error(message) =>
      print_endline("  Unable to get package: " ++ uri)
      print_endline(message);
    | Ok(package) => switch (State.getCompilationResult(uri, state, ~package)) {
      | Error(message) =>
        print_endline("  Invalid compilation result: " ++ message);
      | Ok(TypeError(message, _) | SyntaxError(message, _, _)) =>
        print_endline("  Error compiling: " ++ uri);
      | Ok(Success(_message, {file, extra})) =>
        let res = {
          let%opt_consume (location, loc) = References.locForPos(~extra, (line, col - 1)) |> orLog(
            Printf.sprintf("Nothing definable found at %s:%d:%d", filePath, line, col)
          );
          let%opt_consume (fname, dlocation) = References.definitionForLoc(
            ~pathsForModule=package.pathsForModule,
            ~file=file,
            ~getUri=State.fileForUri(state, ~package),
            ~getModule=State.fileForModule(state, ~package),
            loc,
          ) |> orLog(
            Printf.sprintf("Unable to resolve a definition for %s:%d:%d",
            filePath,
            location.loc_start.pos_lnum,
            location.loc_start.pos_cnum - location.loc_start.pos_bol + 1,
            )
          );
          let%opt_consume fname = Utils.parseUri(fname);
          Printf.printf(
            "Definition for %s:%d:%d found at %s:%d:%d\n",
            filePath,
            location.loc_start.pos_lnum,
            location.loc_start.pos_cnum - location.loc_start.pos_bol + 1,
            fname,
            dlocation.loc_start.pos_lnum,
            dlocation.loc_start.pos_cnum - dlocation.loc_start.pos_bol + 1,
          );
        };
        print_endline("  Good: " ++ uri);
    };
  }
};

let check = (rootPath, files) => {
  Util.Log.spamError := true;
  log("# Reason Langauge Server - checking individual files to ensure they load & process correctly");
  let rootPath = rootPath == "." ? Unix.getcwd() : maybeConcat(Unix.getcwd(), rootPath);
  let state = {
    ...Analyze.TopTypes.empty(),
    rootPath,
    rootUri: Util.Utils.toUri(rootPath)
  };
  files->Belt.List.forEach(file => {
    let file = maybeConcat(rootPath, file);
    let uri = Utils.toUri(file);
    switch (Packages.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
      | Error(message) =>
        print_endline("  Unable to get package: " ++ uri)
        print_endline(message);
      | Ok(package) => switch (State.getCompilationResult(uri, state, ~package)) {
        | Error(message) =>
          print_endline("  Invalid compilation result: " ++ message);
        | Ok(Success(_)) =>
          print_endline("  Good: " ++ uri);
        | Ok(TypeError(message, _) | SyntaxError(message, _, _)) =>
          print_endline("  Error compiling: " ++ uri);
      };
      print_endline(Analyze.State.Show.state(state, package));
    }
  });
  log("Ok");
}

let main = () => {
  switch (Sys.argv->Belt.List.fromArray) {
    | [_] =>
      log("Booting up");
      BasicServer.run(
        ~tick,
        ~log,
        ~messageHandlers=MessageHandlers.handlers,
        ~notificationHandlers=NotificationHandlers.notificationHandlers,
        ~capabilities=_params => capabilities,
        ~getInitialState
      );
      log("Finished");
      out^ |?< close_out;
    | [_, "definition", "-v" | "--verbose", rootPath, file, line, col] =>
      let line = int_of_string(line);
      let col = int_of_string(col);
      Util.Log.spamError := true;
      References.debugReferences := true;
      MerlinFile.debug := true;
      singleDefinition(rootPath, file, line, col)
    | [_, "definition", rootPath, file, line, col] =>
      let line = int_of_string(line);
      let col = int_of_string(col);
      singleDefinition(rootPath, file, line, col)
    | [_, "check", rootPath, ...files] =>
      check(rootPath, files)
    | [_, "-h" | "--help"] | _ =>
      print_endline({|
🎉 Reason Language Server 🎉 

Usage: run without arguments, and communicate over stdin/stdout,
following the language server protocol as defined in
https://microsoft.github.io/language-server-protocol/specification

Logs are stored in `<project_root>/node_modules/.lsp/debug.log`.
      |})
  }
};
