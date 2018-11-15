
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
  Log.log("Hello from " ++ Sys.executable_name);
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

  open InfixResult;

  let packagesByRoot = Hashtbl.create(1);

  /* if client needs plain text in any place, we disable markdown everywhere */
  let clientNeedsPlainText = ! Infix.(
      Json.getPath("capabilities.textDocument.hover.contentFormat", params) |?> Protocol.hasMarkdownCap |? true
      && Json.getPath("capabilities.textDocument.completion.completionItem.documentationFormat", params) |?> Protocol.hasMarkdownCap |? true,
  );

  /* Check if we have `cur__target_dir` as a marker that we're inside an Esy context,
   * i.e. the editor was started with e.g. `esy @myalias code .`.
   * We can't support auto rebuild in this case yet because Esy doesn't provide
   * enough information on which named sandbox we're in.
   */
  let state =
    switch (Utils.getEnvVar("cur__target_dir")) {
    | Some(_) =>
      let empty = TopTypes.empty();
      {
        ...empty,
        settings: {
          ...empty.settings,
          autoRebuild: false
        },
        rootPath,
        rootUri: uri
      }
    | None => {
      ...TopTypes.empty(),
      rootPath,
      rootUri: uri
    }
    };

  Ok({...state, settings: {...state.settings, clientNeedsPlainText}})
};

open TopTypes;

let tick = state => {
  NotificationHandlers.checkPackageTimers(state);
  Diagnostics.checkDocumentTimers(state);
};

let main = () => {
  switch (Sys.argv) {
    | [|_|] =>
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
    | [|_, "-h" | "--help"|] | _ =>
      print_endline({|
## Reason Language Server ##

Usage: run without arguments, and communicate over stdin/stdout,
following the language server protocol as defined in
https://microsoft.github.io/language-server-protocol/specification

Logs are stored in `<project_root>/node_modules/.lsp/debug.log`.
      |})
  }
};