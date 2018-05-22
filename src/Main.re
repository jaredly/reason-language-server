
open Infix;

let out = open_out("/Users/jared/ls.log");

let log = msg => {
  output_string(stderr, msg ++ "\n");
  output_string(out, msg ++ "\n");
  flush(out);
};

let loop = () => {
  let going = ref(true);
  while (going^) {
    switch (Rpc.readMessage(log, stdin)) {
    | Notification(method, params) => {
      log("Got a notification " ++ method);
      flush(out);
      switch (method) {
      | "initialized" => ()
      | "exit" => going := false
      | _ => ()
      }
    }
    | Message(id, method, params) =>
      log("Got a method " ++ method);
      flush(out);
      let reply = Rpc.sendMessage(log, stdout, id);

      switch (method) {
      | "initialize" => {
        open Rpc.J;
        reply(o([
          ("capabilities", o([
            ("textDocumentSync", i(1)),
            ("hoverProvider", t),
            ("completionProvider", t),
            ("signatureHelpProvider", t),
            ("definitionProvider", t),
            ("typeDefinitionProvider", t),
            ("referencesProvider", t),
            ("documentSymbolProvider", t),
            /* ("codeActionProvider", t), */
            ("codeLensProvider", t),
            ("documentFormattingProvider", t),
            ("renameProvider", t),
            /* ("executeCommandOptions", t), */
            /* ("") */
          ]))
        ]))
      }
      | "textDocument/completion" => {
        /* textDocument {uri}, position {line, character} context {triggerKind} */
        reply(Json.Null)
      }
      | "textDocument/didChange"
      | _ => reply(Json.String("Ok folks"))
      }
    }
  };
};

let main = () => {
  log("Booting up");
  loop();
  log("Finished");
  close_out(out);
};
