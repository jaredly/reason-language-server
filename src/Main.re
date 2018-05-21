
open Infix;

module J = {
  open Json;
  let o = o => Object(o);
  let s = s => String(s);
  let n = n => Number(n);
  let i = i => Number(float_of_int(i));
  let l = l => Array(l);
  let t = True;
  let f = False;
};

let out = open_out("/Users/jared/ls.log");

let log = msg => {
  output_string(stderr, msg ++ "\n");
  output_string(out, msg ++ "\n");
  flush(out);
};

let readMessage = input => {
  let clength = input_line(input);
  let cl = "Content-Length: ";
  let cll = String.length(cl);
  if (String.sub(clength, 0, cll) == cl) {
    let num = String.sub(clength, cll, String.length(clength) - cll - 1); /* -1 for trailing \r */
    output_string(stderr, String.escaped(num) ++ "\n");
    let num = (num |> int_of_string) + 2;
    let buffer = Buffer.create(num);
    Buffer.add_channel(buffer, input, num);
    let raw = Buffer.contents(buffer);
    log("Read message " ++ raw);
    let json = try (Json.parse(raw)) {
    | Failure(message) => failwith("Unable to parse message " ++ raw ++ " as json: " ++ message)
    };
    let id = Json.get("id", json) |?> Json.number |! "id required";
    let method = Json.get("method", json) |?> Json.string |! "method required";
    let params = Json.get("params", json) |! "params required";
    (id, method, params)
  } else {
    failwith("Invalid header")
  }
};

let sendMessage = (output, id, result) => {
  open Json;
  let content = Json.stringify(Object([("id", Number(id)), ("result", result)]));
  let length = String.length(content);
  log("Sending response " ++ content);
  output_string(output, "Content-Length: " ++ string_of_int(length) ++ "\r\n\r\n" ++ content);
};

let loop = () => {
  while (true) {
    let (id, method, params) = readMessage(stdin);
    log("Got a method " ++ method);
    output_string(out, "Got method: " ++ method);
    flush(out);
    switch (method) {
    | "initialize" => {
      open J;
      sendMessage(stdout, id, o([
        ("capabilities", o([
          ("textDocumentSync", i(1)),
          ("hoverProvider", t),
          ("completionProvider", t),
          ("signatureHelpProvider", t),
          ("definitionProvider", t),
          ("typeDefinitionProvider", t),
          ("referencesProvider", t),
          ("documentSymbolProvider", t),
          ("codeActionProvider", t),
          ("codeLensProvider", t),
          ("documentFormattingProvider", t),
          ("renameProvider", t),
          /* ("executeCommandOptions", t), */
          /* ("") */
        ]))
      ]))
    }
    | _ => sendMessage(stdout, id, Json.String("Ok folks"))
    }
  };
};

let main = () => {
  log("Booting up");
  loop();
  log("Finished");
  close_out(out);
};
