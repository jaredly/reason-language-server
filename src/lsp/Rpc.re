
module J = {
  open Json;
  let o = o => Object(o);
  let s = s => String(s);
  let n = n => Number(n);
  let i = i => Number(float_of_int(i));
  let l = l => Array(l);
  let t = True;
  let f = False;
  let null = Null;
};

open Infix;

type jsonrpc = Message(Json.t, string, Json.t) | Notification(string, Json.t);

let readMessage = (log, input) => {
  let clength = input_line(input);
  let cl = "Content-Length: ";
  let cll = String.length(cl);
  /* output_string(stderr, "First line here: " ++ String.escaped(clength) ++ "\r\n"); */
  log("Got the first line " ++ clength);
  if (String.sub(clength, 0, cll) == cl) {
    /* if on windows, dont need the extra -1 */
    let offset = Sys.os_type == "Win32" ? 0 : -1; /* -1 for trailing \r */
    let num = String.sub(clength, cll, String.length(clength) - cll + offset);
    /* log("Num bytes to read: " ++ String.escaped(num)); */
    let num = (num |> int_of_string) + (Sys.os_type == "Win32" ? 1 : 2);
    let buffer = Buffer.create(num);
    Buffer.add_channel(buffer, input, num);
    let raw = Buffer.contents(buffer);
    log("Read message " ++ raw);
    let json = try (Json.parse(raw)) {
    | Failure(message) => failwith("Unable to parse message " ++ raw ++ " as json: " ++ message)
    | err => failwith("Other failure " ++ raw ++ " message " ++ Printexc.to_string(err))
    };
    let id = Json.get("id", json);
    let method = Json.get("method", json) |?> Json.string |! "method required";
    let params = Json.get("params", json) |! "params required";
    switch id {
    | None => Notification(method, params)
    | Some(id) => Message(id, method, params)
    }
  } else {
    failwith("Invalid header")
  }
};

let send = (output, content) => {
  let length = String.length(content);
  let sep = Sys.os_type == "Unix" ? "\r\n\r\n" : "\n\n";
  output_string(output, "Content-Length: " ++ string_of_int(length) ++ sep ++ content);
  flush(output);
};

let sendMessage = (log, output, id, result) => {
  open Json;
  open J;
  let content = Json.stringify(o([
    ("id", id),
    ("jsonrpc", s("2.0")),
    ("result", result)]));
  log("Sending response " ++ content);
  send(output, content);
};

let sendError = (log, output, id, error) => {
  open Json;
  open J;
  let content = Json.stringify(o([
    ("id", id),
    ("jsonrpc", s("2.0")),
    ("error", error)]));
  log("Sending response " ++ content);
  send(output, content);
};

let sendNotification = (log, output, method, params) => {
  open J;
  let content = Json.stringify(o([
    ("jsonrpc", s("2.0")),
    ("method", s(method)),
    ("params", params)
  ]));
  log("Sending notification " ++ content);
  send(output, content);
};
