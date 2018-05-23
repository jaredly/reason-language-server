
open Result;

type messageSeverity =
  | Error
  | Warning
  | Info
  | Log;

let severity = m =>
  switch m {
  | Error => 1
  | Warning => 2
  | Info => 3
  | Log => 4
  };

let showMessage = (log, typ, message) =>
  Rpc.sendNotification(
    log,
    stdout,
    "window/showMessage",
    Rpc.J.(o([("type", i(severity(typ))), ("message", s(message))]))
  );

let run = (~ignoreErrors, ~log, ~messageHandlers, ~notificationHandlers, ~getInitialState, ~capabilities) => {

  let rec loop = (state) => {
    switch (Rpc.readMessage(log, stdin)) {
    | Message(id, method, params) =>
      log("Got a method " ++ method);
      switch (List.assoc(method, messageHandlers)) {
      | exception Not_found => {
        /* if (ignoreErrors) {
          Rpc.sendMessage(log, stdout, id, Json.String("Ok folks"))
        } else { */
          Rpc.sendError(log, stdout, id, Rpc.J.(o([
            ("code", i(-32601)), /* MethodNotFoundError */
            ("message", s("Unexpected method: " ++ method))
          ])));
        /* }; */
        loop(state)
      }
      | handler => switch (handler(state, params)) {
      | Ok((state, result)) => {
        Rpc.sendMessage(log, stdout, id, result);
        loop(state)
      }
      | Error(string) => {
        /* if (ignoreErrors) {
          Rpc.sendMessage(log, stdout, id, Json.String("Ok folks"))
        } else { */
          Rpc.sendError(log, stdout, id, Rpc.J.(o([
            ("code", i(-32603)), /* InternalError */
            ("message", s(string))
          ])));
        /* }; */
        loop(state)
      }
      | exception e => {
          Rpc.sendError(log, stdout, id, Rpc.J.(o([
            ("code", i(-32603)), /* InternalError */
            ("message", s(Printexc.to_string(e) ++ Printexc.get_backtrace()))
          ])));
        /* }; */
        loop(state)

      }
      }
      }
    | Notification("exit", _) => log("Got exit! Terminating loop")
    | Notification(method, params) =>
      switch (List.assoc(method, notificationHandlers)) {
      | exception Not_found => loop(state)
      | handler => switch (handler(state, params)) {
      | Ok(state) => loop(state)
      | Error(string) => {showMessage(log, Error, string); loop(state)}
      | exception e => {
          showMessage(log, Error, Printexc.to_string(e) ++ Printexc.get_backtrace());
        /* }; */
        loop(state)

      }
      }
      }
    }
  };

  let initialize = () => {
    switch (Rpc.readMessage(log, stdin)) {
    | Message(id, "initialize", params) => {
      switch (getInitialState(params)) {
      | Ok(state) => {
        Rpc.sendMessage(log, stdout, id, Json.Object([("capabilities", capabilities(params))]));
        loop(state);
      }
      | Error(string) => Rpc.sendError(log, stdout, id, Json.String(string))
      | exception e => {
          Rpc.sendError(log, stdout, id, Rpc.J.(o([
            ("code", i(-32603)), /* InternalError */
            ("message", s(Printexc.to_string(e) ++ Printexc.get_backtrace()))
          ])));
      }
      }
    }
    | _ => failwith("Client must send 'initialize' as first event")
    };
  };

  initialize();
};
