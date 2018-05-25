
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

let handleMessage = (log, messageHandlers, id, method, params, state) => {
    log("Got a method " ++ method);
    switch (List.assoc(method, messageHandlers)) {
    | exception Not_found => {
        Rpc.sendError(log, stdout, id, Rpc.J.(o([
          ("code", i(-32601)), /* MethodNotFoundError */
          ("message", s("Unexpected method: " ++ method))
        ])));
      state
    }
    | handler => switch (handler(state, params)) {
    | Ok((state, result)) => {
      Rpc.sendMessage(log, stdout, id, result);
      state
    }
    | Error(string) => {
        Rpc.sendError(log, stdout, id, Rpc.J.(o([
          ("code", i(-32603)), /* InternalError */
          ("message", s(string))
        ])));
      state
    }
    | exception e => {
        Rpc.sendError(log, stdout, id, Rpc.J.(o([
          ("code", i(-32603)), /* InternalError */
          ("message", s(Printexc.to_string(e) ++ Printexc.get_backtrace()))
        ])));
      /* }; */
      state
    }
    }
  }
};

let handleNotification = (log, notificationHandlers, method, params, state) => {
    switch (List.assoc(method, notificationHandlers)) {
    | exception Not_found => state
    | handler => switch (handler(state, params)) {
    | Ok(state) => state
    | Error(string) => {showMessage(log, Error, string); state}
    | exception e => {
      showMessage(log, Error, Printexc.to_string(e) ++ Printexc.get_backtrace());
      state
    }
    }
    }
};

/* Will wait up to 100ms */
let canRead = (desc) => {
  let (r, w, e) = Unix.select([desc], [], [], 0.1);
  r != []
};

let run = (~tick, ~log, ~messageHandlers, ~notificationHandlers, ~getInitialState, ~capabilities) => {
  let stdin_descr = Unix.descr_of_in_channel(stdin);

  let rec loop = (state) => {
    let state = tick(state);
    if (canRead(stdin_descr)) {
      switch (Rpc.readMessage(log, stdin)) {
      | Message(id, method, params) =>
        loop(handleMessage(log, messageHandlers, id, method, params, state))
      | Notification("exit", _) => log("Got exit! Terminating loop")
      | Notification(method, params) => loop(handleNotification(log, notificationHandlers, method, params, state))
      }
    } else {
      loop(state)
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
