open Infix;

switch (Array.to_list(Sys.argv)) {
  | [] | [_] => print_endline("Fix")
  | [_, directory, ..._] => Files.readDirectory(directory)
  |> List.filter(name => Filename.check_suffix(name, ".txt"))
  |> List.iter(name => {
    let full = Filename.concat(directory, name);
    let lines = Files.readFileExn(full) |> Str.split(Str.regexp_string("\n"));
    switch lines {
      | [] => assert(false)
      | [init, ...rest] => {
          let state = Main.getInitialState(Json.parse(init));
          switch state {
            | Result.Error(_) => ()
            | Ok(state) => {

            let rec loop = (state, messages) => {
              switch messages {
                | [] => ()
                | [line, ...rest] => {
                  switch (Rpc.messageFromJson(Json.parse(line))){
                    | Message(id, method, params) =>
                      loop(BasicServer.handleMessage(Log.log, MessageHandlers.handlers, id, method, params, state), rest)
                    | Notification("exit", _) => ()
                    | Notification(method, params) => loop(BasicServer.handleNotification(Log.log, Main.notificationHandlers, method, params, state), rest)
                  }
                }
              }
            };
            loop(state, rest)
            }
          }
        }
    }
  })
}
