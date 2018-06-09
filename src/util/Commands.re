

let shellEscape = path => {
  if (Sys.os_type == "Win32") {
    /* TODO allow spaces in path...  */
    path
  } else {
    Printf.sprintf("%S", path)
  }
};

let execFull = (~input=?, ~env=Unix.environment(), cmd) => {
  let (cmd_out, cmd_in, cmd_err) = Unix.open_process_full(cmd, env);

  switch input {
  | None => ()
  | Some(text) => output_string(cmd_in, text)
  };
  close_out(cmd_in);

  let cmd_out_descr = Unix.descr_of_in_channel(cmd_out);
  let cmd_err_descr = Unix.descr_of_in_channel(cmd_err);
  let rec loop = ((out, err, opens)) => {
    if (opens == []) {
      (out, err)
    } else {
      let (can_read, _, _) = Unix.select(opens, [], [], 1.0);
      List.fold_left(
        ((out, err, opens), fh) =>
          try (
            if (fh == cmd_err_descr) {
              (out, [input_line(cmd_err), ...err], opens);
            } else {
              ([input_line(cmd_out), ...out], err, opens);
            }
          ) {
          | End_of_file => (out, err, List.filter((fh') => fh != fh', opens))
          },
        (out, err, opens),
        can_read
      ) |> loop
    };
  };
  let (out, err) = loop(([], [], [cmd_out_descr, cmd_err_descr]));
  let out = List.rev(out);
  let err = List.rev(err);
  switch(Unix.close_process_full((cmd_out, cmd_in, cmd_err))) {
    | WEXITED(0) => (out, err, true)
    | WEXITED(_)
    | WSIGNALED(_)
    | WSTOPPED(_) => (out, err, false)
  }
};

/**
 * Get the output of a command, in lines.
 */
let execSync = (cmd) => {
  let cin = Unix.open_process_in(cmd);
  try {
    let rec loop = () =>
      switch (Pervasives.input_line(cin)) {
      | exception End_of_file => []
      | line => {
        [line, ...loop()]
      }
      };
    let lines = loop();
    switch (Unix.close_process_in(cin)) {
    | WEXITED(0) => (lines, true)
    | WEXITED(_)
    | WSIGNALED(_)
    | WSTOPPED(_) => (lines, false)
    }
  } {
  | End_of_file => ([], false)
  }
};

/**
 * Get the output of a command, in lines.
 */
let execWithInput = (cmd, input) => {
  let (stdout, stdin) = Unix.open_process(cmd);
  output_string(stdin, input);
  close_out(stdin);
  try {
    let rec loop = () =>
      switch (Pervasives.input_line(stdout)) {
      | exception End_of_file => []
      | line => {
        [line, ...loop()]
      }
      };
    let lines = loop();
    switch (Unix.close_process((stdout, stdin))) {
    | WEXITED(0) => (lines, true)
    | WEXITED(_)
    | WSIGNALED(_)
    | WSTOPPED(_) => (lines, false)
    }
  } {
  | End_of_file => ([], false)
  }
};