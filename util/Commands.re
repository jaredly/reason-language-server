

let shellEscape = path => Filename.quote(path);

let execFull = (~input=?, ~pwd=?, ~env=Unix.environment(), cmd) => {
  let cmd =
    if (Sys.os_type == "Win32") {
      Printf.sprintf("\"%s\"", cmd)
    } else {
      cmd
    }
  let env = switch pwd {
    | None => env
    | Some(pwd) => Array.map(item => String.length(item) > 4 && String.sub(item, 0, 4) == "PWD=" ? "PWD=" ++ pwd : item, env)
  };
  let prevCwd = switch pwd {
    | None => None
    | Some(pwd) =>
    let prevCwd = Unix.getcwd();
    if (prevCwd == pwd) {
      None
    } else {
      Unix.chdir(pwd);
      Some(prevCwd)
    }
  }
  let (cmd_out, cmd_in, cmd_err) = Unix.open_process_full(cmd, env);
  switch prevCwd {
    | None => ()
    | Some(prevCwd) => Unix.chdir(prevCwd)
  };

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
      switch (input_line(cin)) {
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

let execOption = cmd => {
  let (lines, success) = execSync(cmd);
  if (success) {
    Some(String.concat("\n", lines))
  } else {
    None
  }
};

let execResult = cmd => {
  let (lines, success) = execSync(cmd);
  if (success) {
    RResult.Ok(String.concat("\n", lines))
  } else {
    RResult.Error(String.concat("\n", lines))
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
      switch (input_line(stdout)) {
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
