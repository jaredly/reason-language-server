
type result =
  /* | ParseError(string) */
  | SyntaxError(string, string, SharedTypes.full)
  | TypeError(string, SharedTypes.full)
  | Success(string, SharedTypes.full)
;
open Infix;
open RResult;


let getResult = result => switch result {
| SyntaxError(_, _, data) => data
| TypeError(_, data) => data
| Success(_, data) => data
};

let runRefmt = (~interface, ~moduleName, ~cacheLocation, text, refmt) => {
  let target = cacheLocation /+ moduleName ++ ".ast" ++ (interface ? "i" : "");
  let cmd = Printf.sprintf("%s --print binary %s--parse re > %s",
    Commands.shellEscape(refmt),
    interface ? "-i true " : "",
    Commands.shellEscape(target)
  );
  /* Log.log("refmt " ++ moduleName ++ " " ++ cmd); */
  let (out, error, success) = Commands.execFull(~input=text, cmd);
  if (success) {
    /* Log.log("Worked on the first pass"); */
    Ok((None, target))
  } else {
    let goodError = Some(out @ error);
    let cmd = Printf.sprintf("%s --print binary --recoverable --parse re > %s", Commands.shellEscape(refmt), Commands.shellEscape(target));
    let (out, error, success) = Commands.execFull(~input=text, cmd);
    /* Log.log("Failed to refmt " ++ cmd ++ "\n" ++ String.concat("\n > ", out @ error)); */
    /* Log.log("The text:"); */
    /* Log.log(text); */
    if (!success) {
      /* Log.log("<< Failed to refmt " ++ cmd ++ "\n" ++ String.concat("\n > ", out @ error)); */
      Error("Failed to refmt " ++ cmd ++ "\n" ++ String.concat("\n > ", out @ error))
    } else {
      Ok((goodError, target))
    }
  }
};

let convertToRe = (~formatWidth, ~interface, text, refmt) => {
  let (out, error, success) = Commands.execFull(~input=text, Printf.sprintf("%s --print re --print-width=%d --parse ml%s", Commands.shellEscape(refmt), formatWidth |? 80, interface ? " -i true" : ""));
  if (success) {
    Ok(String.concat("\n", out))
  } else {
    Error(String.concat("\n", out @ error))
  }
};

let format = (text, fmtCmd) => {
  let (out, error, success) = Commands.execFull(~input=text, fmtCmd);
  if (success) {
    Ok(String.concat("\n", out))
  } else {
    Error(String.concat("\n", out @ error))
  }
};

let justBscCommand = (~interface, ~reasonFormat, ~command, compilerPath, sourceFile, includes, flags) => {
  let flags = reasonFormat ? {
    /* Filter out -pp flags for Reason files. Refmt is the only preprocessor
       that should apply to Reason files. */
    let parts = Utils.split_on_char(' ', flags);
    let rec loop = (items) => {
      switch(items) {
        | ["-pp", _ppFlag, ...rest] => loop(rest)
        | [x, ...rest] => [x, ...loop(rest)]
        | [] => []
      }
    };
    loop(parts) |> String.concat(" ")
  } : flags;
  /* TODO make sure that bsc supports -color */
  Printf.sprintf(
    {|%s %s %s -bin-annot %s %s %s|},
    compilerPath,
    command,
    includes |> List.map(i => Printf.sprintf("-I %s", Commands.shellEscape(i))) |> String.concat(" "),
    flags ++ (reasonFormat ? " -bs-re-out" : ""),
    interface ? "-intf" : "-impl",
    sourceFile
  )
};

let runBsc = (~basePath, ~interface, ~reasonFormat, ~command, compilerPath, sourceFile, includes, flags) => {
  let cmd = justBscCommand(~interface, ~reasonFormat, ~command, compilerPath, sourceFile, includes, flags);
  Log.log({|➡️ running compiler |} ++ cmd ++ " with pwd " ++ basePath);
  let (out, error, success) = Commands.execFull(~pwd=basePath, cmd);
  if (success) {
    Ok((out, error))
  } else {
    Error(out @ error)
  }
};

let getInterface = (~moduleName, ~basePath, ~reasonFormat, text, ~cacheLocation, compilerPath, refmtPath, includes, flags) => {
  let interface = false;
  let%try (_syntaxError, astFile) = switch (refmtPath) {
    | Some(refmtPath) => runRefmt(~interface, ~moduleName, ~cacheLocation, text, refmtPath);
    | None => {
      let astFile = cacheLocation /+ moduleName ++ ".ast" ++ (interface ? "i" : "");
      let%try () = Files.writeFileResult(astFile, text);
      Ok((None, astFile))
    }
  };
  switch (runBsc(~basePath, ~interface, ~reasonFormat, ~command="-i", compilerPath, astFile, includes, flags)) {
    | Error(lines) => {
      Error("Failed to generate interface file\n\n" ++ String.concat("\n", lines))
    }
    | Ok((lines, _errlines)) =>
    let text = String.concat("\n", lines);
    Log.log("GOT ITNERFACE");
    Log.log(text);
    switch (reasonFormat, refmtPath) {
      | (false, Some(refmt)) =>
        convertToRe(~formatWidth=None, ~interface=true, text, refmt)
      | _ => Ok(text)
    }
  }
};

let cmtPath = (~cacheLocation, ~moduleName, ~uri) => {
  let interface = Utils.endsWith(uri, "i");
  cacheLocation /+ moduleName ++ ".cmt" ++ (interface ? "i" : "");
};

let isMl = (uri) =>Utils.endsWith(uri, "ml") || Utils.endsWith(uri, "mli");

let getParsetree = (~cacheLocation, ~compilerVersion, ~moduleName, ~uri) => {
  let cmt = cmtPath(~cacheLocation, ~moduleName, ~uri);
  (switch compilerVersion {
    | BuildSystem.V402 => Process_402.astForCmt
    | V406 => Process_406.astForCmt
    | V407 => Process_407.astForCmt
    | V408 => Process_408.astForCmt
  })(cmt);
};

/* let getSource = (~cacheLocation, ~compilerVersion, ~moduleName, ~uri) => {
  let%try parsetree = getParsetree(~cacheLocation, ~compilerVersion, ~moduleName, ~uri);
  if (isMl(uri)) {
    switch (parsetree) {
      | `Implementation(str) => Pprintast.structure(Format.str_formatter, str)
      | `Interface(int) => Pprintast.signature(Format.str_formatter, int)
    };
    Ok(Format.flush_str_formatter())
  } else {
    switch (parsetree) {
      | `Implementation(str) =>
        Reason_toolchain.RE.print_implementation(Format.str_formatter, structure);
      | `Interface(int) =>
      ()
    };
    Ok("")
  }
};

let getAst = (~cacheLocation, ~compilerVersion, ~moduleName, ~uri) => {
  let cmt = cmtPath(~cacheLocation, ~moduleName, ~uri);
  /* (switch compilerVersion {
    | BuildSystem.V402 => Process_402.astForCmt
    | V406 => Process_406.astForCmt
    | V407 => Process_407.astForCmt
    | V408 => Process_408.astForCmt
  })(cmt); */
  Ok("NVM")
}; */

let process = (~uri, ~moduleName, ~basePath, ~reasonFormat, text, ~cacheLocation, ~compilerVersion, ~allLocations, compilerPath, refmtPath, includes, flags) => {
  let interface = Utils.endsWith(uri, "i");
  let%try (syntaxError, astFile) = switch (refmtPath) {
    | Some(refmtPath) => runRefmt(~interface, ~moduleName, ~cacheLocation, text, refmtPath);
    | None => {
      let astFile = cacheLocation /+ moduleName ++ ".ast" ++ (interface ? "i" : "");
      let%try () = Files.writeFileResult(astFile, text);
      Ok((None, astFile))
    }
  };
  let fullForCmt = (switch compilerVersion {
    | BuildSystem.V402 => Process_402.fullForCmt
    | V406 => Process_406.fullForCmt
    | V407 => Process_407.fullForCmt
    | V408 => Process_408.fullForCmt
  })(~moduleName, ~allLocations);
  let cmtPath = cacheLocation /+ moduleName ++ ".cmt" ++ (interface ? "i" : "");
  try (Unix.unlink(cmtPath)) { | _ => ()};
  switch (runBsc(~basePath, ~interface, ~reasonFormat, ~command="-c", compilerPath, astFile, includes, flags)) {
    | Error(lines) => {
      if (!Files.isFile(cmtPath)) {
        Ok(TypeError(String.concat("\n", lines), SharedTypes.initFull(moduleName, uri)))
      } else {
        Log.log("Now loading " ++ cmtPath);
        switch (Files.maybeStat(cmtPath)) {
        | Some({Unix.st_size: size}) => Log.log("Size " ++ string_of_int(size))
        | _ => Log.log("Doesn't exist")
        };
        let%try_wrap {file, extra} = fullForCmt(cmtPath, uri, x => x);
        let errorText = String.concat("\n", lines);
        switch (syntaxError) {
          | Some(s) =>
            /** TODO also report the type errors / warnings from the partial result */
            SyntaxError(String.concat("\n", s), errorText, {file, extra})
          | None => {
            let errorText = switch (ErrorParser.parseDependencyError(errorText)) {
              | Some((name, _oname, _iface)) => errorText ++ "\n\nThis is likely due to an error in module " ++ name
              | None => errorText
            };
            TypeError(errorText, {file, extra})
          }
        }
      }
    }
    | Ok((lines, error)) => {
      // Log.log("Now loading " ++ cmtPath);
      // switch (Files.maybeStat(cmtPath)) {
      // | Some({Unix.st_size: size}) =>
      // Log.log("Size " ++ string_of_int(size))
      // let ic = open_in_bin(cmtPath);
      // let buffer = really_input_string(ic,String.length(Config.cmi_magic_number));
      // Log.log(buffer);
      // Log.log(Config.cmi_magic_number);
      // close_in(ic);
      // | _ => Log.log("Doesn't exist")
      // };
      let%try_wrap full = fullForCmt(cmtPath, uri, x => x);
      Success(String.concat("\n", lines @ error), full)
    }
  }
};
