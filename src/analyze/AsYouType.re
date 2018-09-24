
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

let format = (~formatWidth, ~interface, text, refmt) => {
  let (out, error, success) = Commands.execFull(~input=text, Printf.sprintf("%s --print re --print-width=%d --parse re%s", Commands.shellEscape(refmt), formatWidth |? 80, interface ? " -i true" : ""));
  if (success) {
    Ok(String.concat("\n", out))
  } else {
    Error(String.concat("\n", out @ error))
  }
};

let parseTypeError = text => {
  /* let rx = Str.regexp("File \"[^\"]+\", line ([0-9]), characters ([0-9])+-([0-9])+:"); */
  let rx = Str.regexp({|File "[^"]*", line \([0-9]+\), characters \([0-9]+\)-\([0-9]+\):
?|});
  if (Str.string_match(rx, text, 0)) {
    let line = Str.matched_group(1, text) |> int_of_string;
    let c0 = Str.matched_group(2, text) |> int_of_string;
    let c1 = Str.matched_group(3, text) |> int_of_string;
    let final = Str.match_end();
    Some((line - 1, c0, c1, String.sub(text, final, String.length(text) - final)))
  } else {
    Log.log("Cannot parse type error: " ++ text);
    None
  }
};

let parseLoc = text => {
  /* let rx = Str.regexp("File \"[^\"]+\", line ([0-9]), characters ([0-9])+-([0-9])+:"); */
  let rx = Str.regexp({|File "[^"]*", line \([0-9]+\), characters \([0-9]+\)-\([0-9]+\):|});
  if (Str.string_match(rx, text, 0)) {
    let line = Str.matched_group(1, text) |> int_of_string;
    let c0 = Str.matched_group(2, text) |> int_of_string;
    let c1 = Str.matched_group(3, text) |> int_of_string;
    let final = Str.match_end();
    Some((line - 1, c0, c1))
  } else {
    /* Log.log("Cannot parse type error: " ++ text); */
    None
  }
};

let parseErrors = lines => {
  let rec loop = lines => switch lines {
    | [] => ([], [])
    | [line, ...rest] => {
      let (tail, items) = loop(rest);
      switch (parseLoc(line)) {
        | None => ([line, ...tail], items)
        | Some(loc) => ([], [(loc, tail), ...items])
      }
    }
  };
  let (tail, errors) = loop(lines);
  let errors = tail == [] ? errors : [((0, 0, 0), tail), ...errors];

  errors
};

let parseDependencyError = text => {
  let rx = Str.regexp({|Error: The files \(.+\)\.cmi
       and \(.+\)\.cmi
       make inconsistent assumptions over interface \([A-Za-z_-]+\)|});
  
  switch (Str.search_forward(rx, text, 0)) {
  | exception Not_found => None
  | x =>
    let dep = Str.matched_group(1, text) |> Filename.basename |> String.capitalize;
    let base = Str.matched_group(2, text) |> Filename.basename |> String.capitalize;
    let interface = Str.matched_group(3, text);
    let final = Str.match_end();
    Some((dep, base, interface))
  }
};

let justBscCommand = (~interface, ~reasonFormat, compilerPath, sourceFile, includes, flags) => {
  /* TODO make sure that bsc supports -color */
  Printf.sprintf(
    {|%s %s -bin-annot %s %s %s|},
    compilerPath,
    includes |> List.map(i => Printf.sprintf("-I %s", Commands.shellEscape(i))) |> String.concat(" "),
    flags ++ (reasonFormat ? " -bs-re-out" : ""),
    interface ? "-intf" : "-impl",
    sourceFile
  )
};

let runBsc = (~basePath, ~interface, ~reasonFormat, compilerPath, sourceFile, includes, flags) => {
  let cmd = justBscCommand(~interface, ~reasonFormat, compilerPath, sourceFile, includes, flags);
  Log.log("running bsc " ++ cmd ++ " with pwd " ++ basePath);
  let (out, error, success) = Commands.execFull(~pwd=basePath, cmd);
  if (success) {
    Ok(out @ error)
  } else {
    Error(out @ error)
  }
};

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
  })(~moduleName, ~allLocations);
  switch (runBsc(~basePath, ~interface, ~reasonFormat, compilerPath, astFile, includes, flags)) {
    | Error(lines) => {
      let cmtPath = cacheLocation /+ moduleName ++ ".cmt" ++ (interface ? "i" : "");
      if (!Files.isFile(cmtPath)) {
        Ok(TypeError(String.concat("\n", lines), SharedTypes.initFull(moduleName, uri)))
      } else {
        let%try_wrap {file, extra} = fullForCmt(cmtPath, uri, x => x);
        let errorText = String.concat("\n", lines);
        switch (syntaxError) {
          | Some(s) =>
            /** TODO also report the type errors / warnings from the partial result */
            SyntaxError(String.concat("\n", s), errorText, {file, extra})
          | None => {
            let errorText = switch (parseDependencyError(errorText)) {
              | Some((name, oname, iface)) => errorText ++ "\n\nThis is likely due to an error in module " ++ name
              | None => errorText
            };
            TypeError(errorText, {file, extra})
          }
        }
      }
    }
    | Ok(lines) => {
      let cmt = cacheLocation /+ moduleName ++ ".cmt" ++ (interface ? "i" : "");
      let%try_wrap full = fullForCmt(cmt, uri, x => x);
      Success(String.concat("\n", lines), full)
    }
  }
};
