
type result =
  /* | ParseError(string) */
  | SyntaxError(string, SharedTypes.full)
  | TypeError(string, SharedTypes.full)
  | Success(list(string), SharedTypes.full)
;
open Infix;
open Result;

let getResult = result => switch result {
| SyntaxError(_, data) => data
| TypeError(_, data) => data
| Success(_, data) => data
};

let runRefmt = (~interface, ~moduleName, ~cacheLocation, text, refmt) => {
  let target = cacheLocation /+ moduleName ++ ".ast";
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

let format = (~formatWidth, text, refmt) => {
  let (out, error, success) = Commands.execFull(~input=text, Printf.sprintf("%s --print re --print-width=%d --parse re", Commands.shellEscape(refmt), formatWidth |? 80));
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

let parseDependencyError = text => {
  let rx = Str.regexp({|Error: The files \(.+\)\.cmi
       and \(.+\)\.cmi
       make inconsistent assumptions over interface \([A-Za-z_-]+\)|});
  
  switch (Str.search_forward(rx, text, 0)) {
  | exception Not_found => None
  | x =>
    let dep = Str.matched_group(1, text) |> String.capitalize;
    let base = Str.matched_group(2, text);
    let baseName = Str.matched_group(3, text);
    let final = Str.match_end();
    Some(Filename.dirname(dep))
  }
};

let justBscCommand = (~interface, compilerPath, sourceFile, includes, flags) => {
  /* TODO make sure that bsc supports -color */
  Printf.sprintf(
    {|%s %s -bin-annot %s %s %s|},
    compilerPath,
    includes |> List.map(i => Printf.sprintf("-I %s", Commands.shellEscape(i))) |> String.concat(" "),
    flags,
    interface ? "-intf" : "-impl",
    sourceFile
  )
};

let runBsc = (~interface, compilerPath, sourceFile, includes, flags) => {
  let cmd = justBscCommand(~interface, compilerPath, sourceFile, includes, flags);
  Log.log("running bsc " ++ cmd);
  let (out, error, success) = Commands.execFull(cmd);
  if (success) {
    Ok(out @ error)
  } else {
    Error(out @ error)
  }
};

let process = (~uri, ~moduleName, text, ~cacheLocation, compilerPath, refmtPath, includes, flags) => {
  let interface = Utils.endsWith(uri, "i");
  let%try (syntaxError, astFile) = switch (refmtPath) {
    | Some(refmtPath) => runRefmt(~interface, ~moduleName, ~cacheLocation, text, refmtPath);
    | None => {
      let astFile = cacheLocation /+ moduleName ++ ".ast";
      let%try () = Files.writeFileResult(astFile, text);
      Ok((None, astFile))
    }
  };
  switch (runBsc(~interface, compilerPath, astFile, includes, flags)) {
    | Error(lines) => {
      let cmtPath = cacheLocation /+ moduleName ++ ".cmt";
      if (!Files.isFile(cmtPath)) {
        Ok(TypeError(String.concat("\n", lines), SharedTypes.initFull(moduleName, uri)))
      } else {
        let cmt = Cmt_format.read_cmt(cmtPath);
        let%try file = ProcessCmt.forCmt(uri, x => x, cmt);
        let%try_wrap extra = ProcessExtra.forCmt(~file, cmt);
        switch (syntaxError) {
          | Some(s) => SyntaxError(String.concat("\n", s), {file, extra})
          | None => {
            let errorText = String.concat("\n", lines);
            let errorText = switch (parseDependencyError(errorText)) {
              | Some(name) => errorText ++ "\n\nThis is likely due to an error in module " ++ name
              | None => errorText
            };
            TypeError(errorText, {file, extra})
          }
        }
      }
    }
    | Ok(lines) => {
      let cmt = Cmt_format.read_cmt(cacheLocation /+ moduleName ++ ".cmt");
      let%try file = ProcessCmt.forCmt(uri, x => x, cmt);
      let%try_wrap extra = ProcessExtra.forCmt(~file, cmt);
      Success(lines, {file, extra})
    }
  }
};
