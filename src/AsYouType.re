
type result =
  | ParseError(string)
  | TypeError(string, Cmt_format.cmt_infos, Definition.moduleData)
  | Success(list(string), Cmt_format.cmt_infos, Definition.moduleData)
;
open Infix;
open Result;

let getResult = result => switch result {
| ParseError(_) => None
| TypeError(_, cmt, data) => Some((cmt, data))
| Success(_, cmt, data) => Some((cmt, data))
};

let runRefmt = (text, refmt) => {
  let (out, error, success) = Commands.execFull(~input=text, Printf.sprintf("%S --print binary --parse re > /tmp/ls.ast", refmt));
  if (success) {
    Ok("/tmp/ls.ast")
  } else {
    Error(out @ error)
  }
};

let format = (text, refmt) => {
  let (out, error, success) = Commands.execFull(~input=text, Printf.sprintf("%S --print re --parse re", refmt));
  if (success) {
    Ok(String.concat("\n", out))
  } else {
    Error(String.concat("\n", out @ error))
  }
};

let parseTypeError = text => {
  let rx = Str.regexp("File \"[^\"]+\", line ([0-9]), characters ([0-9])+-([0-9])+:");
  let rx = Str.regexp({|File "[^"]*", line \([0-9]+\), characters \([0-9]+\)-\([0-9]+\):|});
  if (Str.string_match(rx, text, 0)) {
    let line = Str.matched_group(1, text) |> int_of_string;
    let c0 = Str.matched_group(2, text) |> int_of_string;
    let c1 = Str.matched_group(3, text) |> int_of_string;
    Some((line - 1, c0, c1))
  } else {
    None
  }
};

let justBscCommand = (compilerPath, sourceFile, ~libraries, includes, flags) => {
  Printf.sprintf(
    {|%s -w +A %s %s -bin-annot -impl %s %s|},
    compilerPath,
    includes |> List.map(Printf.sprintf("-I %S")) |> String.concat(" "),
    libraries |> List.map(Printf.sprintf("%S")) |> String.concat(" "),
    sourceFile,
    flags
  )
};

let runBsc = (compilerPath, sourceFile, ~libraries, includes, flags) => {
  let cmd = justBscCommand(compilerPath, sourceFile, ~libraries, includes, flags);
  Log.log("running bsc " ++ cmd);
  let (out, error, success) = Commands.execFull(cmd);
  if (success) {
    Ok(out @ error)
  } else {
    Error(out @ error)
  }
};

let process = (text, compilerPath, refmtPath, ~libraries, includes, flags) => {
  /* Log.log("Compiling text " ++ text); */
  open InfixResult;
  switch (runRefmt(text, refmtPath)) {
  | Error(lines) => ParseError(String.concat("\n", lines))
  | Ok(fname) => switch (runBsc(compilerPath, fname, ~libraries, includes, flags)) {
    | Error(lines) => {
      let cmt = Cmt_format.read_cmt("/tmp/ls.cmt");
      TypeError(String.concat("\n", lines), cmt, Definition.process(cmt.Cmt_format.cmt_annots))
    }
    | Ok(lines) => {
      let cmt = Cmt_format.read_cmt("/tmp/ls.cmt");
      Success(lines, cmt, Definition.process(cmt.Cmt_format.cmt_annots))
    }
  }
  }
};