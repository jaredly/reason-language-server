
type result =
  | ParseError(string)
  | TypeError(string, Cmt_format.cmt_infos, Definition.moduleData)
  | Success(list(string), Cmt_format.cmt_infos, Definition.moduleData)
;
open Infix;
open Result;

let runRefmt = (text, rootPath) => {
  let refmt = rootPath /+ "node_modules/bs-platform/lib/refmt.exe";
  let (out, error, success) = Commands.execFull(~input=text, Printf.sprintf("%S --print binary --parse re > /tmp/ls.ast", refmt));
  if (success) {
    Ok("/tmp/ls.ast")
  } else {
    Error(out @ error)
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

let justBscCommand = (rootPath, sourceFile, includes, flags) => {
  Printf.sprintf(
    {|%s -w +A %s -impl %s %s|},
    rootPath /+ "node_modules/bs-platform/lib/bsc.exe",
    includes |> List.map(Printf.sprintf("-I %S")) |> String.concat(" "),
    sourceFile,
    flags
  )
};

let runBsc = (rootPath, sourceFile, includes, flags) => {
  let cmd = justBscCommand(rootPath, sourceFile, includes, flags);
  let (out, error, success) = Commands.execFull(cmd);
  if (success) {
    Ok(out @ error)
  } else {
    Error(out @ error)
  }
};

let process = (text, rootPath, includes, flags) => {
  Log.log("Compiling text " ++ text);
  open InfixResult;
  switch (runRefmt(text, rootPath)) {
  | Error(lines) => ParseError(String.concat("\n", lines))
  | Ok(fname) => switch (runBsc(rootPath, fname, includes, flags)) {
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