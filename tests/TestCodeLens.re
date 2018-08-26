
open Infix;
open SharedTypes;

/* Log.spamError := true; */

let getOutput = (files, text) => {
  /* let (text, offset, pos) = TestUtils.extractPosition(text); */
  let (state, package, cmt, full) = TestUtils.setUp(files, text);

  let opens = CodeLens.forOpens(full.extra);

  opens |> List.map(((item, loc)) => {
    "line " ++ string_of_int(loc.Location.loc_start.pos_lnum) ++ ": " ++ item
  }) |> String.concat("\n");
};

let name = "TestCodeLens";