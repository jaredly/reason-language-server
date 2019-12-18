
open SharedTypes;

let getOutput = (~projectDir, files, text) => {
  let (_state, _package, _cmt, full) = TestUtils.setUp(~projectDir, files, text);

  let opens = CodeLens.forOpens(full.extra);

  opens |> List.map(((item, loc)) => {
    "line " ++ string_of_int(loc.Location.loc_start.pos_lnum) ++ ": " ++ item
  }) |> String.concat("\n");
};

let name = "TestCodeLens";