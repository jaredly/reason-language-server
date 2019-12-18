
open SharedTypes;

/* Log.spamError := true; */

let fixture = {|
=== Basic
module One = {
  let x = 10;
};
open One;
let y = x;
-->
line 4: 1 uses. values: {x}

=== Nested with include
module Two = {
  let y = 30;
};
module One = {
  let x = 10;
  include Two;
};
open One;
let z = y + x;
-->
line 8: 2 uses. values: {x, y}

=== Local open
module One = {
  let x = 10;
};
let _ = {
  open One;
  let y = x;
};
-->
line 5: 1 uses. values: {x}

=== Multiple
module One = {
  let x = 10;
};
module Two = {
  let y = 30;
};
open One;
open Two;
let r = x + y;
-->
line 7: 1 uses. values: {x}
line 8: 1 uses. values: {y}
|};

let getOutput = (~projectDir, files, text) => {
  /* let (text, offset, pos) = TestUtils.extractPosition(text); */
  let (_state, _package, _cmt, full) = TestUtils.setUp(~projectDir, files, text);

  let opens = CodeLens.forOpens(full.extra);

  opens |> List.map(((item, loc)) => {
    "line " ++ string_of_int(loc.Location.loc_start.pos_lnum) ++ ": " ++ item
  }) |> String.concat("\n");
};

let name = "TestCodeLens";