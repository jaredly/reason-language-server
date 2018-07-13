
open SharedTypes;

let checkPos = ((line, char), {Location.loc_start: {pos_lnum, pos_bol, pos_cnum}, loc_end}) =>
  Lexing.(
    if (line < pos_lnum || line == pos_lnum && char < pos_cnum - pos_bol) {
      false
    } else if (line > loc_end.pos_lnum
               || line == loc_end.pos_lnum
               && char > loc_end.pos_cnum
               - loc_end.pos_bol) {
      false
    } else {
      true
    }
  );

let forPos = (~extra, ~getModule, pos) => {
  let%opt loc = extra.locations |> Utils.find(((loc, l)) => {
    /* let (line, char) = pos; */
    open Location;
    /* Log.log(Printf.sprintf("At (%d, %d): %d %d ", line, char, loc.loc_start.pos_lnum, loc.loc_start.pos_cnum - loc.loc_start.pos_bol)); */
    checkPos(pos, loc) ? Some(l) : None
  });
  switch (loc) {
    | Explanation(_)
    | Typed(_, NotFound)
    | Open => None
    | Typed(_, LocalReference(stamp, tip))
    | Typed(_, Definition(stamp, tip)) => {
      extra.internalReferences
      |. Hashtbl.find(stamp)
      |. x => Some([("file:///path/to/Test.re", x)])
    }
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      None
    }
  }
};