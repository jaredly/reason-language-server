
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

let locForPos = (~extra, pos) => {
  extra.locations |> Utils.find(((loc, l)) => {
    /* let (line, char) = pos; */
    open Location;
    /* Log.log(Printf.sprintf("At (%d, %d): %d %d ", line, char, loc.loc_start.pos_lnum, loc.loc_start.pos_cnum - loc.loc_start.pos_bol)); */
    checkPos(pos, loc) ? Some(l) : None
  });
};

let local = (~extra, loc) =>
  switch (loc) {
  | Loc.Explanation(_)
  | Typed(_, NotFound)
  | Open => None
  | Typed(_, LocalReference(stamp, tip))
  | Typed(_, Definition(stamp, tip)) =>
    extra.internalReferences |. Query.hashFind(stamp)
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    let%opt_wrap refs = extra.externalReferences |. Query.hashFind(moduleName);
    refs |. Belt.List.keepMap(((p, t, l)) => p == path && t == tip ? Some(l) : None);
  };

let forLoc = (~file, ~extra, ~getModule, ~getExtra, loc) => {
  switch (loc) {
    | Loc.Explanation(_)
    | Typed(_, NotFound)
    | Open => None
    | Typed(_, LocalReference(stamp, tip))
    | Typed(_, Definition(stamp, tip)) => {
      let local = extra.internalReferences |. Query.hashFind(stamp);
      None
    }
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      let%opt file = getModule(moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
      let%opt stamp = Query.exportedForTip(~env, name, tip);
      let%opt extra = getExtra(env.file.uri);
      let local = extra.internalReferences |. Query.hashFind(stamp);
      None
    }
  }
};

let forPos = (~extra, ~getModule, pos) => {
  let%opt loc = locForPos(~extra, pos);
  let%opt refs = local(~extra, loc);
  Some([("file:///path/to/Test.re", refs)])
};