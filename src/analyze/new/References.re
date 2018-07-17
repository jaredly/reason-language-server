
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
    open Location;
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

let forLocalStamp = (~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) => {
  let env = {Query.file, exported: file.contents.exported};
  open Infix;
  let%opt localStamp = switch tip {
    | Constructor(name) => Query.getConstructor(file, stamp, name) |?>> x => x.stamp
    | Attribute(name) => Query.getAttribute(file, stamp, name) |?>> x => x.stamp
    | _ => Some(stamp)
  };
  let%opt local = extra.internalReferences |. Query.hashFind(localStamp);
  open Infix;
  let externals = {
    print_endline("Checking externals: " ++ string_of_int(stamp));
    let%opt declared = Query.declaredForTip(~stamps=env.file.stamps, stamp, tip);
    if (isVisible(declared)) {
      let%opt path = pathFromVisibility(declared.modulePath, declared.name.txt);
      print_endline("Now checking path " ++ pathToString(path));
      let thisModuleName = file.moduleName;
      allModules |. Belt.List.keep(name => name != file.moduleName) |. Belt.List.keepMap(name => {
        let%opt file = getModule(name);
        let%opt extra = getExtra(name);
        let%opt refs = extra.externalReferences |. Query.hashFind(thisModuleName);
        let refs = refs |. Belt.List.keepMap(((p, t, l)) => p == path && t == tip ? Some(l) : None);
        Some((file.uri, refs))
      }) |. Some
    } else {
      print_endline("Not visible");
      Some([])
    }
  } |? [];
  Some([(file.uri, local), ...externals])
};

let forLoc = (~file, ~extra, ~allModules, ~getModule, ~getExtra, loc) => {
  switch (loc) {
    | Loc.Explanation(_)
    | Typed(_, NotFound)
    | Open => None
    | Typed(_, LocalReference(stamp, tip))
    | Typed(_, Definition(stamp, tip)) => {
      forLocalStamp(~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip)
    }
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      let%opt file = getModule(moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
      let%opt stamp = Query.exportedForTip(~env, name, tip);
      let%opt extra = getExtra(env.file.uri);
      let local = extra.internalReferences |. Query.hashFind(stamp);
      /* TODO TODO */
      None
    }
  }
};

let forPos = (~extra, pos) => {
  let%opt loc = locForPos(~extra, pos);
  let%opt refs = local(~extra, loc);
  Some(refs)
};

let definition = (~file, stamp, tip) => {
  switch tip {
    | Constructor(name) =>
      let%opt constructor = Query.getConstructor(file, stamp, name);
      Some((file.uri, constructor.name.loc))
    | Attribute(name) =>
      let%opt attribute = Query.getAttribute(file, stamp, name);
      Some((file.uri, attribute.name.loc))
    | _ =>
      let%opt declared = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
      Some((file.uri, declared.name.loc))
  };
};

let definitionForLoc = (~file, ~getModule, loc) => {
  switch (loc) {
    | Loc.Explanation(_)
    | Typed(_, NotFound)
    | Typed(_, Definition(_, _))
    | Open => None
    | Typed(_, LocalReference(stamp, tip)) => {
      definition(~file, stamp, tip)
    }
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      let%opt file = getModule(moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
      let%opt stamp = Query.exportedForTip(~env, name, tip);
      definition(~file, stamp, tip)
    }
  }
};

let definitionForPos = (~file, ~extra, ~getModule, pos) => {
  let%opt loc = locForPos(~extra, pos);
  definitionForLoc(~file, ~getModule, loc)
};
