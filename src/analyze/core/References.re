
open SharedTypes;

let maybeLog = m => {
  /* Log.log(m); */
  ()
};

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
    checkPos(pos, loc) ? Some((loc, l)) : None
  });
};

let local = (~file, ~extra, loc) =>
  switch (loc) {
  | Loc.Explanation(_)
  | Typed(_, NotFound)
  | Module(NotFound)
  | TopLevelModule(_)
  | Open => None
  | TypeDefinition(_, _, stamp) => {
    extra.internalReferences |. Query.hashFind(stamp)
  }
  | Module(LocalReference(stamp, tip) | Definition(stamp, tip))
  | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip)) =>
    open Infix;
    let%opt localStamp = switch tip {
      | Constructor(name) => Query.getConstructor(file, stamp, name) |?>> x => x.stamp
      | Attribute(name) => Query.getAttribute(file, stamp, name) |?>> x => x.stamp
      | _ => Some(stamp)
    };
    extra.internalReferences |. Query.hashFind(localStamp)
  | Module(GlobalReference(moduleName, path, tip))
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    let%opt_wrap refs = extra.externalReferences |. Query.hashFind(moduleName);
    refs |. Belt.List.keepMap(((p, t, l)) => p == path && t == tip ? Some(l) : None);
  };

let definedForLoc = (~file, ~getModule, loc) => {
  let inner = (~file, stamp, tip) => {
    open Infix;
    switch tip {
      | Constructor(name) => {
        let%opt declared = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
        let%opt constructor = Query.getConstructor(file, stamp, name);
        Some((declared, file, `Constructor(constructor)))
      }
      | Attribute(name) => {
        let%opt declared = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
        let%opt attribute = Query.getAttribute(file, stamp, name);
        Some((declared, file, `Attribute(attribute)))
      }
      | _ => {
        maybeLog("Trying for declared " ++ SharedTypes.tipToString(tip) ++ " " ++ string_of_int(stamp) ++ " in file " ++ file.uri);
        let%opt x = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
        Some((x, file, `Declared))
      }
    };
  };


  switch (loc) {
  | Loc.Explanation(_)
  | Typed(_, NotFound)
  | Module(NotFound)
  | TopLevelModule(_)
  | Open => None
  | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip))
  | Module(LocalReference(stamp, tip) | Definition(stamp, tip)) =>
    inner(~file, stamp, tip)
  | TypeDefinition(_, _, stamp) => inner(~file, stamp, Type)
  | Module(GlobalReference(moduleName, path, tip))
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    {
      maybeLog("Getting global " ++ moduleName);
      let%try file = getModule(moduleName) |> Result.orError("Cannot get module " ++ moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%try (env, name) = Query.resolvePath(~env, ~path, ~getModule) |> Result.orError("Cannot resolve path " ++ pathToString(path));
      let%try stamp = Query.exportedForTip(~env, name, tip) |> Result.orError("Exported not found for tip " ++ name ++ " > " ++ tipToString(tip));
      maybeLog("Getting for " ++ string_of_int(stamp) ++ " in " ++ name);
      let%try res = inner(~file=env.file, stamp, tip) |> Result.orError("could not get defined");
      maybeLog("Yes!! got it");
      Ok(res)
    } |> Result.toOptionAndLog
    /* let%try extra = getExtra(moduleName) |> Result.orError("Failed to get extra for " ++ env.file.uri); */
    /* maybeLog("Finding references for (global) " ++ file.uri ++ " and stamp " ++ string_of_int(stamp) ++ " and tip " ++ tipToString(tip)); */
    /* forLocalStamp(~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) |> Result.orError("Could not get for local stamp") */
  };
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
    maybeLog("Checking externals: " ++ string_of_int(stamp));
    let%opt declared = Query.declaredForTip(~stamps=env.file.stamps, stamp, tip);
    if (isVisible(declared)) {
      let%opt path = pathFromVisibility(declared.modulePath, declared.name.txt);
      maybeLog("Now checking path " ++ pathToString(path));
      let thisModuleName = file.moduleName;
      allModules |. Belt.List.keep(name => name != file.moduleName) |. Belt.List.keepMap(name => {
        let%try file = getModule(name) |> Result.orError("Could not get file for module " ++ name);
        let%try extra = getExtra(name) |> Result.orError("Could not get extra for module " ++ name);
        let%try refs = extra.externalReferences |. Query.hashFind(thisModuleName) |> Result.orError("No references in " ++ name ++ " for " ++ thisModuleName);
        let refs = refs |. Belt.List.keepMap(((p, t, l)) => p == path && t == tip ? Some(l) : None);
        Ok((file.uri, refs))
      } |> Result.toOptionAndLog) |. Some
    } else {
      maybeLog("Not visible");
      Some([])
    }
  } |? [];
  Some([(file.uri, local), ...externals])
};

let forLoc = (~file, ~extra, ~allModules, ~getModule, ~getExtra, loc) => {
  switch (loc) {
    | Loc.Explanation(_)
    | Typed(_, NotFound)
    | Module(NotFound)
    | TopLevelModule(_)
    | Open => Result.Error("Not a valid loc")
    | TypeDefinition(_, _, stamp) => {
      forLocalStamp(~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, Type) |> Result.orError("Could not get for local stamp")
    }
    | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip))
    | Module(LocalReference(stamp, tip) | Definition(stamp, tip))
    => {
      maybeLog("Finding references for " ++ file.uri ++ " and stamp " ++ string_of_int(stamp) ++ " and tip " ++ tipToString(tip));
      forLocalStamp(~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) |> Result.orError("Could not get for local stamp")
    }
    | Module(GlobalReference(moduleName, path, tip))
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      let%try file = getModule(moduleName) |> Result.orError("Cannot get module " ++ moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%try (env, name) = Query.resolvePath(~env, ~path, ~getModule) |> Result.orError("Cannot resolve path " ++ pathToString(path));
      let%try stamp = Query.exportedForTip(~env, name, tip) |> Result.orError("Exported not found for tip " ++ name ++ " > " ++ tipToString(tip));
      let%try extra = getExtra(moduleName) |> Result.orError("Failed to get extra for " ++ env.file.uri);
      maybeLog("Finding references for (global) " ++ file.uri ++ " and stamp " ++ string_of_int(stamp) ++ " and tip " ++ tipToString(tip));
      forLocalStamp(~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) |> Result.orError("Could not get for local stamp")
    }
  }
};

let forPos = (~file, ~extra, pos) => {
  let%opt (_, loc) = locForPos(~extra, pos);
  maybeLog("Got a loc for pos");
  let%opt refs = local(~file, ~extra, loc);
  Some(refs)
};

let validateLoc = (loc: Location.t, backup: Location.t) => {
  if (loc.loc_start.pos_cnum == -1) {
    if (backup.loc_start.pos_cnum == -1) {
      {
        Location.loc_ghost: true,
        loc_start: {
          pos_cnum: 0,
          pos_lnum: 1,
          pos_bol: 0,
          pos_fname: ""
        },
        loc_end: {
          pos_cnum: 0,
          pos_lnum: 1,
          pos_bol: 0,
          pos_fname: ""
        },
        }
    } else {
      backup
    }
  } else {
    loc
  }
};

let rec definition = (~file, ~getModule, stamp, tip) => {
  switch tip {
    | Constructor(name) =>
      let%opt constructor = Query.getConstructor(file, stamp, name);
      Some((file.uri, constructor.name.loc))
    | Attribute(name) =>
      let%opt attribute = Query.getAttribute(file, stamp, name);
      Some((file.uri, attribute.name.loc))
    | Module =>
      let%opt md = Query.hashFind(file.stamps.modules, stamp);
      switch (md.contents) {
        | Structure(_) =>
          Some((file.uri, validateLoc(md.name.loc, md.extentLoc)))
        | Ident(path) => 
          let env = {Query.file, exported: file.contents.exported};
          switch (Query.fromCompilerPath(~env, path)) {
            | `Not_found => None
            | `Exported(env, name) =>
              let%opt stamp = Query.hashFind(env.exported.modules, name);
              let%opt md = Query.hashFind(env.file.stamps.modules, stamp);
              Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc)))
            | `Global(moduleName, path) =>
              let%opt file = getModule(moduleName);
              let env = {file, Query.exported: file.contents.exported};
              let%opt (env, name) = Query.resolvePath(~env, ~getModule, ~path);
              let%opt stamp = Query.hashFind(env.exported.modules, name);
              let%opt md = Query.hashFind(env.file.stamps.modules, stamp);
              Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc)))
            | `Stamp(stamp) =>
              let%opt md = Query.hashFind(file.stamps.modules, stamp);
              Some((file.uri, validateLoc(md.name.loc, md.extentLoc)))
            | `GlobalMod(name) =>
              let%opt file = getModule(name);
              /* Log.log("Congrats, found a global mod"); */
              Some((file.uri, Utils.topLoc(file.uri)))
            | _ => None
          };
          /* let%opt stamp = Query.exportedForTip(~env, name, tip);
          /** oooh wht do I do if the stamp is inside a pseudo-file? */
          Log.log("Got stamp " ++ string_of_int(stamp));
          definition(~file=env.file, stamp, tip) */
      }
    | _ =>
      let%opt declared = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
      let loc = validateLoc(declared.name.loc, declared.extentLoc);
      Some((file.uri, loc))
  };
};

let definitionForLoc = (~package, ~file, ~getModule, loc) => {
  switch (loc) {
    | Loc.Explanation(_)
    | Typed(_, NotFound | Definition(_, _))
    | Module(NotFound | Definition(_, _))
    | TypeDefinition(_, _, _)
    | Open => None
    | TopLevelModule(name) =>
      maybeLog("Toplevel " ++ name);
      open Infix;
      let%opt src = Utils.maybeHash(package.TopTypes.pathsForModule, name) |?> TopTypes.getSrc;
      Some((Utils.toUri(src), Utils.topLoc(src)))
      /* switch (Hashtbl.find(package.TopTypes.pathsForModule, name)) {
        | (_, Some(src)) => Some((Utils.toUri(src), Utils.topLoc(src)))
        | (_, None) => None
        | exception Not_found => {
          maybeLog("No path for module " ++ name);
          None
        }
      } */
    | Module(LocalReference(stamp, tip))
    | Typed(_, LocalReference(stamp, tip)) => {
      maybeLog("Local defn " ++ SharedTypes.tipToString(tip));
      definition(~file, ~getModule, stamp, tip)
    }
    | Module(GlobalReference(moduleName, path, tip))
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      maybeLog("Global defn " ++ moduleName ++ " " ++ SharedTypes.pathToString(path) ++ " : " ++ SharedTypes.tipToString(tip));
      let%opt file = getModule(moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
      let%opt stamp = Query.exportedForTip(~env, name, tip);
      /** oooh wht do I do if the stamp is inside a pseudo-file? */
      maybeLog("Got stamp " ++ string_of_int(stamp));
      definition(~file=env.file, ~getModule, stamp, tip)
    }
  }
};

let definitionForPos = (~package, ~file, ~extra, ~getModule, pos) => {
  let%opt (_, loc) = locForPos(~extra, pos);
  maybeLog("Got a loc for pos");
  definitionForLoc(~package, ~file, ~getModule, loc)
};
