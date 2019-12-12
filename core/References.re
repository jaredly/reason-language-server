
open SharedTypes;

let debugReferences = ref(true);

let maybeLog = m => {
  if (debugReferences^) {
    Log.log("[ref] " ++ m);
  }
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

let posMatch = ({Lexing.pos_cnum}, {Lexing.pos_cnum: c2}) => {
  pos_cnum == c2
};

let locForLocations = (~extra, location: Location.t) => {
  /* maybeLog("looking for " ++ Utils.showLocation(location)); */
  extra.locations |> Utils.find(((loc: Location.t, l)) => {
    /* maybeLog("  > checking " ++ Utils.showLocation(loc));
    maybeLog("    " ++ (switch l {
      | Loc.Typed(_) => "typed"
      | Constant(_) => "constant"
      | Module(_) => "module"
      | TopLevelModule(_) => "toplevelmodule"
      | TypeDefinition(_) => "Typedefinition"
      | Explanation(_) => "explanation"
      | Open => "open"
    })); */
    posMatch(location.loc_start, loc.loc_start) && posMatch(location.loc_end, loc.loc_end) ? Some(l) : None
  });
};

/** Other locations *within this file* that refer to the same thing.
 *
 * Useful for "highlight" stuff. */
let localReferencesForLoc = (~file, ~extra, loc) =>
  switch (loc) {
  | Loc.Explanation(_)
  | Typed(_, NotFound)
  | Module(NotFound)
  | TopLevelModule(_)
  | Constant(_)
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
  | Constant(_)
  | Open => None
  | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip))
  | Module(LocalReference(stamp, tip) | Definition(stamp, tip)) =>
    inner(~file, stamp, tip)
  | TypeDefinition(_, _, stamp) => inner(~file, stamp, Type)
  | Module(GlobalReference(moduleName, path, tip))
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    {
      maybeLog("Getting global " ++ moduleName);
      let%try file = getModule(moduleName) |> RResult.orError("Cannot get module " ++ moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%try (env, name) = Query.resolvePath(~env, ~path, ~getModule) |> RResult.orError("Cannot resolve path " ++ pathToString(path));
      let%try stamp = Query.exportedForTip(~env, name, tip) |> RResult.orError("Exported not found for tip " ++ name ++ " > " ++ tipToString(tip));
      maybeLog("Getting for " ++ string_of_int(stamp) ++ " in " ++ name);
      let%try res = inner(~file=env.file, stamp, tip) |> RResult.orError("could not get defined");
      maybeLog("Yes!! got it");
      Ok(res)
    } |> RResult.toOptionAndLog
    /* let%try extra = getExtra(moduleName) |> RResult.orError("Failed to get extra for " ++ env.file.uri); */
    /* maybeLog("Finding references for (global) " ++ file.uri ++ " and stamp " ++ string_of_int(stamp) ++ " and tip " ++ tipToString(tip)); */
    /* forLocalStamp(~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) |> RResult.orError("Could not get for local stamp") */
  };
};

let alternateDeclared = (~file, ~pathsForModule, ~getUri, declared, tip) => {
  let%opt paths = Utils.maybeHash(pathsForModule, file.moduleName);
  maybeLog("paths for " ++ file.moduleName);
  switch (paths) {
  | SharedTypes.IntfAndImpl(_, Some(intf), _, Some(impl)) =>
    maybeLog("Have both!!");
    let intf = Utils.toUri(intf);
    let impl = Utils.toUri(impl);
    if (intf == file.uri) {
      let%opt (file, extra) = getUri(impl) |> RResult.toOptionAndLog;
      let%opt declared =
        Query.declaredForExportedTip(~stamps=file.stamps, ~exported=file.contents.exported, declared.name.txt, tip);
      Some((file, extra, declared));
    } else {
      let%opt (file, extra) = getUri(intf) |> RResult.toOptionAndLog;
      let%opt declared =
        Query.declaredForExportedTip(~stamps=file.stamps, ~exported=file.contents.exported, declared.name.txt, tip);
      Some((file, extra, declared));
    };
  | _ => None
  };
};

let resolveModuleReference = (~file, ~getModule, declared: SharedTypes.declared(SharedTypes.Module.kind)) => {
  switch (declared.contents) {
    | SharedTypes.Module.Structure(_) =>
      Some((file, Some(declared)))
    | Ident(path) =>
      let env = {Query.file, exported: file.contents.exported};
      switch (Query.fromCompilerPath(~env, path)) {
        | `Not_found => None
        | `Exported(env, name) =>
          let%opt stamp = Query.hashFind(env.exported.modules, name);
          let%opt md = Query.hashFind(env.file.stamps.modules, stamp);
          Some((env.file, Some(md)))
          /* Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc))) */
        | `Global(moduleName, path) =>
          let%opt file = getModule(moduleName);
          let env = {file, Query.exported: file.contents.exported};
          let%opt (env, name) = Query.resolvePath(~env, ~getModule, ~path);
          let%opt stamp = Query.hashFind(env.exported.modules, name);
          let%opt md = Query.hashFind(env.file.stamps.modules, stamp);
          Some((env.file, Some(md)))
          /* Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc))) */
        | `Stamp(stamp) =>
          let%opt md = Query.hashFind(file.stamps.modules, stamp);
          Some((file, Some(md)))
          /* Some((file.uri, validateLoc(md.name.loc, md.extentLoc))) */
        | `GlobalMod(name) =>
          let%opt file = getModule(name);
          /* maybeLog("Congrats, found a global mod"); */
          Some((file, None))
        | _ => None
      };
  }
};

let forLocalStamp = (~pathsForModule, ~file, ~extra, ~allModules, ~getModule, ~getUri, ~getExtra, stamp, tip) => {
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
      /**
      if this file has a corresponding interface or implementation file
      also find the references in that file.
       */
      let alternativeReferences = {
        let%opt (file, extra, {stamp}) = alternateDeclared(~pathsForModule, ~file, ~getUri, declared, tip);
        let%opt localStamp = switch tip {
          | Constructor(name) => Query.getConstructor(file, stamp, name) |?>> x => x.stamp
          | Attribute(name) => Query.getAttribute(file, stamp, name) |?>> x => x.stamp
          | _ => Some(stamp)
        };
        let%opt local = extra.internalReferences |. Query.hashFind(localStamp);
        Some([(file.uri, local)])
      } |? [];

      let%opt path = pathFromVisibility(declared.modulePath, declared.name.txt);
      maybeLog("Now checking path " ++ pathToString(path));
      let thisModuleName = file.moduleName;
      let externals = allModules |. Belt.List.keep(name => name != file.moduleName) |. Belt.List.keepMap(name => {
        let%try file = getModule(name) |> RResult.orError("Could not get file for module " ++ name);
        let%try extra = getExtra(name) |> RResult.orError("Could not get extra for module " ++ name);
        let%try refs = extra.externalReferences |. Query.hashFind(thisModuleName) |> RResult.orError("No references in " ++ name ++ " for " ++ thisModuleName);
        let refs = refs |. Belt.List.keepMap(((p, t, l)) => p == path && t == tip ? Some(l) : None);
        Ok((file.uri, refs))
      } |> RResult.toOptionAndLog);
      Some(alternativeReferences @ externals)
    } else {
      maybeLog("Not visible");
      Some([])
    }
  } |? [];
  Some([(file.uri, local), ...externals])
};

let allReferencesForLoc = (~pathsForModule, ~getUri, ~file, ~extra, ~allModules, ~getModule, ~getExtra, loc) => {
  switch (loc) {
    | Loc.Explanation(_)
    | Typed(_, NotFound)
    | Module(NotFound)
    | TopLevelModule(_)
    | Constant(_)
    | Open => RResult.Error("Not a valid loc")
    | TypeDefinition(_, _, stamp) => {
      forLocalStamp(~pathsForModule, ~getUri, ~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, Type) |> RResult.orError("Could not get for local stamp")
    }
    | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip))
    | Module(LocalReference(stamp, tip) | Definition(stamp, tip))
    => {
      maybeLog("Finding references for " ++ file.uri ++ " and stamp " ++ string_of_int(stamp) ++ " and tip " ++ tipToString(tip));
      forLocalStamp(~pathsForModule, ~getUri, ~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) |> RResult.orError("Could not get for local stamp")
    }
    | Module(GlobalReference(moduleName, path, tip))
    | Typed(_, GlobalReference(moduleName, path, tip)) => {
      let%try file = getModule(moduleName) |> RResult.orError("Cannot get module " ++ moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%try (env, name) = Query.resolvePath(~env, ~path, ~getModule) |> RResult.orError("Cannot resolve path " ++ pathToString(path));
      let%try stamp = Query.exportedForTip(~env, name, tip) |> RResult.orError("Exported not found for tip " ++ name ++ " > " ++ tipToString(tip));
      let%try (file, extra) = getUri(env.file.uri);
      maybeLog("Finding references for (global) " ++ env.file.uri ++ " and stamp " ++ string_of_int(stamp) ++ " and tip " ++ tipToString(tip));
      forLocalStamp(~pathsForModule, ~getUri, ~file, ~extra, ~allModules, ~getModule, ~getExtra, stamp, tip) |> RResult.orError("Could not get for local stamp")
    }
  }
};

let forPos = (~file, ~extra, pos) => {
  let%opt (_, loc) = locForPos(~extra, pos);
  maybeLog("Got a loc for pos");
  let%opt refs = localReferencesForLoc(~file, ~extra, loc);
  Some(refs)
};

let validateLoc = (loc: Location.t, backup: Location.t) =>
  if (loc.loc_start.pos_cnum == (-1)) {
    if (backup.loc_start.pos_cnum == (-1)) {
      {
        Location.loc_ghost: true,
        loc_start: {
          pos_cnum: 0,
          pos_lnum: 1,
          pos_bol: 0,
          pos_fname: "",
        },
        loc_end: {
          pos_cnum: 0,
          pos_lnum: 1,
          pos_bol: 0,
          pos_fname: "",
        },
      };
    } else {
      backup;
    };
  } else {
    loc;
  };

let resolveModuleDefinition = (~file, ~getModule, stamp) => {
  let%opt md = Query.hashFind(file.stamps.modules, stamp);
  let%opt (file, declared) = resolveModuleReference(~file, ~getModule, md);
  let loc = switch declared {
    | None => Utils.topLoc(file.uri)
    | Some(declared) => validateLoc(declared.name.loc, declared.extentLoc)
  };
  Some((file.uri, loc))
};

let definition = (~file, ~getModule, stamp, tip) => {
  switch tip {
    | Constructor(name) =>
      let%opt constructor = Query.getConstructor(file, stamp, name);
      Some((file.uri, constructor.name.loc))
    | Attribute(name) =>
      let%opt attribute = Query.getAttribute(file, stamp, name);
      Some((file.uri, attribute.name.loc))
    | Module => resolveModuleDefinition(~file, ~getModule, stamp)
    | _ =>
      let%opt declared = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
      let loc = validateLoc(declared.name.loc, declared.extentLoc);
      let env = Query.fileEnv(file);
      let uri = Query.getSourceUri(~env, ~getModule, declared.modulePath);
      maybeLog("Inner uri " ++ uri);
      Some((uri, loc))
  };
};

let orLog = (message, v) => switch v {
  | None => maybeLog(message); None
  | _ => v
};

let definitionForLoc = (~pathsForModule, ~file, ~getUri, ~getModule, loc) => {
  switch (loc) {
    | Loc.Typed(_, Definition(stamp, tip)) => {
      maybeLog("Trying to find a defintion for a definition");
      let%opt declared = Query.declaredForTip(~stamps=file.stamps, stamp, tip);
      maybeLog("Declared");
      if (declared.exported) {
        maybeLog("exported, looking for alternate " ++ file.moduleName);
        let%opt (file, _extra, declared) = alternateDeclared(~pathsForModule, ~file, ~getUri, declared, tip);
        let loc = validateLoc(declared.name.loc, declared.extentLoc);
        Some((file.uri, loc))
      } else {
        None
      }
    }
    | Loc.Explanation(_)
    | Typed(_, NotFound)
    | Module(NotFound | Definition(_, _))
    | TypeDefinition(_, _, _)
    | Constant(_)
    | Open => None
    | TopLevelModule(name) =>
      maybeLog("Toplevel " ++ name);
      open Infix;
      let%opt src = Utils.maybeHash(pathsForModule, name) |> orLog("No paths found") |?> SharedTypes.getSrc |> orLog("No src found");
      Some((Utils.toUri(src), Utils.topLoc(src)))
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

let definitionForPos = (~pathsForModule, ~file, ~extra, ~getUri, ~getModule, pos) => {
  let%opt (_, loc) = locForPos(~extra, pos);
  maybeLog("Got a loc for pos");
  definitionForLoc(~pathsForModule, ~file, ~getUri, ~getModule, loc)
};
