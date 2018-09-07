
open SharedTypes;

/* TODO maybe keep track of the "current module path" */
/* maybe add a "current module path" for debugging purposes */
type queryEnv = {
  file,
  exported: Module.exported,
};

let fileEnv = file => {file, exported: file.contents.exported};

let hashFind = (tbl, key) => switch (Hashtbl.find(tbl, key)) {
  | exception Not_found => None
  | result => Some(result)
};

let tupleOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => (pos_lnum - 1, pos_cnum - pos_bol);
let locationIsBefore = ({Location.loc_start}, pos) =>
  tupleOfLexing(loc_start) <= pos;

let findInScope = (pos, name, stamps) => {
  /* Log.log("Find " ++ name ++ " with " ++ string_of_int(Hashtbl.length(stamps)) ++ " stamps"); */
  Hashtbl.fold((_stamp, declared, result) => {
    if (declared.name.txt == name) {
      let (l, c) = pos;
      /* Log.log("a stamp " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ string_of_int(l) ++ "," ++ string_of_int(c)); */
      if (locationIsBefore(declared.scopeLoc, pos)) {
        switch result {
          | None => Some(declared)
          | Some(current) =>
            if (current.name.loc.loc_start.pos_cnum < declared.name.loc.loc_start.pos_cnum) {
              Some(declared)
            } else {
              result
            }
        }
      } else {
        result
      }
    } else {
      /* Log.log("wrong name " ++ declared.name.txt); */
      result
    }
  }, stamps, None)
};

let rec joinPaths = (modulePath, path) => {
  switch modulePath {
    | Path.Pident({stamp, name}) => (stamp, name, path)
    | Path.Papply(fnPath, _argPath) => joinPaths(fnPath, path)
    | Path.Pdot(inner, name, _) => joinPaths(inner, Nested(name, path))
  }
};

let rec makePath = (modulePath) => {
  switch modulePath {
    | Path.Pident({stamp: 0, name}) => `GlobalMod(name)
    | Path.Pident({stamp, name}) => `Stamp(stamp)
    | Path.Papply(fnPath, _argPath) => makePath(fnPath)
    | Path.Pdot(inner, name, _) => `Path(joinPaths(inner, Tip(name)))
  }
};

let rec makeRelativePath = (basePath, otherPath) => {
  let rec loop = (base, other, tip) => {
    if (Path.same(base, other)) {
      Some(tip)
    } else {
      switch other {
        | Pdot(inner, name, _) => loop(basePath, inner, Nested(name, tip))
        | _ => None
      }
    }
  };
  switch otherPath {
    | Path.Pdot(inner, name, _) => loop(basePath, inner, Tip(name))
    | _ => None
  }
};

let rec resolvePathInner = (~env, ~path) => {
  switch path {
    | Tip(name) => Some(`Local(env, name))
    | Nested(subName, subPath) => {
      let%opt stamp = hashFind(env.exported.modules, subName);
      let%opt {contents: kind} = hashFind(env.file.stamps.modules, stamp);
      findInModule(~env, kind, subPath)
    }
  }
} and findInModule = (~env, kind, path) => {
  switch kind {
  | Module.Structure({exported}) => resolvePathInner(~env={...env, exported}, ~path)
  | Ident(modulePath) =>
    let (stamp, moduleName, fullPath) = joinPaths(modulePath, path);
    if (stamp == 0) {
      Some(`Global(moduleName, fullPath))
    } else {
      let%opt {contents: kind} = hashFind(env.file.stamps.modules, stamp);
      findInModule(~env, kind, fullPath);
    }
  }
};

/* let rec findSubModule = (~env, ~getModule) */

let rec resolvePath = (~env, ~path, ~getModule) => {
  let%opt result = resolvePathInner(~env, ~path);
  switch result {
    | `Local(env, name) => Some((env, name))
    | `Global(moduleName, fullPath) => {
      let%opt file = getModule(moduleName);
      resolvePath(~env={file, exported: file.contents.exported}, ~path=fullPath, ~getModule)
    }
  }
};

let resolveFromStamps = (~env, ~path, ~getModule, ~pos) => {
  switch path {
    | Tip(name) => Some((env, name))
    | Nested(name, inner) =>
    /* Log.log("Finding from stamps " ++ name); */
      let%opt declared = findInScope(pos, name, env.file.stamps.modules);
      /* Log.log("found it"); */
      let%opt res = findInModule(~env, declared.contents, inner);
      switch res {
        | `Local((env, name)) => Some((env, name))
        | `Global(moduleName, fullPath) => {
          let%opt file = getModule(moduleName);
          resolvePath(~env={file, exported: file.contents.exported}, ~path=fullPath, ~getModule)
        }
      }
  }
};

open Infix;

let fromCompilerPath = (~env, path) => {
  switch (makePath(path)) {
    | `Stamp(stamp) => `Stamp(stamp)
    | `Path((0, moduleName, path)) => `Global(moduleName, path)
    | `GlobalMod(name) => `GlobalMod(name)
    | `Path((stamp, moduleName, path)) => {
      let res = {
        let%opt {contents: kind} = hashFind(env.file.stamps.modules, stamp);
        let%opt_wrap res = findInModule(~env, kind, path);
        switch res {
          | `Local(env, name) => `Exported(env, name)
          | `Global(moduleName, fullPath) => `Global(moduleName, fullPath)
        };
      };
      res |? `Not_found
    }
  };
};

let resolveFromCompilerPath = (~env, ~getModule, path) => {
  switch (fromCompilerPath(~env, path)) {
    | `Global(moduleName, path) => {
      let res = {
        let%opt file = getModule(moduleName);
        let env = {file, exported: file.contents.exported};
        let%opt_wrap (env, name) = resolvePath(~env, ~getModule, ~path);
        `Exported(env, name)
      };
      res |? `Not_found
    }
    | `Stamp(stamp) => `Stamp(stamp)
    | `GlobalMod(_) => `Not_found
    | `Not_found => `Not_found
    | `Exported(env, name) => `Exported(env, name)
  }
};

let declaredForExportedTip = (~stamps: SharedTypes.stamps, ~exported: SharedTypes.Module.exported, name, tip) => switch tip {
  | Value => hashFind(exported.values, name) |?> stamp => hashFind(stamps.values, stamp) |?>> x => {...x, contents: ()}
  | Attribute(_) | Constructor(_)
  | Type => hashFind(exported.types, name) |?> stamp => hashFind(stamps.types, stamp) |?>> x => {...x, contents: ()}
  | Module => hashFind(exported.modules, name) |?> stamp => hashFind(stamps.modules, stamp) |?>> x => {...x, contents: ()}
  | ModuleType => hashFind(exported.moduleTypes, name) |?> stamp => hashFind(stamps.moduleTypes, stamp) |?>> x => {...x, contents: ()}
};

let declaredForTip = (~stamps, stamp, tip) => switch tip {
  | Value => hashFind(stamps.values, stamp) |?>> x => {...x, contents: ()}
  | Attribute(_) | Constructor(_)
  | Type => hashFind(stamps.types, stamp) |?>> x => {...x, contents: ()}
  | Module => hashFind(stamps.modules, stamp) |?>> x => {...x, contents: ()}
  | ModuleType => hashFind(stamps.moduleTypes, stamp) |?>> x => {...x, contents: ()}
};

let getAttribute = (file, stamp, name) => {
  let%opt {contents: {kind}} = hashFind(file.stamps.types, stamp);
  switch (kind) {
    | Record(labels) => {
      let%opt label = labels |. Belt.List.getBy(label => label.name.txt == name);
      Some(label)
    }
    | _ => None
  }
};

let getConstructor = (file, stamp, name) => {
  let%opt {contents: {kind}} = hashFind(file.stamps.types, stamp);
  switch (kind) {
    | Variant(constructors) => {
      let%opt const = constructors |. Belt.List.getBy(const => const.name.txt == name);
      Some(const)
    }
    | _ => None
  }
};

let exportedForTip = (~env, name, tip) => switch tip {
  | Value => hashFind(env.exported.values, name)
  | Attribute(_) | Constructor(_)
  | Type => hashFind(env.exported.types, name)
  | Module => hashFind(env.exported.modules, name)
  | ModuleType => hashFind(env.exported.moduleTypes, name)
};

/** TODO move to the Process_ stuff */
let rec dig = (typ) =>
  switch typ.UnifiedTypes.desc {
  | UnifiedTypes.Tlink(inner) => dig(inner)
  | UnifiedTypes.Tsubst(inner) => dig(inner)
  | UnifiedTypes.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let digConstructor = (~env, ~getModule, expr) => {
  let expr = dig(expr);
  switch (expr.desc) {
  | Tconstr(path, _args, _memo) =>
    switch (resolveFromCompilerPath(~env, ~getModule, path)) {
    | `Not_found => None
    | `Stamp(stamp) =>
      let%opt t = hashFind(env.file.stamps.types, stamp);
      Some((env, t));
    | `Exported(env, name) =>
      let%opt stamp = hashFind(env.exported.types, name);
      let%opt t = hashFind(env.file.stamps.types, stamp);
      Some((env, t));
    | _ => None
    }
  | _ => None
  };
};