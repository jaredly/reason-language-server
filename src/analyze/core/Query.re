
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

let findInScope = (pos, name, stamps) => {
  Hashtbl.fold((_stamp, declared, result) => {
    if (declared.name.txt == name) {
      let (l, c) = pos;
      Log.log("a stamp " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ string_of_int(l) ++ "," ++ string_of_int(c));
      if (Protocol.locationIsBefore(declared.scopeLoc, pos)) {
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

open Infix;

let fromCompilerPath = (~env, path) => {
  switch (makePath(path)) {
    | `Stamp(stamp) => `Stamp(stamp)
    | `Path((0, moduleName, path)) => `Global(moduleName, path)
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
    | `Not_found => `Not_found
    | `Exported(env, name) => `Exported(env, name)
  }
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

let rec dig = (typ) =>
  switch typ.Types.desc {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | Types.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let digConstructor = (~env, ~getModule, expr) => {
  let expr = dig(expr);
  Log.log("digging");
  switch (expr.desc) {
    | Tconstr(path, _args, _memo) => {
      Log.log("dug");
      switch (resolveFromCompilerPath(~env, ~getModule, path)) {
      | `Not_found => None
      | `Stamp(stamp) =>
        Log.log("stamp");
        let%opt t = hashFind(env.file.stamps.types, stamp);
        Some((env, t))
      | `Exported(env, name) =>
        Log.log("exported " ++ name);
        let%opt stamp = hashFind(env.exported.types, name);
        let%opt t = hashFind(env.file.stamps.types, stamp);
        Some((env, t))
      | _ => None
      };
    }
    | _ => {
      Log.log("not a constructor: " ++ (PrintType.default.expr(PrintType.default, expr) |> PrintType.prettyString));
      None
    }
  }
};