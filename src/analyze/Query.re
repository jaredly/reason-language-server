
open SharedTypes;

/* TODO maybe keep track of the "current module path" */
type queryEnv = {
  uri: string,
  stamps,
  exported: Module.exported,
};

let hashFind = (tbl, key) => switch (Hashtbl.find(tbl, key)) {
  | exception Not_found => None
  | result => Some(result)
};

let rec joinPaths = (modulePath, path) => {
  switch modulePath {
    | Path.Pident({stamp, name}) => (stamp, name, path)
    | Path.Papply(fnPath, _argPath) => joinPaths(fnPath, path)
    | Path.Pdot(inner, name, _) => joinPaths(inner, Nested(name, path))
  }
};

let rec resolvePathInner = (~env, ~path) => {
  switch path {
    | Nested(subName, subPath) => {
      let%opt stamp = hashFind(env.exported.modules, subName);
      let%opt {contents: {kind}} = hashFind(env.stamps.modules, stamp);
      findInModule(~env, kind, subPath)
    }
    | Tip(name, tip) => Some(`Local(env, name, tip))
  }
} and findInModule = (~env, kind, path) => {
  switch kind {
  | Module.Structure({exported}) => resolvePathInner(~env, ~path)
  | Ident(modulePath, _ident) =>
    let (stamp, moduleName, fullPath) = joinPaths(modulePath, path);
    if (stamp == 0) {
      Some(`Global(moduleName, fullPath))
    } else {
      let%opt {contents: {kind}} = hashFind(env.stamps.modules, stamp);
      findInModule(~env, kind, fullPath);
    }
  }
};

let rec resolvePath = (~env, ~path, ~getModule) => {
  let%opt result = resolvePathInner(~env, ~path);
  switch result {
    | `Local(env, name, tip) => Some((env, name, tip))
    | `Global(moduleName, fullPath) => {
      let%opt (uri, stamps, exported) = getModule(moduleName);
      resolvePath(~env={stamps, exported, uri}, ~path=fullPath, ~getModule)
    }
  }
};
