
open Analyze;
open DigTypes;

let getType = (~env: Query.queryEnv, name) => {
  open Infix;
  Query.hashFind(env.exported.types, name) |?> stamp => Query.hashFind(env.file.stamps.types, stamp)
};

let isBuiltin = fun 
  | "list" | "string" | "option" | "int" | "float" | "bool" | "array" => true
  | _ => false;
let rec getFullType_ = (env, path, name) => switch path {
  | [] => getType(~env, name)
  | [one, ...more] => 
    let%opt modStamp = Query.hashFind(env.exported.modules, one);
    let%opt declared = Query.hashFind(env.file.stamps.modules, modStamp);
    switch (declared.contents) {
      | Ident(_) => None
      | Structure(contents) =>
        getFullType_({...env, exported: contents.exported}, more, name)
    }
};

let getFullType = (~env: Query.queryEnv, path, name) => {
  getFullType_(env, path, name)
};


let mapSource = (~env, ~getModule, path) => {
    let resolved = Query.resolveFromCompilerPath(~env, ~getModule, path);
    open Infix;
    let declared =
      switch (resolved) {
      | `Not_found =>
        print_endline("Unresolved " ++ Path.name(path) ++ " in " ++ env.file.uri);
        None
      | `Stamp(stamp) =>
        switch (Query.hashFind(env.file.stamps.types, stamp)) {
          | None =>
            switch (Path.name(path)) {
              | "list" | "string" | "option" | "int" | "float" | "bool" | "array" => None
              | _ =>
              print_endline("Bad stamp " ++ Path.name(path) ++ " in " ++ env.file.uri ++ " :: " ++ string_of_int(stamp))
              None
            }
          | Some(decl) => Some((decl, env))
        }
         /* |?>> (d => (d, env.file)) */
      | `Exported(env, name) => getType(~env, name) |?>> (d => (d, env))
      };
    switch (declared) {
    | None => switch path {
      | Path.Pident(ident) when isBuiltin(Ident.name(ident)) => Builtin(Ident.name(ident))
      | _ => {
        print_endline("!!! Not found " ++ Path.name(path));
        NotFound
      }
    }
    | Some(({contents, name, modulePath} as declared, env)) =>
      let%opt_force ((uri, moduleName), path) = Query.showVisibilityPath(~env, ~getModule, modulePath);
      Public({
        uri,
        moduleName,
        declared,
        modulePath: path,
        name: name.txt,
        env,
      })
    };

  };


let recursiveMapSource = (~env, ~getModule, loop, path) => {
  let result = mapSource(~env, ~getModule, path);
  switch result {
    | Public({
      uri,
      moduleName,
      modulePath: path,
      declared,
      name,
      env,
    }) =>
      loop(~env, (moduleName, path, name), declared)
    | _ => ()
  };

  result
};

let rec digType = (~tbl, ~set, ~state, ~package, ~env, ~getModule, key, t: SharedTypes.declared(SharedTypes.Type.t)) => {
  if (!Hashtbl.mem(set, key)) {
    let loop = digType(~tbl, ~set, ~state, ~package, ~getModule);
    Hashtbl.replace(set, key, ());
    Hashtbl.replace(tbl, key,
        (
          t.contents.typ.migrateAttributes(),
      SharedTypes.SimpleType.declMapSource(
        recursiveMapSource(~env, ~getModule, loop),
          t.contents.typ.asSimpleDeclaration(t.name.txt),
        )
      )
    )
  };
};

let rec splitFull = fullName => {
  let parts = Util.Utils.split_on_char('.', fullName);
  let rec loop = parts => switch parts {
    | [] => assert(false)
    | [one] => ([], one)
    | [one, ...more] =>
      let (path, last) = loop(more);
      ([one, ...path], last)
  };
  loop(parts)
};

let fileToReference = (~state, uri, fullName) => {
  let%try package = State.getPackage(~reportDiagnostics=(_, _) => (), uri, state);
  let%try (file, _) = State.fileForUri(state, ~package, uri);
  let env = Query.fileEnv(file);
  let (path, name) = splitFull(fullName);
  let%try declared = getFullType(~env, path, name) |> RResult.orError("No declared type named " ++ fullName);
  let getModuleName = uri => {
    let%opt path = Utils.parseUri(uri);
    package.nameForPath->Query.hashFind(path);
  };
  let%opt_force moduleName = getModuleName(uri);
  Ok((moduleName, path, name))
};

let forInitialType = (~tbl, ~state, uri, fullName) => {
  let%try package = State.getPackage(~reportDiagnostics=(_, _) => (), uri, state);
  /* print_endline("Got package..."); */
  let%try (file, _) = State.fileForUri(state, ~package, uri);
  let env = Query.fileEnv(file);
  let (path, name) = splitFull(fullName);
  let%try declared = getFullType(~env, path, name) |> RResult.orError("No declared type named " ++ fullName);
  /* let tbl = Hashtbl.create(10); */
  let getModule = State.fileForModule(state, ~package);
  let getModuleName = uri => {
    let%opt path = Utils.parseUri(uri);
    package.nameForPath->Query.hashFind(path);
  };
  let%opt_force moduleName = getModuleName(uri);
  let set = Hashtbl.create(10);
  Hashtbl.iter((k, _) => Hashtbl.replace(set, k, ()), tbl);
  ignore(
    digType(
      ~tbl,
      ~set,
      ~state,
      ~package,
      ~env,
      ~getModule,
      (moduleName, path, name),
      declared,
    ),
  );

  Ok((moduleName, path, name));
};

let toSimpleMap = (tbl) => {
  let ntbl = Hashtbl.create(10);
  Hashtbl.iter((key, (attributes, value)) => {
    Hashtbl.replace(ntbl, key, (attributes, SharedTypes.SimpleType.declMapSource(DigTypes.toShortSource, value)))
  }, tbl);
  ntbl
};
