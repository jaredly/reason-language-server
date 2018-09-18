
open Lib;
open DigTypes;

let getType = (~env: Query.queryEnv, name) => {
  open Infix;
  Query.hashFind(env.exported.types, name) |?> stamp => Query.hashFind(env.file.stamps.types, stamp)
};

let mapSource = (~env, ~getModule, ~getModuleName, digType, path) => {
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
              | "list" | "string" | "option" | "int" | "float" | "bool" => None
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
      | Path.Pident({name: ("list" | "string" | "option" | "int" | "float" | "bool") as name}) => Builtin(name)
      | _ => {
        print_endline("!!! Not found " ++ Path.name(path));
        NotFound
      }
    }
    | Some(({contents, name, modulePath} as declared, env)) =>
      let%opt_force (uri, path) = SharedTypes.showVisibilityPath(modulePath);
      let%opt_force moduleName = getModuleName(uri);
      /* let key = path @ [name.txt]; */
      digType(~env, (moduleName, path, name.txt), declared);
      Public({
        uri,
        moduleName,
        modulePath: path,
        name: name.txt,
      })
    };

  };

let rec digType = (~tbl, ~set, ~state, ~package, ~env, ~getModule, ~getModuleName, key, t: SharedTypes.declared(SharedTypes.Type.t)) => {
  if (!Hashtbl.mem(set, key)) {
    let loop = digType(~tbl, ~set, ~state, ~package, ~getModuleName, ~getModule);
    Hashtbl.replace(set, key, ());
    Hashtbl.replace(tbl, key,
      SharedTypes.SimpleType.declMapSource(
        mapSource(~env, ~getModule, ~getModuleName, loop),
        t.contents.typ.asSimpleDeclaration(t.name.txt)
      )
    )
  };
};

let forInitialType = (~state, uri, name) => {
  let%try package = State.getPackage(uri, state);
  print_endline("Got package...")
  let%try (file, _) = State.fileForUri(state, ~package, uri);
  let env = Query.fileEnv(file);
  let%try declared = getType(~env, name) |> Result.orError("No declared type named " ++ name);
  let tbl = Hashtbl.create(10);
  let getModule = State.fileForModule(state, ~package);
  let getModuleName = uri => {
    let%opt path = Utils.parseUri(uri);
    package.nameForPath -> Query.hashFind(path);
  };
  let%opt_force moduleName = getModuleName(uri);
  /* let moduleName = switch (getModuleName(uri)) {
    | None =>
      Hashtbl.iter((k, v) => {
        print_endline(k ++ " : " ++ v);
      }, package.nameForPath);
      failwith("No module name for "++ uri)
    | Some(m) => m
  }; */
  ignore(digType(~tbl, ~set=Hashtbl.create(10), ~state, ~package, ~env, ~getModule, ~getModuleName, (moduleName, [], name), declared));

  Ok(tbl)
};
