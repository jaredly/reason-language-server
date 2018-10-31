
open Analyze;
open DigTypes;

let getType = (~env: Query.queryEnv, name) => {
  open Infix;
  Query.hashFind(env.exported.types, name) |?> stamp => Query.hashFind(env.file.stamps.types, stamp)
};

let isBuiltin = fun 
  | "list" | "string" | "option" | "int" | "float" | "bool" | "array" => true
  | _ => false;

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
      SharedTypes.SimpleType.declMapSource(
        recursiveMapSource(~env, ~getModule, loop),
        t.contents.typ.asSimpleDeclaration(t.name.txt)
      )
    )
  };
};

let forInitialType = (~tbl, ~state, uri, name) => {
  let%try package = State.getPackage(~reportDiagnostics=(_, _) => (), uri, state);
  print_endline("Got package...");
  let%try (file, _) = State.fileForUri(state, ~package, uri);
  let env = Query.fileEnv(file);
  let%try declared = getType(~env, name) |> RResult.orError("No declared type named " ++ name);
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
      (moduleName, [], name),
      declared,
    ),
  );

  Ok();
};
