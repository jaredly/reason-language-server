
open Lib;
module Json = Vendor.Json;
/* Log.spamError := true; */

type reference = {
  uri: string,
  modulePath: list(string),
  name: string,
};

type typeSource =
  | Builtin(string)
  | Public(reference)
  | NotFound;

let sourceToJson = source => Rpc.J.(switch source {
  | NotFound => s("NotFound")
  | Public({uri, modulePath, name}) => o([
    ("uri", s(uri)),
    ("modulePath", l(modulePath->Belt.List.map(s))),
    ("name", s(name)),
  ])
  | Builtin(name) => o([("builtin", s(name))])
});

let getType = (~env: Query.queryEnv, name) => {
  open Infix;
  Query.hashFind(env.exported.types, name) |?> stamp => Query.hashFind(env.file.stamps.types, stamp)
};

let mapSource = (~env, ~getModule, digType, path) => {
    let resolved = Query.resolveFromCompilerPath(~env, ~getModule, path);
    open Infix;
    let declared =
      switch (resolved) {
      | `Not_found => None
      | `Stamp(stamp) => Query.hashFind(env.file.stamps.types, stamp) |?>> (d => (d, env.file))
      | `Exported(env, name) => getType(~env, name) |?>> (d => (d, env.file))
      };
    switch (declared) {
    | None => switch path {
      | Path.Pident({name: ("list" | "string" | "option" | "int" | "float" | "bool") as name}) => Builtin(name)
      | _ => {
        print_endline("!!! Not found " ++ Path.name(path));
        NotFound
      }
    }
    | Some(({contents, name, modulePath} as declared, file)) =>
      let%opt_force (uri, path) = SharedTypes.showVisibilityPath(modulePath);
      digType(uri, String.concat(".", path) ++ ":" ++ name.txt, declared);
      Public({
        uri,
        modulePath: path,
        name: name.txt,
      })
    };

  };

let processExpr = (~env, ~getModule, digType, expr) => {
  let expr = SharedTypes.SimpleType.mapSource(mapSource(~env, ~getModule, digType), expr);
  SerializeSimplerType.toJson(sourceToJson, expr)
};

let processDecl = (~env, ~getModule, digType, {name, contents}: SharedTypes.declared(SharedTypes.Type.t)) => {
  let decl = contents.typ.asSimpleDeclaration(name.txt)
  SerializeSimplerType.declToJson(sourceToJson, SharedTypes.SimpleType.declMapSource(
    mapSource(~env, ~getModule, digType),
    decl
  ))
};

let rec digType = (~tbl, ~state, ~package, ~env, ~getModule, filename, name, t) => {
  if (!Hashtbl.mem(tbl, (filename, name))) {
    Hashtbl.replace(tbl, (filename, name), Json.String("Nope"));
    /* Hashtbl.replace(tbl, (filename, name),
      showType(~env, ~getModule, digType(~tbl, ~state, ~package, ~env, ~getModule), t)
    ) */
    Hashtbl.replace(tbl, (filename, name),
      processDecl(~env, ~getModule, digType(~tbl, ~state, ~package, ~env, ~getModule), t)
    )
  };
};

let main = (base, src, name) => {
  let state = TopTypes.{
    ...TopTypes.empty(),
    rootPath: base,
    rootUri: "file://" ++ base,
  };

  let full = Filename.concat(base, src);
  let uri = Utils.toUri(full);
  print_endline("Uri: " ++ uri);
  let%try package = State.getPackage(uri, state);
  print_endline("Got package...")
  /* print_endline(State.Show.state(state, package)) */
  let%try modname = package.nameForPath->Query.hashFind(full) |> Result.orError("No modname found for " ++ full);
  let%try (file, _) = State.fileForUri(state, ~package, uri);
  let env = Query.fileEnv(file);
  let%try declared = getType(~env, name) |> Result.orError("No declared type named " ++ name);
  let tbl = Hashtbl.create(10);
  let getModule = State.fileForModule(state, ~package);
  ignore(digType(~tbl, ~state, ~package, ~env, ~getModule, full, name, declared));

  let json = Rpc.J.o(Hashtbl.fold(((filename, name), v, items) => {
    [(filename ++ ":" ++ name, v), ...items]
  }, tbl, []));
  /* print_endline(Json.stringifyPretty(json)); */
  Ok(())
};

switch (Sys.argv) {
  | [|_, src, name|] => {
    switch (main(Sys.getcwd(), src, name)) {
      | Result.Ok(()) => print_endline("Success")
      | Result.Error(message) => print_endline("Failed: " ++ message)
    }
  }
  | _ => failwith("Bad args")
}