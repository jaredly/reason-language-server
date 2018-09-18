
open Lib;
module Json = Vendor.Json;
/* Log.spamError := true; */

let getType = (~env: Query.queryEnv, name) => {
  open Infix;
  Query.hashFind(env.exported.types, name) |?> stamp => Query.hashFind(env.file.stamps.types, stamp)
};

let rec showConstructor = (~env, ~getModule, loop, path, args) => {
  open Rpc.J;
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
    | Path.Pident({name: ("list" | "string" | "option" | "int" | "float" | "bool") as name}) => o([
      ("builtin", s(name)),
      ("args", l(args |> List.map(showFlexible(~env, ~getModule, loop)))),
    ])
    | _ => o([
      ("error", s("Not found")),
      ("path", s(Path.name(path))),
      ("args", l(args |> List.map(showFlexible(~env, ~getModule, loop)))),
    ])
  }
  | Some(({contents, name, modulePath} as declared, file)) =>
    let%opt_force (uri, path) = SharedTypes.showVisibilityPath(modulePath);
    loop(uri, String.concat(".", path) ++ ":" ++ name.txt, declared);
    o([
      ("uri", s(uri)),
      ("name", s(name.txt)),
      ("modPath", l(path |> List.map(s))),
      ("args", l(args |> List.map(showFlexible(~env, ~getModule, loop)))),
    ]);
  };
}
and showFlexible = (~env, ~getModule, loop, typ: SharedTypes.flexibleType) =>
  Rpc.J.(
    switch (typ.getConstructorPath()) {
    | None => s(typ.toString())
    | Some((path, args)) =>
      showConstructor(~env, ~getModule, loop, path, args)
    }
  );

let showType = (~env, ~getModule, loop, {modulePath, contents: typ}: SharedTypes.declared(SharedTypes.Type.t)) => {
  let%opt_force (uri, path) = SharedTypes.showVisibilityPath(modulePath);
  open Rpc.J;
  o([
    ("uri", s(uri)),
    ("modPath", l(path |> List.map(s))),
    ("params", Rpc.J.l(typ.params->Belt.List.map(((typ, _)) => showFlexible(~env, ~getModule, loop, typ)))),
    (
      "kind",
      switch (typ.kind) {
      | Open => Json.String("open")
      | Abstract(None) => Json.String("abstract")
      | Abstract(Some((path, args))) => showConstructor(~env, ~getModule, loop, path, args)
      | Tuple(items) => o([
        ("kind", s("tuple")),
        ("items", l(items->Belt.List.map(showFlexible(~env, ~getModule, loop))))
      ])
      | Record(items) =>
        Rpc.J.(
          o([
            ("kind", s("record")),
            (
              "items",
              l(
                items
                ->Belt.List.map(({typ, name: {txt}}) =>
                    o([("name", s(txt)), ("type", showFlexible(~env, ~getModule, loop, typ))])
                  ),
              ),
            ),
          ])
        )
      | Variant(items) => Rpc.J.(o([
        ("kind", s("variant")),
        ("constructors", l(
          items->Belt.List.map(({name: {txt}, args, res}) => {
            o([
              ("name", s(txt)),
              ("args", l(args->Belt.List.map(((arg, _)) => showFlexible(~env, ~getModule, loop, arg)))),
              ("res", switch res {
                | None => Json.Null
                | Some(t) => showFlexible(~env, ~getModule, loop, t)
              })
            ])
          })
        ))
      ]))
      },
    ),
  ]);
};

let rec digType = (~tbl, ~state, ~package, ~env, ~getModule, filename, name, t) => {
  if (!Hashtbl.mem(tbl, (filename, name))) {
    Hashtbl.replace(tbl, (filename, name), Json.String("Nope"));
    Hashtbl.replace(tbl, (filename, name), showType(~env, ~getModule, digType(~tbl, ~state, ~package, ~env, ~getModule), t))
  };
  /* Hashtbl.find(tbl, (filename, name)) */
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
  print_endline(Json.stringifyPretty(json));
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