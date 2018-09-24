
open Lib;
module Json = Vendor.Json;

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
    | _ =>
    print_endline("Not found " ++ Path.name(path));
    o([
      ("error", s("Not found")),
      ("path", s(Path.name(path))),
      ("args", l(args |> List.map(showFlexible(~env, ~getModule, loop)))),
    ])
  }
  | Some(({contents, name, modulePath} as declared, file)) =>
    let%opt_force ((uri, moduleName), path) = SharedTypes.showVisibilityPath(modulePath);
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
  let%opt_force ((uri, moduleName), path) = SharedTypes.showVisibilityPath(modulePath);
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
