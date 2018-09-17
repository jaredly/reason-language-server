
open Lib;
module Json = Vendor.Json;
/* Log.spamError := true; */

let getType = (~stamps: SharedTypes.stamps, ~exported: SharedTypes.Module.exported, name) => {
  open Infix;
  Query.hashFind(exported.types, name) |?> stamp => Query.hashFind(stamps.types, stamp)
};

let showType = (loop, typ: SharedTypes.Type.t) => {
  switch (typ.kind) {
    | Abstract => Json.String("[abstract]")
    | Open => Json.String("[open]")
    | Record(items) =>
      open Rpc.J;
      o([
        ("kind", s("record")),
        ("params", l(typ.SharedTypes.Type.params |> List.map(((param, _)) => {
          s(param.SharedTypes.toString())
        }))),
        ("items", l(items->Belt.List.map(({typ, name: {txt}}) => {
          o([
            ("name", s(txt)),
            ("type", switch (typ.getConstructorPath()) {
              | None => s(typ.toString())
              | Some((path, args)) => s("[path] " ++ Path.name(path) ++ " " ++ (
                args->Belt.List.map(arg => arg.toString())
                |> String.concat(", ")
              ))
            })
          ])
        })))
      ])
    | Variant(items) =>
      open Rpc.J;
      o([
        ("kind", s("variant"))
      ])
  };
};

let rec digType = (~tbl, ~state, ~package, filename, name, t) => {
  if (!Hashtbl.mem(tbl, (filename, name))) {
    Hashtbl.replace(tbl, (filename, name), Json.Null);
    Hashtbl.replace(tbl, (filename, name), showType(digType(~tbl, ~state, ~package), t))
  }
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
  let modname = FindFiles.getName(src);
  let%try ({stamps, contents: {exported}}, _) = State.fileForUri(state, ~package, uri);
  let%try declared = getType(~stamps, ~exported, name) |> Result.orError("No declared type named " ++ name);
  let tbl = Hashtbl.create(10);
  digType(~tbl, ~state, ~package, full, name, declared.contents);
  tbl |> Hashtbl.iter((k, v) => {
    print_endline(Json.stringifyPretty(v))
  })
  print_endline("Hello folks");
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