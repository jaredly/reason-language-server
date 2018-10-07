
Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;
open TypeMap.DigTypes;

let deserializers = tbl => {
  let decls =
    Hashtbl.fold(
      ((moduleName, modulePath, name), decl, bindings) => [
        BsJson.declDeserializer(~moduleName, ~modulePath, ~name, decl),
        ...bindings,
      ],
      tbl,
      [],
    );

  Ast_helper.Str.value(Recursive, decls);
};

let serializers = tbl => {
  let decls =
    Hashtbl.fold(
      ((moduleName, modulePath, name), decl, bindings) => [
        BsJson.declSerializer(~moduleName, ~modulePath, ~name, decl),
        ...bindings,
      ],
      tbl,
      [],
    );

  Ast_helper.Str.value(Recursive, decls);
};

let getTypeMap = (base, state, types) => {
  let tbl = Hashtbl.create(10);

  types->Belt.List.forEach(typ => {
    switch (Utils.split_on_char(':', typ)) {
      | [path, name] =>
        let%try_force () = TypeMap.GetTypeMap.forInitialType(~tbl, ~state, Utils.toUri(Filename.concat(base, path)), name);
      | _ => failwith("Expected /some/path.re:typename")
    }
  });
  tbl
};

let toBoth = (base, dest, types) => {
  let state = TopTypes.forRootPath(base);
  let tbl = getTypeMap(base, state, types);

  Pprintast.structure(Format.str_formatter, [deserializers(tbl), serializers(tbl)]);

  let ml = Format.flush_str_formatter();
  Files.writeFile(dest, ml) |> ignore;
  Ok();
};

switch (Sys.argv->Belt.List.fromArray) {
  | [_, "main"] => ()
  | [_, dest, ...items] => {
    switch (toBoth(Sys.getcwd(), dest, items)) {
      | RResult.Ok(()) => print_endline("Success")
      | RResult.Error(message) => print_endline("Failed: " ++ message)
    }
  }
  | _ => failwith("Bad args")
}