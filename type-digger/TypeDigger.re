
Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;

let makeFns = (maker, tbl) => {
  let decls =
    Hashtbl.fold(
      ((moduleName, modulePath, name), decl, bindings) => [
        maker(~moduleName, ~modulePath, ~name, decl),
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

  Pprintast.structure(Format.str_formatter, [makeFns(Serde.BsJson.declDeserializer, tbl), makeFns(Serde.BsJson.declSerializer, tbl)]);

  let ml = Format.flush_str_formatter();
  Files.writeFile(dest, ml) |> ignore;
  Ok();
};

let toJson = (base, dest, types) => {
  let state = TopTypes.forRootPath(base);
  let tbl = getTypeMap(base, state, types);

  Pprintast.structure(Format.str_formatter, [makeFns(Serde.Json.declDeserializer, tbl), makeFns(Serde.Json.declSerializer, tbl)]);

  let ml = Format.flush_str_formatter();
  Files.writeFile(dest, ml) |> ignore;
  Ok();
};

switch (Sys.argv->Belt.List.fromArray) {
  | [_, "json", dest, ...items] => {
    switch (toJson(Sys.getcwd(), dest, items)) {
      | RResult.Ok(()) => print_endline("Success")
      | RResult.Error(message) => print_endline("Failed: " ++ message)
    }
  }
  | [_, dest, ...([_, ..._] as items)] => {
    switch (toBoth(Sys.getcwd(), dest, items)) {
      | RResult.Ok(()) => print_endline("Success")
      | RResult.Error(message) => print_endline("Failed: " ++ message)
    }
  }
  | [_, config] => {
    let config = Json.parse(Util.Files.readFileExn(config));
    open Util.RResult.InfixResult;
    let%try_force output = Util.RJson.get("output", config) |?> Util.RJson.string;
    let%try_force engine = Util.RJson.get("engine", config) |?> Util.RJson.string;
    let%try_force entries = Util.RJson.get("entries", config) |?> Util.RJson.array;
    open Util.Infix;
    let custom = Json.get("custom", config) |?> Json.array |? [];

    let state = TopTypes.forRootPath(Sys.getcwd());

    let tbl = Hashtbl.create(10);

    custom->Belt.List.forEach(custom => {
      open Util.RResult.InfixResult;
      let%try_force modname = Util.RJson.get("module", custom) |?> Util.RJson.string;
      let%try_force path = Util.RJson.get("path", custom) |?> Util.RJson.array;
      let%try_force name = Util.RJson.get("name", custom) |?> Util.RJson.string;
      open Util.Infix;
      let args = Json.get("args", custom) |?> Json.number |?>> int_of_float |? 0;
      Hashtbl.replace(tbl, (modname, path |> List.map(item => {
        let%opt_force item = Json.string(item);
        item
      }), name), SharedTypes.SimpleType.{
        name,
        variables: {
          let rec loop = n => n <= 0 ? [] : [SharedTypes.SimpleType.Variable("arg" ++ string_of_int(args - n)), ...loop(n - 1)];
          loop(args)
        },
        body: Abstract
      })
    });

    /* Digest.string */

    entries->Belt.List.forEach(typ => {
      let%opt_force file = Json.get("file", typ) |?> Json.string;
      let%opt_force typeName = Json.get("type", typ) |?> Json.string;
      let%try_force () = TypeMap.GetTypeMap.forInitialType(~tbl, ~state, Utils.toUri(Filename.concat(Sys.getcwd(), file)), typeName);
    });

    /* tbl */
    let body = switch engine {
      | "bs-json" => [makeFns(Serde.BsJson.declDeserializer, tbl), makeFns(Serde.BsJson.declSerializer, tbl)]
      | "rex-json" => [makeFns(Serde.Json.declDeserializer, tbl), makeFns(Serde.Json.declSerializer, tbl)]
      | _ => assert(false)
    };

    Pprintast.structure(Format.str_formatter, body);

    let ml = Format.flush_str_formatter();
    Files.writeFile(output, ml) |> ignore;

  }
  | _ => failwith("Bad args")
}
