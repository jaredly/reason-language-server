
Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;
let loc = Location.none;

let makeModule = (moduleName, contents) =>
  Ast_helper.Str.module_(
    Ast_helper.Mb.mk(Location.mknoloc(moduleName), Ast_helper.Mod.mk(Parsetree.Pmod_structure(contents))),
  );

let hashList = tbl => Hashtbl.fold((key, value, result) => [(key, value), ...result], tbl, []);

let lockTypes = (version, tbl) => {
  let decls =
  hashList(tbl)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name), decl)) => {
    Serde.OutputType.outputDeclaration(moduleName, modulePath, name, (source, args) =>  {
      Ast_helper.Typ.constr(Location.mknoloc(switch source {
        | TypeMap.DigTypes.NotFound => failwith("Not found type reference")
        | Builtin(name) => Longident.Lident(name)
        | Public((moduleName, modulePath, name)) =>
          Longident.Lident(Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name))
          /* TypeMap.DigTypes.referenceToLident(reference) */
      }), args)
    }, decl)
  });
  makeModule("V" ++ string_of_int(version) ++ "_Locked", [Ast_helper.Str.type_(Recursive, decls)])
};

let makeFns = (moduleName, maker, tbl) => {
  let decls =
    hashList(tbl)
    ->Belt.List.sort(compare)
    ->Belt.List.map((((moduleName, modulePath, name), decl)) => 
        maker(~moduleName, ~modulePath, ~name, decl)
    );

    makeModule(moduleName, [Ast_helper.Str.value(Recursive, decls)])
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

let main = configPath => {
  let config = Json.parse(Util.Files.readFileExn(configPath));
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
    Hashtbl.replace(
      tbl,
      (
        modname,
        path
        |> List.map(item => {
             let%opt_force item = Json.string(item);
             item;
           }),
        name,
      ),
      SharedTypes.SimpleType.{
        name,
        variables: {
          let rec loop = n =>
            n <= 0 ? [] : [SharedTypes.SimpleType.Variable("arg" ++ string_of_int(args - n)), ...loop(n - 1)];
          loop(args);
        },
        body: Abstract,
      },
    );
  });

  /* Digest.string */

  entries->Belt.List.forEach(typ => {
    let%opt_force file = Json.get("file", typ) |?> Json.string;
    let%opt_force typeName = Json.get("type", typ) |?> Json.string;
    let%try_force () =
      TypeMap.GetTypeMap.forInitialType(~tbl, ~state, Utils.toUri(Filename.concat(Sys.getcwd(), file)), typeName);
    ();
  });

  let simpleTbl = TypeMap.GetTypeMap.toSimpleMap(tbl);

  /* tbl */
  let body =
    switch (engine) {
    | "bs-json" => [
        makeFns("DeserializeRaw", Serde.BsJson.declDeserializer, tbl),
        makeFns("SerializeRaw", Serde.BsJson.declSerializer, tbl),
      ]
    | "rex-json" => [
        makeFns("DeserializeRaw", Serde.Json.declDeserializer, tbl),
        makeFns("SerializeRaw", Serde.Json.declSerializer, tbl),
      ]
    | _ => assert(false)
    };
  let body = [lockTypes(1, simpleTbl), ...body];

  Pprintast.structure(Format.str_formatter, body @ [%str include SerializeRaw; include DeserializeRaw]);

  let ml = Format.flush_str_formatter();
  Files.writeFile(output, ml) |> ignore;
};

switch (Sys.argv->Belt.List.fromArray) {
  | [_, config] => main(config)
  | _ => failwith("Bad args")
}
