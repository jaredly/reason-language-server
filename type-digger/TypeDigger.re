
Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;
let loc = Location.none;

let makeModule = (moduleName, contents) =>
  Ast_helper.Str.module_(
    Ast_helper.Mb.mk(Location.mknoloc(moduleName), Ast_helper.Mod.mk(Parsetree.Pmod_structure(contents))),
  );

let hashList = tbl => Hashtbl.fold((key, value, result) => [(key, value), ...result], tbl, []);

let lockTypes = (tbl) => {
  /* let decls = */
  hashList(tbl)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name), decl)) => {
    Serde.OutputType.outputDeclaration(moduleName, modulePath, name, (source, args) =>  {
      Ast_helper.Typ.constr(Location.mknoloc(switch source {
        | TypeMap.DigTypes.NotFound => failwith("Not found type reference")
        | Builtin(name) => Longident.Lident(name)
        | Public((moduleName, modulePath, name)) =>
          Longident.Lident(Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name))
      }), args)
    }, decl)
  });
  /* makeModule("V" ++ string_of_int(version) ++ "_Locked", [Ast_helper.Str.type_(Recursive, decls)]) */
};

let makeFns = (maker, tbl) => {
  /* let decls = */
    hashList(tbl)
    ->Belt.List.sort(compare)
    ->Belt.List.map((((moduleName, modulePath, name), decl)) => 
        maker(~moduleName, ~modulePath, ~name, decl)
    );

    /* makeModule(moduleName, [Ast_helper.Str.value(Recursive, decls)]) */
};

/* let getTypeMap = (base, state, types) => {
  let tbl = Hashtbl.create(10);

  types->Belt.List.forEach(typ => {
    switch (Utils.split_on_char(':', typ)) {
      | [path, name] =>
        let%try_force () = TypeMap.GetTypeMap.forInitialType(~tbl, ~state, Utils.toUri(Filename.concat(base, path)), name);
      | _ => failwith("Expected /some/path.re:typename")
    }
  });
  tbl
}; */

open TypeMapSerde.Config;

let loadTypeMap = config => {
  open Util.RResult.InfixResult;
  /* let%try_force entries = Util.RJson.get("entries", config) |?> Util.RJson.array; */
  open Util.Infix;
  /* let custom = Json.get("custom", config) |?> Json.array |? []; */

  let state = TopTypes.forRootPath(Sys.getcwd());

  let tbl = Hashtbl.create(10);

  config.custom->Belt.List.forEach(({path, name, module_, args}) => {
    open Util.RResult.InfixResult;
    open Util.Infix;
    Hashtbl.replace(
      tbl,
      (
        module_,
        path,
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

  config.entries->Belt.List.forEach(({file, type_}) => {
    let%try_force () =
      TypeMap.GetTypeMap.forInitialType(~tbl, ~state, Utils.toUri(Filename.concat(Sys.getcwd(), file)), type_);
    ();
  });

  (state, TypeMap.GetTypeMap.toSimpleMap(tbl));
};

let compareHashtbls = (one, two) => {
  if (Hashtbl.length(one) != Hashtbl.length(two)) {
    false
  } else {
    Hashtbl.fold((k, v, good) => {
      good && Hashtbl.mem(two, k) && v == Hashtbl.find(two, k)
    }, one, true)
  }
};

let main = configPath => {
  let json = Json.parse(Util.Files.readFileExn(configPath));
  let%try_force config = TypeMapSerde.configFromJson(json);
  let (state, tbl) = loadTypeMap(config);

  let lockFilePath = Filename.dirname(configPath)->Filename.concat("types.lock.json");

  let lockfile = switch (Files.readFileResult(lockFilePath)) {
    | Error(_) => {
      TypeMap.DigTypes.version: config.version,
      pastVersions: Hashtbl.create(1),
      current: tbl
    }
    | Ok(contents) => {
      let json = Json.parse(contents);
      let%try_force lockfile = TypeMapSerde.lockfileFromJson(json)
      if (lockfile.version == config.version) {
        /* TODO allow addative type changes */
        if (!compareHashtbls(lockfile.current, tbl)) {
          failwith("Types do not match lockfile! You must increment the version number in your types.json")
        } else {
          lockfile
        }
      } else if (lockfile.version + 1 == config.version) {
        lockfile.pastVersions->Hashtbl.replace(lockfile.version, lockfile.current);
        {
          version: config.version,
          pastVersions: lockfile.pastVersions,
          current: tbl
        }
      } else {
        failwith("Version must be incremented by one")
      }
    }
  };

  let lockfileJson = TypeMapSerde.lockfileToJson(lockfile);
  Files.writeFileExn(lockFilePath, Json.stringifyPretty(~indent=2, lockfileJson));

  let capitalize = s => s == "" ? "" :
  String.uppercase(String.sub(s, 0, 1)) ++ String.sub(s, 1, String.length(s) - 1);

  /* tbl */
  let fns =
    switch (config.engine) {
    | Bs_json => makeFns(Serde.BsJson.declDeserializer, tbl) @ makeFns(Serde.BsJson.declSerializer, tbl)
    | Rex_json => makeFns(Serde.Json.declDeserializer, tbl) @ makeFns(Serde.Json.declSerializer, tbl)
    };

  let versionModuleName = version => "Version" ++ string_of_int(version);

  let body = 
    makeModule(versionModuleName(config.version), [
      Ast_helper.Str.type_(Recursive, lockTypes(tbl)),
      Ast_helper.Str.value(Recursive, fns)
    ]);
  open Parsetree;
  let body = [
    [%stri
      let currentVersion = [%e Ast_helper.Exp.constant(Parsetree.Pconst_integer(string_of_int(config.version), None))]
    ],
    body,
    ...switch (config.engine) {
       | Bs_json => [%str
           let parseVersion = [%e Serde.BsJson.deserializeTransformer.parseVersion];
           let wrapWithVersion = [%e Serde.BsJson.serializeTransformer.wrapWithVersion]
         ]
       | Rex_json => [%str
           let parseVersion = [%e Serde.Json.deserializeTransformer.parseVersion];
           let wrapWithVersion = [%e Serde.Json.serializeTransformer.wrapWithVersion]
         ]
       },
  ];

  let expIdent = lident => Ast_helper.Exp.ident(Location.mknoloc(lident));

  let converters = config.entries->Belt.List.map(({file, type_, publicName}) => {
    let uri = Utils.toUri(Filename.concat(Sys.getcwd(), file));
    let%try_force (moduleName, modulePath, name) = TypeMap.GetTypeMap.fileToReference(~state, uri, type_);
    let des = Serde.MakeDeserializer.transformerName(~moduleName, ~modulePath, ~name);
    let ser = Serde.MakeSerializer.transformerName(~moduleName, ~modulePath, ~name);
    let fullName = Serde.MakeDeserializer.fullName(~moduleName, ~modulePath, ~name);

    open Longident;

    let makeUpgrader = (current, target) => {
      open Parsetree;
      let rec loop = current => if (current === target) {
        [%expr Belt.Result.Ok(data)]
      } else {
        [%expr switch ([%e expIdent(Ldot(Lident(versionModuleName(current + 1)), "upgrade_" ++ fullName))](data)) {
          | Belt.Result.Error(error) => Belt.Result.Error(error)
          | Ok(data) => [%e loop(current + 1)]
        }]
      };
      loop(current)
    };

    Ast_helper.Str.value(
      Asttypes.Nonrecursive,
      [
        Ast_helper.Vb.mk(
          Ast_helper.Pat.var(Location.mknoloc("serialize" ++ capitalize(publicName))),
          [%expr data => wrapWithVersion(currentVersion, [%e
            Ast_helper.Exp.ident(Location.mknoloc(Longident.Ldot(Longident.Lident(versionModuleName(config.version)), ser)))
          ](data))]
        ),
        Ast_helper.Vb.mk(
          Ast_helper.Pat.var(Location.mknoloc("deserialize" ++ capitalize(publicName))),
          [%expr
            data => switch (parseVersion(data)) {
              | Belt.Result.Error(err) => Belt.Result.Error(err)
              | [@implicit_arity]Ok((version, data)) => [%e 
                Ast_helper.Exp.match(
                  [%expr version],
                  {
                    let rec loop = n => n < config.version ? [Ast_helper.Exp.case([%pat? _], [%expr Belt.Result.Error("Unexpected version " ++ string_of_int(version))])] : {
                      [
                        {
                          Ast_helper.Exp.case(
                            Ast_helper.Pat.constant(Parsetree.Pconst_integer(string_of_int(n), None)),
                            [%expr switch ([%e expIdent(Ldot(Lident(versionModuleName(n)), des))](data)) {
                              | Belt.Result.Error(error) => Belt.Result.Error(error)
                              | Ok(data) => [%e makeUpgrader(n, config.version)]
                            }]
                          )
                        },
                        ...loop(n - 1)
                      ]
                    };
                    loop(config.version)
                  }
                )
              ]
            }
          ],
          /* Ast_helper.Exp.ident(Location.mknoloc(Longident.Ldot(Longident.Lident(versionModuleName(config.version)), des))), */
        ),
      ],
    );
  });

  Pprintast.structure(Format.str_formatter, body @ converters);

  let ml = Format.flush_str_formatter();
  Files.writeFile(config.output, ml) |> ignore;
};

switch (Sys.argv->Belt.List.fromArray) {
  | [_, config] => main(config)
  | _ => failwith("Bad args")
}
