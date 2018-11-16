
Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;
module Locked = TypeMapSerde.Config.Locked;
let loc = Location.none;

let makeModule = (moduleName, contents) =>
  Ast_helper.Str.module_(
    Ast_helper.Mb.mk(Location.mknoloc(moduleName), Ast_helper.Mod.mk(Parsetree.Pmod_structure(contents))),
  );

let versionModuleName = version => "Version" ++ string_of_int(version);

let hashList = tbl => Hashtbl.fold((key, value, result) => [(key, value), ...result], tbl, []);

let lockTypes = (~currentVersion, version, typeMap, lockedDeep) => {
  hashList(typeMap)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name) as ref, (attributes, decl))) => {
    let alias = if (currentVersion == version ||
      lockedDeep[currentVersion]->Upgrade.hashFind(ref) == lockedDeep[version]->Upgrade.hashFind(ref)
    ) {
      Some(Serde.OutputType.unflatten([moduleName] @ modulePath @ [name]));
    } else if (version > 1 &&
      lockedDeep[version - 1]->Upgrade.hashFind(ref) == lockedDeep[version]->Upgrade.hashFind(ref)
    ) {
      Some(Ldot(Lident(versionModuleName(version - 1)), Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name)))
    } else {
      None
    }
    Serde.OutputType.outputDeclaration(~alias, moduleName, modulePath, name, Serde.OutputType.showSource, decl)
  });
};

let optimiseLater = (timeout, closure) => {
  let start = Unix.gettimeofday();
  let result = closure();
  let time = Unix.gettimeofday() -. start;
  if (time > 1.) {
    print_endline("This took too long! " ++ string_of_float(time))
  };
  result
};

let makeFns = (maker, tbl) => {
    hashList(tbl)
    ->Belt.List.sort(compare)
    ->Belt.List.map((((moduleName, modulePath, name), (attributes, decl))) => 
        maker(~moduleName, ~modulePath, ~name, decl)
    );
};

open TypeMapSerde.Config;

let loadTypeMap = config => {
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
      (
        [],
      SharedTypes.SimpleType.{
        name,
        variables: {
          let rec loop = n =>
            n <= 0 ? [] : [SharedTypes.SimpleType.Variable("arg" ++ string_of_int(args - n)), ...loop(n - 1)];
          loop(args);
        },
        body: Abstract,
      },
      )
    );
  });

  let lockedEntries = config.entries->Belt.List.map(({file, type_}) => {
    let%try_force ((moduleName, modulePath, name)) =
      TypeMap.GetTypeMap.forInitialType(~tbl, ~state, Utils.toUri(Filename.concat(Sys.getcwd(), file)), type_);
    {Locked.moduleName, modulePath, name};
  });

  (state, TypeMap.GetTypeMap.toSimpleMap(tbl), lockedEntries);
};

let areHashtblsEqual = (one, two) => {
  if (Hashtbl.length(one) != Hashtbl.length(two)) {
    false
  } else {
    Hashtbl.fold((k, (a, v), good) => {
      good && Hashtbl.mem(two, k) && {
        let (a2, v2) = Hashtbl.find(two, k);
        /* TODO compare migrate functions? Actually maybe I don't care about them being locked. */
        v2 == v
      }
    }, one, true)
  }
};

let main = configPath => {
  let json = Json.parse(Util.Files.readFileExn(configPath));
  let%try_force config = TypeMapSerde.configFromJson(json);
  let (state, currentTypeMap, lockedEntries) = loadTypeMap(config);

  let lockFilePath = Filename.dirname(configPath)->Filename.concat("types.lock.json");

  let lockfile = switch (Files.readFileResult(lockFilePath)) {
    | Error(_) => {
      Locked.versions: [|{
        typeMap: currentTypeMap,
        engineVersion: 1,
        entries: lockedEntries,
      }|],
      engine: config.engine,
    }
    | Ok(contents) => {
      let json = Json.parse(contents);
      let%try_force lockfile = TypeMapSerde.lockfileFromJson(json)
      if (lockfile.engine != config.engine) {
        failwith("Config engine does not match lockfile engine.")
      };
      let latestVersion = Locked.getLatestVersion(lockfile);
      if (latestVersion == config.version) {
        /* TODO allow addative type changes... maybe */
        if (!areHashtblsEqual(lockfile->Locked.getVersion(config.version).typeMap, currentTypeMap)) {

          /* let lockfileJson = TypeMapSerde.lockfileToJson({
            ...lockfile,
            current: currentTypeMap
          });
          Files.writeFileExn(lockFilePath ++ ".new", Json.stringifyPretty(~indent=2, lockfileJson)); */

          failwith("Types do not match lockfile! You must increment the version number in your types.json")
        } else {
          lockfile
        }
      } else if (latestVersion + 1 == config.version) {
        lockfile->Locked.addVersion(~typeMap=currentTypeMap, ~entries=lockedEntries, ~engineVersion=1)
      } else {
        failwith("Version must be incremented by one")
      }
    }
  };

  let capitalize = s => s == "" ? "" :
  String.uppercase(String.sub(s, 0, 1)) ++ String.sub(s, 1, String.length(s) - 1);

  let lockedDeep = Belt.Array.concat([|Hashtbl.create(0)|], lockfile.versions->Belt.Array.mapWithIndex((index, config) => {
    Lockdown.typesAndDependencies(config.typeMap)
  }));

  let makeFullModule = (version, typeMap) => {
    let fns =
      switch (config.engine) {
      | Bs_json =>
        let fns = makeFns(Serde.BsJson.declDeserializer, typeMap);
        version == config.version ? fns @ makeFns(Serde.BsJson.declSerializer, typeMap) : fns;
      | Rex_json =>
        let fns = makeFns(Serde.Json.declDeserializer, typeMap);
        version == config.version ? fns @ makeFns(Serde.Json.declSerializer, typeMap) : fns;
      };

    makeModule(versionModuleName(version), [
      Ast_helper.Str.type_(Recursive, lockTypes(~currentVersion=config.version, version, typeMap, lockedDeep)),
      Ast_helper.Str.value(Recursive, fns),
      ...(if (version > 1) {
        let pastTypeMap = lockfile->Locked.getVersion(version - 1).typeMap;
        [
          Ast_helper.Str.value(
            Recursive,
            hashList(typeMap)
            ->Belt.List.sort(compare)
            ->Belt.List.keepMap((((moduleName, modulePath, name) as ref, decl)) =>  {
              switch (pastTypeMap->Upgrade.hashFind(ref)) {
                | Some(dPast) => Some(
                    Upgrade.makeUpgrader(version, pastTypeMap, lockedDeep, ~moduleName, ~modulePath, ~name, decl, dPast)
                  )
                | None => None
              }
            })
          ),
        ];
      } else {
        []
      })
    ]);
  };

  let makeAllModules = () => {
    let rec loop = version => version > config.version ? [] : {
      let tbl = if (version == config.version) {
        currentTypeMap
      } else {
        lockfile->Locked.getVersion(version).typeMap
      };

      [makeFullModule(version, tbl), ...loop(version + 1)]
    };
    loop(1)
    /* loop(config.version) */
  };

  open Parsetree;
  let body = makeAllModules() @ [
    [%stri
      let currentVersion = [%e Ast_helper.Exp.constant(Parsetree.Pconst_integer(string_of_int(config.version), None))]
    ],
    /* body, */
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
    let publicName = switch publicName {
      | None => type_
      | Some(name) => name
    };
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
        [%expr {
          let data = [%e expIdent(Ldot(Lident(versionModuleName(current + 1)), "migrate_" ++ fullName))](data);
          [%e loop(current + 1)]
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
              | Belt.Result.Error(err) => Belt.Result.Error([err])
              | [@implicit_arity]Ok((version, data)) => [%e 
                Ast_helper.Exp.match(
                  [%expr version],
                  {
                    let rec loop = n =>
                      if (n < 1) {
                        [Ast_helper.Exp.case([%pat? _], [%expr Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])])];
                      } else {
                        [
                          Ast_helper.Exp.case(
                            Ast_helper.Pat.constant(Parsetree.Pconst_integer(string_of_int(n), None)),
                            switch%expr ([%e expIdent(Ldot(Lident(versionModuleName(n)), des))](data)) {
                            | Belt.Result.Error(error) => Belt.Result.Error(error)
                            | Ok(data) =>
                              %e
                              makeUpgrader(n, config.version)
                            },
                          ),
                          ...loop(n - 1),
                        ];
                      };
                    loop(config.version)
                  }
                )
              ]
            }
          ],
        ),
      ],
    );
  });

  Pprintast.structure(Format.str_formatter, body @ converters);
  let ml = Format.flush_str_formatter();

  let lockfileJson = TypeMapSerde.lockfileToJson(lockfile);
  Files.writeFileExn(lockFilePath, Json.stringifyPretty(~indent=2, lockfileJson));

  Files.writeFile(config.output, ml) |> ignore;
};

switch (Sys.argv->Belt.List.fromArray) {
  | [_, config] => main(config)
  | _ => failwith("Bad args")
}
