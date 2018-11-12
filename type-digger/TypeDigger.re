
Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;
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
  ->Belt.List.map((((moduleName, modulePath, name) as ref, decl)) => {
    let alias = if (currentVersion == version ||
      lockedDeep[currentVersion]->Hashtbl.find(ref) == lockedDeep[version]->Hashtbl.find(ref)
    ) {
      Some(Serde.OutputType.unflatten([moduleName] @ modulePath @ [name]));
    } else if (version > 1 &&
      lockedDeep[version - 1]->Hashtbl.find(ref) == lockedDeep[version]->Hashtbl.find(ref)
    ) {
      Some(Ldot(Lident(versionModuleName(version - 1)), Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name)))
    } else {
      None
    }
    Serde.OutputType.outputDeclaration(~alias, moduleName, modulePath, name, (source, args) =>  {
      Ast_helper.Typ.constr(Location.mknoloc(switch source {
        | TypeMap.DigTypes.NotFound => failwith("Not found type reference")
        | Builtin(name) => Longident.Lident(name)
        | Public((moduleName, modulePath, name)) =>
          Longident.Lident(Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name))
      }), args)
    }, decl)
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

let typesAndDependencies = (tbl) => {
  let collector = Hashtbl.create(10);

  let rec loop = (source) => {
    if (!Hashtbl.mem(collector, source)) {
      let decl = Hashtbl.find(tbl, source);
      collector->Hashtbl.replace(source, `Reference(source));

      let sources = SharedTypes.SimpleType.usedSources(decl)->Belt.List.keepMap(source => switch source {
        | TypeMap.DigTypes.NotFound => assert(false)
        | Builtin(_) => None
        | Public(s) => Some(s)
      });
      sources->Belt.List.forEach(loop)

      let contents = sources->Belt.List.reduce([`Plain(decl)], (result, source) => switch (collector->Hashtbl.find(source)) {
        | `Reference(what) => [`Reference(what), ...result]
        | `Resolved(items) => items @ result
      });

      collector->Hashtbl.replace(source, `Resolved(contents))
    }
  };

  Hashtbl.iter((key, value) => loop(key), tbl);

  let collected = Hashtbl.create(10);
  collector |> Hashtbl.iter((key, value) => switch value {
    | `Resolved(items) => collected->Hashtbl.replace(key, items)
    | _ => assert(false)
  });

  let resolve = (source, items) => {
    let (unresolved, contents) = items->Belt.List.reduce((false, []), ((unresolved, contents), item) => switch item {
      | `Reference(inner) when inner == source => (unresolved, contents)
      | `Reference(inner) => (true, collected->Hashtbl.find(inner) @ contents)
      | `Plain(x) => (unresolved, [`Plain(x), ...contents])
    })
    Hashtbl.replace(collected, source, contents);
    unresolved
  };

  let rec loop = i => {
    let unresolved = Hashtbl.fold((k, v, unresolved) => {
      resolve(k, v) || unresolved
    }, collected, false);
    if (unresolved) {
      if (i > 1000) {
        failwith("Failed to resolve in 1000 iterations");
      };
      loop(i + 1)
    }
  };
  loop(0);

  let resolved = Hashtbl.create(10);
  collected |> Hashtbl.iter((k, v) => resolved->Hashtbl.replace(k, v->Belt.List.map(item => switch item {
      | `Plain(x) => x
      | `Reference(inner) => failwith("Unresolved reference")
  })->Belt.List.sort(compare)));
  resolved
};

let makeFns = (maker, tbl) => {
    hashList(tbl)
    ->Belt.List.sort(compare)
    ->Belt.List.map((((moduleName, modulePath, name), decl)) => 
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
  let (state, currentTypeMap) = loadTypeMap(config);

  let lockFilePath = Filename.dirname(configPath)->Filename.concat("types.lock.json");

  let lockfile = switch (Files.readFileResult(lockFilePath)) {
    | Error(_) => {
      TypeMap.DigTypes.version: config.version,
      pastVersions: Hashtbl.create(1),
      current: currentTypeMap
    }
    | Ok(contents) => {
      let json = Json.parse(contents);
      let%try_force lockfile = TypeMapSerde.lockfileFromJson(json)
      if (lockfile.version == config.version) {
        /* TODO allow addative type changes */
        if (!compareHashtbls(lockfile.current, currentTypeMap)) {
          failwith("Types do not match lockfile! You must increment the version number in your types.json")
        } else {
          lockfile
        }
      } else if (lockfile.version + 1 == config.version) {
        lockfile.pastVersions->Hashtbl.replace(lockfile.version, lockfile.current);
        {
          version: config.version,
          pastVersions: lockfile.pastVersions,
          current: currentTypeMap
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

  let lockedDeep = Array.make(config.version + 1, Hashtbl.create(0));
  lockedDeep[config.version] = typesAndDependencies(currentTypeMap);
  for (version in 1 to config.version - 1) {
    lockedDeep[version] = typesAndDependencies(lockfile.pastVersions->Hashtbl.find(version))
  };

  let makeFullModule = (version, typeMap) => {
    let fns =
      switch (config.engine) {
      | Bs_json => makeFns(Serde.BsJson.declDeserializer, typeMap) @ makeFns(Serde.BsJson.declSerializer, typeMap)
      | Rex_json => makeFns(Serde.Json.declDeserializer, typeMap) @ makeFns(Serde.Json.declSerializer, typeMap)
      };

    makeModule(versionModuleName(version), [
      Ast_helper.Str.type_(Recursive, lockTypes(~currentVersion=config.version, version, typeMap, lockedDeep)),
      Ast_helper.Str.value(Recursive, fns)
    ]);
  };

  let makeAllModules = () => {
    let rec loop = version => version > config.version ? [] : {
      let tbl = if (version == config.version) {
        currentTypeMap
      } else {
        lockfile.pastVersions->Hashtbl.find(version)
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
                    let rec loop = n =>
                      if (n < config.version /* 1 */) {
                        [Ast_helper.Exp.case([%pat? _], [%expr Belt.Result.Error("Unexpected version " ++ string_of_int(version))])];
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
  Files.writeFile(config.output, ml) |> ignore;
};

switch (Sys.argv->Belt.List.fromArray) {
  | [_, config] => main(config)
  | _ => failwith("Bad args")
}
