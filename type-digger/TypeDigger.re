Printexc.record_backtrace(true);
open Analyze;
module Json = Vendor.Json;
module Locked = TypeMapSerde.Config.Locked;
let loc = Location.none;
open DigUtils;

// let optimiseLater = (timeout, closure) => {
//   let start = Unix.gettimeofday();
//   let result = closure();
//   let time = Unix.gettimeofday() -. start;
//   if (time > timeout) {
//     print_endline("This took too long! " ++ string_of_float(time))
//   };
//   result
// };

open TypeMapSerde.Config;

let loadTypeMap = config => {
  let state = TopTypes.forRootPath(Sys.getcwd());

  let tbl = Hashtbl.create(10);

  config.custom
  ->Belt.List.forEach(({path, name, module_, args}) =>
      Hashtbl.replace(
        tbl,
        (module_, path, name),
        (
          [],
          SharedTypes.SimpleType.{
            name,
            variables: {
              switch (args) {
              | None => []
              | Some(0) => []
              | Some(args) =>
                let rec loop = n =>
                  n <= 0
                    ? []
                    : [
                      SharedTypes.SimpleType.Variable("arg" ++ string_of_int(args - n)),
                      ...loop(n - 1),
                    ];
                loop(args);
              };
            },
            body: Abstract,
          },
        ),
      )
    );

  let globalEngines =
    switch (config.globalEngines) {
    | None =>
      switch (config.engines->activeEngines) {
      | [] => failwith("No engines specified")
      | engines => engines
      }
    | Some(engines) => engines
    };

  let lockedEntries =
    config.entries
    ->Belt.List.map(({file, type_, engines}) => {
        let%try_force (moduleName, modulePath, name) =
          TypeMap.GetTypeMap.forInitialType(
            ~tbl,
            ~state,
            Utils.toUri(Filename.concat(Sys.getcwd(), file)),
            type_,
          );
        {
          Locked.moduleName,
          modulePath,
          name,
          engines:
            (
              switch (engines) {
              | None => globalEngines
              | Some(engines) => engines
              // TODO: use the real engine version
              }
            )
            ->Belt.List.map(engine => (engine, 1)),
        };
      });

  (state, TypeMap.GetTypeMap.toSimpleMap(tbl), lockedEntries);
};

// let isSerializationAttribute = (({Location.txt}, _)) => Util.Utils.startsWith(txt, "rename.");

let makeLockfilePath = configPath => {
  let base = Filename.dirname(configPath);
  let name = Filename.basename(configPath);
  let parts = String.split_on_char('.', name);
  let newName =
    switch (parts) {
    | [single, ..._] => single ++ ".lock.json"
    | [] => "lockfile.json"
    };
  Filename.concat(base, newName);
};

let main = (~upvert=false, ~override=false, configPath) => {
  let json = Json.parse(Util.Files.readFileExn(configPath));
  let%try_force config =
    switch (TypeMapSerde.configFromJson(json)) {
    | Error(m) => Error(String.concat("::", m))
    | Ok(v) => Ok(v)
    };
  TypeMapSerde.checkVersion(~upvert, ~configPath, config, json);
  let (state, currentTypeMap, lockedEntries) = loadTypeMap(config);

  let (engine, outfile, helpers) =
    switch (config.engines) {
    | {bs_json: Some({output, helpers})} => ((module Serde.BsJson): (module Serde.Engine.T), output, helpers)
    | {rex_json: Some({output, helpers})} => ((module Serde.Json): (module Serde.Engine.T), output, helpers)
    | _ => failwith("No engine espcified")
    };

  let lockFilePath = makeLockfilePath(configPath);

  let lockfile =
    Lockfile.parseLockfile(~override, config, lockedEntries, currentTypeMap, lockFilePath);

  let lockedDeep =
    Belt.Array.concat(
      [|Hashtbl.create(0)|],
      lockfile.versions
      ->Belt.Array.mapWithIndex((_index, config) =>
          Lockdown.typesAndDependencies(config.typeMap)
        ),
    );

  let makeTypeModules = () => {
    let rec loop = version =>
      version > config.version
        ? []
        : {
          [
            TypesFile.makeModule(
              ~currentVersion=config.version,
              ~lockedDeep,
              ~lockfile,
              version,
              lockfile->Locked.getVersion(version).typeMap,
            ),
            ...loop(version + 1),
          ];
        };
    loop(1);
  };

  let makeAllModules = () => {
    let rec loop = version =>
      version > config.version
        ? []
        : {
          [
            SerdeFile.makeFullModule(
              ~helpers,
              ~engine,
              ~currentVersion=config.version,
              ~lockedDeep,
              ~lockfile,
              version,
              lockfile->Locked.getVersion(version),
            ),
            ...loop(version + 1),
          ];
        };
    loop(1);
  };

  module Engine = (val engine: Serde.Engine.T);

  let body =
    Parsetree.[[%stri [@ocaml.warning "-34"]]]
    @ makeTypeModules()
    @ makeAllModules()
    @ Parsetree.[
        [%stri
          let currentVersion = [%e
            Ast_helper.Exp.constant(
              Parsetree.Pconst_integer(string_of_int(config.version), None),
            )
          ]
        ],
        [%stri
          module Current = [%m
            Ast_helper.Mod.ident(
              Location.mknoloc(Longident.Lident(versionModuleName(config.version))),
            )
          ]
        ],
        /* body, */
        ...[%str
             let parseVersion = [%e Engine.deserializeTransformer.parseVersion];
             let wrapWithVersion = [%e Engine.serializeTransformer.wrapWithVersion]
           ],
      ];

  let converters = SerdeFile.makeConverters(~config, ~state);

  let structure = body @ converters;

  if (outfile->Filename.check_suffix(".re")) {
    let reason_structure = Reason_toolchain.From_current.copy_structure(structure);
    Reason_toolchain.RE.print_implementation_with_comments(
      Format.str_formatter,
      (reason_structure, []),
    );
  } else {
    Pprintast.structure(Format.str_formatter, structure);
  };
  let ml = Format.flush_str_formatter();

  let lockfileJson = TypeMapSerde.lockfileToJson(lockfile);
  Files.writeFileExn(lockFilePath, Json.stringifyPretty(~indent=2, lockfileJson));

  Files.writeFile(outfile, ml) |> ignore;
};

switch (Sys.argv->Belt.List.fromArray) {
| [_, config] => main(config)
| [_, config, "--override"] => main(~override=true, config)
| [_, config, "--upvert"] => main(~upvert=true, config)
| _ => failwith("Bad args")
};