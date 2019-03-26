
type simpleExpr = SharedTypes.SimpleType.expr(TypeMap.DigTypes.typeSource(
  TypeMap.DigTypes.shortReference
));

type simpleDeclaration = SharedTypes.SimpleType.declaration(TypeMap.DigTypes.typeSource(
  TypeMap.DigTypes.shortReference
));

[@rename.Rex_json "rex-json"]
[@rename.Ezjsonm "ezjsonm"]
[@rename.Bs_json "Js.Json"]
[@rename.Yojson "yojson"]
type engine = Rex_json | Bs_json | Ezjsonm | Yojson;

[@rename.module_ "module"]
[@migrate.args custom => custom.args == 0 ? None : Some(custom.args)]
type custom = {
  module_: string,
  path: list(string),
  name: string,
  args: option(int),
};

[@rename.type_ "type"]
[@migrate.engines entry => None]
[@migrate.history entry => None]
[@migrate.minVersion entry => None]
type entry = {
  file: string,
  type_: string,
  // TODO: support this attribute (currently unused)
  engines: option(list(engine)),
  publicName: option(string),
  // TODO: support this attribute (currently unused)
  history: option(bool),
  // TODO: support this attribute (currently unused)
  minVersion: option(int),
};

// type rexMode = Results | FullExceptions | ThinExceptions;

// type rexJsonConfig = {
//   mode: rexMode,
// };
type errorMode = Result | TrackedExceptions | PlainExceptions;

// TODO: support these options (currently ignored)
type engineOptions = {
  tailCall: bool,
  deserializeErrorMode: errorMode
};

type engineConfig = {
  output: string,
  // a module name
  helpers: option(string),
  options: option(engineOptions),
};

[@rename.rex_json "rex-json"]
[@rename.bs_json "Js.Json"]
type engines = {
  rex_json: option(engineConfig),
  bs_json: option(engineConfig),
  ezjsonm: option(engineConfig),
  yojson: option(engineConfig),
};

let engineConfigs = engines => switch engines {
  | {rex_json: None, bs_json: Some(config)} => [(Bs_json, config)]
  | {rex_json: Some(config), bs_json: None} => [(Rex_json, config)]
  | {rex_json: Some(rex), bs_json: Some(bs)} => [(Rex_json, rex), (Bs_json, bs)]
  | {rex_json: None, bs_json: None} => []
};

let engineConfigs = engines => {
  let result = [];
  let result = switch engines {
    | {rex_json: Some(config)} => [(Rex_json, config), ...result]
    | _ => result
  };
  let result = switch engines {
    | {bs_json: Some(config)} => [(Bs_json, config), ...result]
    | _ => result
  };
  let result = switch engines {
    | {ezjsonm: Some(config)} => [(Ezjsonm, config), ...result]
    | _ => result
  };
  result
};

let activeEngines = engines => engineConfigs(engines)->Belt.List.map(fst);

[@migrate.engines ({engine, output}) => switch engine {
  | Rex_json => {bs_json: None, rex_json: Some({output, helpers: None, options: None}), ezjsonm: None, yojson: None}
  | Bs_json => {rex_json: None, bs_json: Some({output, helpers: None, options: None}), ezjsonm: None, yojson: None}
}]
[@migrate.globalEngines t => None]
[@migrate.lockedTypes t => None]
[@migrate.minVersion t => None]
type t = {
  version: int,
  // TODO: support this attribute
  minVersion: option(int),
  lockedTypes: option(string),
  engines,
  globalEngines: option(list(engine)),
  entries: list(entry),
  custom: list(custom),
};

module Locked = {
  [@migrate.engines l => []]
  type lockedEntry = {
    moduleName: string,
    modulePath: list(string),
    // TODO: support engine versioning
    engines: list((engine, int)),
    name: string,
  };


  type lockedConfig('reference) = {
    entries: list(lockedEntry),
    typeMap: TypeMap.DigTypes.typeMap('reference)
  };

  [@migrate
    ({engine, versions}) => {
      versions:
        versions->Belt.Array.map(config =>
          {
            entries:
              config.entries
              ->Belt.List.map(({moduleName, modulePath, name}) =>
                  {moduleName, modulePath, name, engines: [(engine, config.engineVersion)]}
                ),
          }
        ),
    }
  ];
  type lockfile('reference) = {versions: array(lockedConfig('reference))};

  let getLatestVersion = lockfile => Array.length(lockfile.versions);
  let getVersion = (lockfile, version) => lockfile.versions[version - 1];
  let addVersion = (lockfile, ~typeMap, ~entries) => {
    // ...lockfile,
    versions: Belt.List.toArray(Belt.List.fromArray(lockfile.versions) @ [{typeMap, entries}])
  };
  let updateVersion = (lockfile, ~typeMap, ~entries) => {
    // ...lockfile,
    versions: Belt.List.toArray(
      Belt.List.fromArray(lockfile.versions)->Belt.List.reverse->Belt.List.tailExn->Belt.List.reverse
      @
      [{typeMap, entries}]
    )
  };
};

type serializableLockfile = Locked.lockfile(TypeMap.DigTypes.shortReference);
