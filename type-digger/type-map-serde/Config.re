
type simpleExpr = SharedTypes.SimpleType.expr(TypeMap.DigTypes.typeSource(
  TypeMap.DigTypes.shortReference
));

type simpleDeclaration = SharedTypes.SimpleType.declaration(TypeMap.DigTypes.typeSource(
  TypeMap.DigTypes.shortReference
));

[@rename.Rex_json "rex-json"]
[@rename.Bs_json "Js.Json"]
type engine = Rex_json | Bs_json;

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
  engines: option(list(engine)),
  publicName: option(string),
  history: option(bool),
  minVersion: option(int),
};

type rexJsonConfig = {
  tailCall: bool,
};

type bsJsonConfig = {
  tailCall: bool,
};

type engineConfig('options) = {
  output: string,
  // a module name
  helpers: option(string),
  options: option('options),
};

[@rename.rex_json "rex-json"]
[@rename.bs_json "Js.Json"]
type engines = {
  rex_json: option(engineConfig(rexJsonConfig)),
  bs_json: option(engineConfig(bsJsonConfig)),
};

[@migrate.engines ({engine, output}) => switch engine {
  | Rex_json => {bs_json: None, rex_json: Some({output, options: None, helpers: None})}
  | Bs_json => {res_json: None, bs_json: Some({output, options: None, helpers: None})}
}]
[@migrate.globalEngines t => None]
[@migrate.outputTypes t => None]
[@migrate.minVersion t => None]
type t = {
  version: int,
  minVersion: option(int),
  outputTypes: option(string),
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
