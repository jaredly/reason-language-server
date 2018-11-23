
[@rename.module_ "module"]
type custom = {
  module_: string,
  path: list(string),
  name: string,
  args: int,
};

[@rename.type_ "type"]
type entry = {
  file: string,
  type_: string,
  publicName: option(string),
};

[@rename.Rex_json "rex-json"]
[@rename.Bs_json "Js.Json"]
type engine = Rex_json | Bs_json;

type t = {
  version: int,
  output: string,
  engine,
  entries: list(entry),
  custom: list(custom),
};

module Locked = {
  type lockedEntry = {
    moduleName: string,
    modulePath: list(string),
    name: string,
  };

  type lockedConfig('reference) = {
    entries: list(lockedEntry),
    engineVersion: int,
    typeMap: TypeMap.DigTypes.typeMap('reference)
  };

  type lockfile('reference) = {
    engine,
    versions: array(lockedConfig('reference))
  };

  let getLatestVersion = lockfile => Array.length(lockfile.versions);
  let getVersion = (lockfile, version) => lockfile.versions[version - 1];
  let addVersion = (lockfile, ~typeMap, ~entries, ~engineVersion) => {
    ...lockfile,
    versions: Belt.List.toArray(Belt.List.fromArray(lockfile.versions) @ [{typeMap, entries, engineVersion}])
  };
  let updateVersion = (lockfile, ~typeMap, ~entries, ~engineVersion) => {
    ...lockfile,
    versions: Belt.List.toArray(
      Belt.List.fromArray(lockfile.versions)->Belt.List.reverse->Belt.List.tailExn->Belt.List.reverse
      @
      [{typeMap, entries, engineVersion}]
    )
  };
};

type serializableLockfile = Locked.lockfile(TypeMap.DigTypes.shortReference);
