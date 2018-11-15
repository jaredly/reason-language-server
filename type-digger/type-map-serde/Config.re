
type custom = {
  module_: string,
  path: list(string),
  name: string,
  args: int,
};

type versionPlacement = EmbeddedVersion | WrappedVersion;

type entry = {
  file: string,
  type_: string,
  publicName: option(string),
  versionPlacement: option(versionPlacement),
};

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
    versionPlacement: versionPlacement,
  };

  type lockedConfig('reference) = {
    version: int,
    entries: list(lockedEntry),
    versionedEngine: (engine, int),
    typeMap: TypeMap.DigTypes.typeMap('reference)
  };

  type lockfile('reference) = {
    current: lockedConfig('reference),
    pastVersions: Hashtbl.t(int, lockedConfig('reference))
  };
};

type serializableLockfile = Locked.lockfile(TypeMap.DigTypes.shortReference);
