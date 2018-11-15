
type custom = {
  module_: string,
  path: list(string),
  name: string,
  args: int,
};

type entry = {
  file: string,
  type_: string,
  publicName: option(string),
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
};

type serializableLockfile = Locked.lockfile(TypeMap.DigTypes.shortReference);
