
type custom = {
  module_: string,
  path: list(string),
  name: string,
  args: int,
};

type entry = {
  file: string,
  type_: string,
  publicName: string,
  history: bool,
};

type engine = Rex_json | Bs_json;

type t = {
  version: int,
  output: string,
  engine,
  entries: list(entry),
  custom: list(custom),
};
