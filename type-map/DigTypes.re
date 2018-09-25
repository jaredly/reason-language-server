
type reference = {
  uri: string,
  moduleName: string,
  declared: SharedTypes.declared(SharedTypes.Type.t),
  modulePath: list(string),
  name: string,
  env: Query.queryEnv,
};

type typeSource =
  | Builtin(string)
  | Public(reference)
  | NotFound;
