
type shortReference = (Analyze.TopTypes.moduleName, list(string), string);

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

type lockfile = {
  version: int,
  pastVersions: Belt.HashMap.Int.t(
    list((
      shortReference,
      SharedTypes.SimpleType.declaration(typeSource)
    ))
  ),
  current: list((
    shortReference,
    SharedTypes.SimpleType.declaration(typeSource)
  ))
};