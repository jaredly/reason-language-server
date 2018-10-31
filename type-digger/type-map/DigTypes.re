
type shortReference = (Analyze.TopTypes.moduleName, list(string), string);

type reference = {
  uri: string,
  moduleName: string,
  declared: SharedTypes.declared(SharedTypes.Type.t),
  modulePath: list(string),
  name: string,
  env: Query.queryEnv,
};

type typeSource('reference) =
  | Builtin(string)
  | Public('reference)
  | NotFound;

type lockfile('reference) = {
  version: int,
  pastVersions: Belt.HashMap.Int.t(
    list((
      shortReference,
      SharedTypes.SimpleType.declaration(typeSource('reference))
    ))
  ),
  current: list((
    shortReference,
    SharedTypes.SimpleType.declaration(typeSource('reference))
  ))
};

type serializableLockfile = lockfile(shortReference);