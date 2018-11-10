
type shortReference = (Analyze.TopTypes.moduleName, list(string), string);

type reference = {
  uri: string,
  moduleName: string,
  declared: SharedTypes.declared(SharedTypes.Type.t),
  modulePath: list(string),
  name: string,
  env: Query.queryEnv,
};

let showReference = ({moduleName, modulePath, name}) => {
  String.concat(".", [moduleName] @ modulePath @ [name])
};

let referenceToLident = ({moduleName, modulePath, name}) => {
  switch (Longident.unflatten([moduleName] @ modulePath @ [name])) {
    | None => assert(false)
    | Some(lident) => lident
  }
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
