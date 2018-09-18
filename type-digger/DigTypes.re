
type reference = {
  uri: string,
  moduleName: string,
  modulePath: list(string),
  name: string,
};

type typeSource =
  | Builtin(string)
  | Public(reference)
  | NotFound;
