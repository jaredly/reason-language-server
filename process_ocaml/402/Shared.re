
open Compiler_libs_402;

/** TODO move to the Process_ stuff */
let rec dig = (typ) =>
  switch typ.Types.desc {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | Types.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let digConstructor = (expr) => {
  let expr = dig(expr);
  switch (expr.desc) {
  | Tconstr(path, _args, _memo) => Some(path)
  | _ => None
  };
};

let rec variableKind = (t) =>
  switch t.Types.desc {
  | Tlink(t) => variableKind(t)
  | Tsubst(t) => variableKind(t)
  | Tarrow(_) => `Function
  | Ttuple(_) => `Array
  | Tconstr(_) => `Variable
  | Tobject(_) => `Object
  | Tnil => `Null
  | Tvariant(_) => `EnumMember
  | Tpoly(_) => `EnumMember
  | Tpackage(_) => `Module
  | _ => `Variable
  };

let rec makeFlexible = t => {
  SharedTypes.toString: () => {
    PrintType.default.expr(PrintType.default, t)
    |> PrintType.prettyString(~width=40)
  },
  variableKind: variableKind(t),
  getConstructorPath: () => digConstructor(t),
  getArguments: () => {
      loop(t)
  }
}

/* TODO move this into ProcessExtra or somewheres */
and loop = t => switch (t.Types.desc) {
  | Types.Tsubst(t)
  | Tlink(t) => loop(t)
  | Tarrow(label, argt, res, _) =>
    let (args, fin) = loop(res);
    ([(label, makeFlexible(argt)), ...args], fin)
  | _ => ([], makeFlexible(t))
};
