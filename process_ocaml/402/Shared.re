
/** TODO move to the Process_ stuff */
let rec dig = (typ) =>
  switch typ.UnifiedTypes.desc {
  | UnifiedTypes.Tlink(inner) => dig(inner)
  | UnifiedTypes.Tsubst(inner) => dig(inner)
  | UnifiedTypes.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let digConstructor = (expr) => {
  let expr = dig(expr);
  switch (expr.desc) {
  | Tconstr(path, _args, _memo) => Some(path)
  | _ => None
  };
};

let makeFlexible = t => {
  SharedTypes.toString: () => {
    PrintType.default.expr(PrintType.default, t)
    |> PrintType.prettyString(~width=40)
  },
  getConstructorPath: () => digConstructor(t),
  getArguments: () => []
};
