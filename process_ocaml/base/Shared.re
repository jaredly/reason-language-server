
#if 406
open Compiler_libs_406;
#elif 402
open Compiler_libs_402;
#endif
open Belt.Result;

let tryReadCmi = cmi =>
  switch (Cmi_format.read_cmi(cmi)) {
  | exception _ => Error("Invalid cmi format - probably wrong ocaml version")
  | x => Ok(x)
  };

let tryReadCmt = cmt =>
  switch (Cmt_format.read_cmt(cmt)) {
  | exception _ => Error("Invalid cmt format - probably wrong ocaml version")
  | x => Ok(x)
  };

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

let typeKind = (t) =>
  switch t.Types.type_kind {
  | Type_open
  | Type_abstract => `TypeParameter
  | Type_record(_) => `Interface
  | Type_variant(_) => `Enum
  };

let makeDeclaration = t => {
  SharedTypes.declToString: name =>
PrintType.default.decl(PrintType.default, name, name, t) |> PrintType.prettyString,
  declarationKind: typeKind(t)
}

let labelToString = label => switch label {
  | Asttypes.Nolabel => ""
  | Optional(label) | Labelled(label) => label
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

and loop = t => switch (t.Types.desc) {
  | Types.Tsubst(t)
  | Tlink(t) => loop(t)
  | Tarrow(label, argt, res, _) =>
    let (args, fin) = loop(res);
    ([(labelToString(label), makeFlexible(argt)), ...args], fin)
  | _ => ([], makeFlexible(t))
};
