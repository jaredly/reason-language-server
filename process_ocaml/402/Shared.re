
open Compiler_libs_402;
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
  | Tconstr(path, args, _memo) => Some((path, args))
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

let rec getFnArgs = t => {
  switch (dig(t).desc) {
    | Tarrow(label, arg, res, _) =>
      let (args, res) = getFnArgs(res);
      ([(label, arg), ...args], res)
    | _ => ([], t)
  }
};

/* HACK(jared): They removed all way for me to produce an "Ident.t" with the correct stamp.
   They forced my hand.
*/
let convertIdent = (oldIdent) => {(Obj.magic(oldIdent): Current.ident)};

let rec mapOldPath = oldPath => {
  switch (oldPath) {
    | Path.Pident(oldIdent) => Current.Pident(convertIdent(oldIdent))
    | Path.Pdot(inner, name, int) => Current.Pdot(mapOldPath(inner),name,int)
    | Path.Papply(one, two) => Current.Papply(mapOldPath(one), mapOldPath(two))
  }
};

let rec asSimpleType = t => {
  open SharedTypes;
  switch (dig(t).desc) {
    | Tvar(None) => SimpleType.AnonVariable
    | Tvar(Some(text)) => SimpleType.Variable(text)
    | Tarrow(label, arg, res, _) =>
      let (args, res) = getFnArgs(res);
      let args = [(label, arg), ...args];
      let args = args->Belt.List.map(((label, arg)) => (
        /* label */
        label == "" ? None : Some(label)
        , asSimpleType(arg)));
      SimpleType.Fn(args, asSimpleType(res))
    | Ttuple(items) =>
      SimpleType.Tuple(items->Belt.List.map(asSimpleType))
    | Tconstr(path, args, _) =>
      SimpleType.Reference(path, args->Belt.List.map(asSimpleType))
    | _ => SimpleType.Other
  }
};

let rec asSimpleDeclaration = (name, t) => {
  open SharedTypes;
  {
    SimpleType.name,
    variables: t.Types.type_params->Belt.List.map(param => {
      asSimpleType(param)
    }),
    body: switch (t.type_kind, t.type_manifest) {
      | (Type_open, _) => Open
      | (Type_abstract, None) => Abstract
      | (Type_abstract, Some(expr)) => Expr(asSimpleType(expr))
      | (Type_record(labels, _), _) => Record(
        labels->Belt.List.map(({ld_id, ld_type}) => (
          Ident.name(ld_id),
          asSimpleType(ld_type)
        ))
      )
      | (Type_variant(constructors), _) => Variant(
        constructors->Belt.List.map(({cd_id, cd_args, cd_res}) => (
          Ident.name(cd_id),
          cd_args->Belt.List.map(asSimpleType),
          switch (cd_res) {
          | None => None
          | Some(arg) => Some(asSimpleType(arg))
          }
        ))
      )
    }
  }
};

module Converter = Migrate_parsetree.Convert(
  Migrate_parsetree.OCaml_402,
  Migrate_parsetree.OCaml_current
);

let migrateAttributes = t => {
  t.Types.type_attributes->Belt.List.map((({Asttypes.txt}, payload)) => {
    (Current.mknoloc(txt), switch payload {
      | PStr(structure) => Current.PStr(Converter.copy_structure(Obj.magic(structure)))
      | PPat(pattern, guard) => Current.PPat(Converter.copy_pattern(Obj.magic(pattern)), switch guard {
        | None => None
        | Some(exp) => Some(Converter.copy_expression(Obj.magic(exp)))
      })
      | PTyp(typ) => Current.PTyp(Converter.copy_core_type(Obj.magic(typ)))
    })
  });
};

let makeDeclaration = t => {
  SharedTypes.declToString: name =>
PrintType.default.decl(PrintType.default, name, name, t) |> PrintType.prettyString,
  declarationKind: typeKind(t),
  asSimpleDeclaration: name => asSimpleDeclaration(name, t)
  |> SharedTypes.SimpleType.declMapSource(mapOldPath),
  migrateAttributes: () => migrateAttributes(t),
}

let labelToString = label => label;

let rec makeFlexible = t => {
  SharedTypes.toString: () => {
    PrintType.default.expr(PrintType.default, t)
    |> PrintType.prettyString(~width=40)
  },
  variableKind: variableKind(t),
  getConstructorPath: () => switch (digConstructor(t)) {
    | None => None
    | Some((path, args)) =>
      let newPath = mapOldPath(path);
      Some((newPath, args |> List.map(makeFlexible)))
  },
  getArguments: () => {
      loop(t)
  },
  asSimpleType: () => asSimpleType(t)
 |> SharedTypes.SimpleType.mapSource(mapOldPath)
}

and loop = t => switch (t.Types.desc) {
  | Types.Tsubst(t)
  | Tlink(t) => loop(t)
  | Tarrow(label, argt, res, _) =>
    let (args, fin) = loop(res);
    ([(labelToString(label), makeFlexible(argt)), ...args], fin)
  | _ => ([], makeFlexible(t))
};
