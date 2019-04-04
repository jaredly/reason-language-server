
#if 407
open Compiler_libs_407;
#elif 406
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

#if 407
let mapOldPath = oldPath => oldPath
#else
let rec mapOldPath = oldPath => {
  switch (oldPath) {
    | Path.Pident(oldIdent) => Current.Pident(convertIdent(oldIdent))
    | Path.Pdot(inner, name, int) => Current.Pdot(mapOldPath(inner),name,int)
    | Path.Papply(one, two) => Current.Papply(mapOldPath(one), mapOldPath(two))
  }
};
#endif

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
#if 402
        label == "" ? None : Some(label)
#else
        switch label {
          | Nolabel => None
          | Labelled(x) => Some(x)
          | Optional(x) => Some(x)
        }
#endif
        , asSimpleType(arg)));
      SimpleType.Fn(args, asSimpleType(res))
    | Ttuple(items) =>
      SimpleType.Tuple(items->Belt.List.map(asSimpleType))
    | Tconstr(path, args, _) =>
      SimpleType.Reference(path, args->Belt.List.map(asSimpleType))
    | Tvariant({row_fields, row_more: _, row_closed, row_fixed: _, row_name: _}) =>
      SimpleType.RowVariant(
        row_fields->Belt.List.map(((label, field)) => switch field {
          | Reither(_, [arg], _, _) => (label, Some(asSimpleType(arg)))
          | _ => (label, None)
        }),
        row_closed
      )
    | _ => SimpleType.Other
  }
};

let asSimpleDeclaration = (name, t) => {
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
#if 402
          cd_args->Belt.List.map(asSimpleType),
#else
          switch (cd_args) {
            | Cstr_tuple(args) =>
              args->Belt.List.map(asSimpleType)
            | Cstr_record(_) => []
          },
#endif
          switch (cd_res) {
          | None => None
          | Some(arg) => Some(asSimpleType(arg))
          }
        ))
      )
    }
  }
};

let migrateAttributes = t => {
  t.Types.type_attributes->Belt.List.map((({Asttypes.txt}, payload)) => {
    (Current.mknoloc(txt), switch payload {
      | PStr(structure) =>
        Current.PStr(Current.Parser.implementation(Current.Lexer.token, Stdlib.Lexing.from_string({
          Pprintast.structure(Stdlib.Format.str_formatter, structure);
          Stdlib.Format.flush_str_formatter()
        })))
      | PPat(pattern, guard) => Current.PPat(Current.Parser.parse_pattern(Current.Lexer.token, Stdlib.Lexing.from_string({
          Pprintast.pattern(Stdlib.Format.str_formatter, pattern)
          Stdlib.Format.flush_str_formatter()
        })), switch guard {
          | None => None
          | Some(expr) => Some(Current.Parser.parse_expression(Current.Lexer.token, Stdlib.Lexing.from_string({
            Pprintast.expression(Stdlib.Format.str_formatter, expr);
            Stdlib.Format.flush_str_formatter()
          })))
        })
      | PTyp(typ) => Current.PTyp(Current.Parser.parse_core_type(Current.Lexer.token, Stdlib.Lexing.from_string({
          Pprintast.core_type(Stdlib.Format.str_formatter, typ);
          Stdlib.Format.flush_str_formatter()
        })))
#if 402
#else
      | PSig(signature) => Current.PSig(Current.Parser.interface(Current.Lexer.token, Stdlib.Lexing.from_string({
        Pprintast.signature(Stdlib.Format.str_formatter, signature);
        Stdlib.Format.flush_str_formatter()
      })))
#endif
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

#if 402
let labelToString = label => label;
#else
let labelToString = label => switch label {
  | Asttypes.Nolabel => ""
  | Optional(label) | Labelled(label) => label
};
#endif

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
