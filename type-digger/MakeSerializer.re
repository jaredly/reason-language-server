
let transformerName = (~moduleName, ~modulePath, ~name) =>
  "transform" ++ 
  Str.global_replace(
    Str.regexp_string("-"),
    "__",
    moduleName,
  )  ++ "__" ++ String.concat("__", modulePath) ++ "__" ++ name;

open Parsetree;
open Ast_helper;
open Longident;
open Asttypes;
open SharedTypes.SimpleType;

let makeIdent = lident => Exp.ident(Location.mknoloc(lident));
let makeJson = (kind, contents) => Exp.apply(makeIdent(Ldot(Ldot(Lident("Js"), "Json"), kind)), [
  (Nolabel, contents)
]);

let jsonObject = items => makeJson("object", Exp.apply(
  makeIdent(Ldot(Ldot(Lident("Js"), "Dict"), "fromArray")),
  [(Nolabel, Exp.array(items))]
));

let failer = message => Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]);

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public({modulePath, moduleName, name}) =>
    makeIdent(Lident(transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin(name) => failer("Builtin_" ++ name)
};

let rec makeList = items => switch items {
  | [] => Exp.construct(Location.mknoloc(Lident("[]")), None)
  | [one, ...rest] => Exp.construct(Location.mknoloc(Lident("::")), Some(Exp.tuple([
    one, makeList(rest)
  ])))
}

let rec forExpr = (sourceTransformer, t) => switch t {
  | Variable(string) => makeIdent(Lident(string ++ "Transformer"))
  | AnonVariable => failer("Non variable")
  | Reference(source, args) =>
    switch args {
      | [] => sourceTransformer(source)
      | args => Exp.apply(
        sourceTransformer(source),
        args->Belt.List.map(arg => (Nolabel, forExpr(sourceTransformer, arg)))
      )
    }
  | Tuple(items) =>
    let rec loop = (i, items) => switch items {
      | [] => ([], [])
      | [arg, ...rest] =>
        let name = "arg" ++ string_of_int(i);
        let (pats, exps) = loop(i + 1, rest);
        ([
          Pat.var(Location.mknoloc(name)),
          ...pats
        ], [
          Exp.apply(forExpr(sourceTransformer, arg), [
            (Nolabel, Exp.ident(Location.mknoloc(Lident(name))))
          ]),
          ...exps
        ])
    };
    let (pats, exps) = loop(0, items);
    Exp.fun_(Nolabel, None, Pat.tuple(pats),
      makeJson("array", Exp.array(exps))
    )
  | _ => failer("not impl expr")
};

let forBody = (sourceTransformer, body) => switch body {
  | Open => failer("Cannot transform an open type")
  | Abstract => failer("Cannot transform an abstract type")
  | Expr(e) => forExpr(sourceTransformer, e)
  | Record(items) => 
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("record")),
      jsonObject(items->Belt.List.map(((label, expr)) => {
        Exp.tuple([
          Exp.constant(Pconst_string(label, None)),
          Exp.apply(
            forExpr(sourceTransformer, expr),
            [(Nolabel, Exp.field(makeIdent(Lident("record")),
            Location.mknoloc(Lident(label))
            ))]
          )
        ])
      }))
    )
  | Variant(constructors) => failer("varinat")
  | _ => failer("Not impl")
};

let declInner = (sourceTransformer, {variables, body}) => {
  let rec loop = vbls => switch vbls {
    | [] => forBody(sourceTransformer, body)
    | [arg, ...rest] =>
      Exp.fun_(Nolabel, None, Pat.var(Location.mknoloc(
        switch arg {
          | Variable(string) => string ++ "Transformer"
          | AnonVariable => "ANON"
          | _ => "OTHER"
        }
      )), loop(rest))
  };
  loop(variables)
};

let decl = (sourceTransformer, ~moduleName, ~modulePath, ~name, decl) => {
  Vb.mk(
    Pat.var(Location.mknoloc(transformerName(~moduleName, ~modulePath, ~name))),
    declInner(sourceTransformer, decl)
  )
};
