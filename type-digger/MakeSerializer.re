
let transformerName = (~moduleName, ~modulePath, ~name) =>
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

let failer = message => Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]);

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public({modulePath, moduleName, name}) =>
    Exp.ident(Location.mknoloc(Lident(transformerName(~moduleName, ~modulePath, ~name))))
  | Builtin(name) => failer("Builtin_" ++ name)
};

let rec expr = (sourceTransformer, t) => switch t {
  | Variable(string) => failer("transform vbl " ++ string)
  | AnonVariable => failer("Non variable")
  | Reference(source, args) =>
    switch args {
      | [] => sourceTransformer(source)
      | args => Exp.apply(
        sourceTransformer(source),
        args->Belt.List.map(arg => (Nolabel, expr(sourceTransformer, arg)))
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
          Exp.apply(expr(sourceTransformer, arg), [
            (Nolabel, Exp.ident(Location.mknoloc(Lident(name))))
          ]),
          ...exps
        ])
    };
    let (pats, exps) = loop(0, items);
    Exp.fun_(Nolabel, None, Pat.tuple(pats), Exp.tuple(exps))
  | _ => failer("not impl expr")
};

let forBody = (sourceTransformer, body) => switch body {
  | Open => failer("Cannot transform an open type")
  | Abstract => failer("Cannot transform an abstract type")
  | Expr(e) => expr(sourceTransformer, e)
  | Record(items) => failer("record")
  | Variant(constructors) => failer("varinat")
  | _ => failer("Not impl")
};

let declInner = (sourceTransformer, {variables, body}) => {
  let rec loop = vbls => switch vbls {
    | [] => forBody(sourceTransformer, body)
    | [arg, ...rest] =>
      Exp.fun_(Nolabel, None, Pat.var(Location.mknoloc(
        switch arg {
          | Variable(string) => string
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
