
open Longident;
open TypeMap;

let makeLident = (~moduleName, ~modulePath, ~name) => {
  let base = switch (Str.split(Str.regexp_string("-"), moduleName)) {
    | [one, two] => Ldot(Lident(two), one)
    | [one] => Lident(one)
    | _ => failwith("Bad modulename")
  };
  let base = modulePath->Belt.List.reduce(base, (base, item) => Ldot(base, item));
  Ldot(base, name)
};

let transformerName = (~moduleName, ~modulePath, ~name) =>
  "serialize_" ++
  Str.global_replace(
    Str.regexp_string("-"),
    "__",
    moduleName,
  )  ++ "__" ++ String.concat("__", modulePath) ++ "__" ++ name;

open Parsetree;
open Ast_helper;
open Asttypes;
open SharedTypes.SimpleType;

let makeIdent = lident => Exp.ident(Location.mknoloc(lident));


type transformer('source) = {
  source: ('source) => Parsetree.expression,
  list: (Parsetree.expression) => Parsetree.expression,
  tuple: (list(Parsetree.expression)) => Parsetree.expression,
  record: (list((string, Parsetree.expression))) => Parsetree.expression,
  constructor: (string, list(Parsetree.expression)) => Parsetree.expression,
};

let failer = message => Exp.fun_(Nolabel, None, Pat.any(),
Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]));

let rec makeList = items => switch items {
  | [] => Exp.construct(Location.mknoloc(Lident("[]")), None)
  | [one, ...rest] => Exp.construct(Location.mknoloc(Lident("::")), Some(Exp.tuple([
    one, makeList(rest)
  ])))
}

let rec forExpr = (transformer, t) => switch t {
  | Variable(string) => makeIdent(Lident(string ++ "Transformer"))
  | AnonVariable => failer("Non variable")
  | Reference(source, args) =>
    switch (source, args) {
      | (DigTypes.Builtin("list"), [arg]) =>
        Exp.fun_(
          Nolabel,
          None,
          Pat.var(Location.mknoloc("list")),
          transformer.list(
              Exp.apply(
                makeIdent(Ldot(Ldot(Lident("Belt"), "List"), "map")),
                [
                  (Nolabel, makeIdent(Lident("list"))),
                  (Nolabel, forExpr(transformer, arg)),
                ]
              )
          )
        )

      | _ =>
        switch args {
          | [] => transformer.source(source)
          | args => Exp.apply(
            transformer.source(source),
            args->Belt.List.map(arg => (Nolabel, forExpr(transformer, arg)))
          )
        }
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
          Exp.apply(forExpr(transformer, arg), [
            (Nolabel, Exp.ident(Location.mknoloc(Lident(name))))
          ]),
          ...exps
        ])
    };
    let (pats, exps) = loop(0, items);
    Exp.fun_(Nolabel, None, Pat.tuple(pats),
      transformer.tuple(exps)
    )
  | _ => failer("not impl expr")
};

let forBody = (transformer, coreType, body, fullName, variables) => switch body {
  | Open => failer("Cannot transform an open type")
  | Abstract =>
    let body = makeIdent(Ldot(Lident("TransformHelpers"), fullName));
    switch (variables) {
      | [] => body
      | args => Exp.apply(body, args->Belt.List.map(
        arg => (Nolabel, makeIdent(Lident(switch arg {
          | Variable(string) => string ++ "Transformer"
          | AnonVariable => "ANON"
          | _ => "OTHER"
        })))
      ))
    }
  | Expr(e) =>
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("value")),
      Exp.apply(forExpr(transformer, e), [
        (Nolabel, makeIdent(Lident("value")))])
    )
  | Record(items) =>
    Exp.fun_(
      Nolabel,
      None,
      /* Pat.constraint_( */
        Pat.var(Location.mknoloc("record")),
        /* coreType */
      /* ), */
      transformer.record( items->Belt.List.map(((label, expr)) => {
        (label,
          Exp.apply(
            forExpr(transformer, expr),
            [(Nolabel, Exp.field(makeIdent(Lident("record")),
            Location.mknoloc(Lident(label))
            ))]
          )
        )
      })
      )
    )
  | Variant(constructors) =>
    Exp.fun_(
      Nolabel,
      None,
      /* Pat.constraint_( */
        Pat.var(Location.mknoloc("constructor")),
        /* coreType */
      /* ), */
      Exp.match(
        makeIdent(Lident("constructor")),
        constructors->Belt.List.map(((name, args, result)) => {
          Exp.case(
            Pat.construct(
              Location.mknoloc(Lident(name)),
              switch args {
                | [] => None
                | [one] => Some(Pat.var(Location.mknoloc("arg0")))
                | many => Some(Pat.tuple(
                  many->Belt.List.mapWithIndex((index, _) => (
                    Pat.var(Location.mknoloc("arg" ++ string_of_int(index)))
                  ))
                ))
              }
            ),
            transformer.constructor(
              name,
              args->Belt.List.mapWithIndex((index, arg) => {
                Exp.apply(forExpr(transformer, arg),
                [(Nolabel, makeIdent(Lident("arg" ++ string_of_int(index))))])
              })
            )
          )

        })
      )
    )
};

let makeTypArgs = variables =>
      variables->Belt.List.mapWithIndex((index, arg) => {
        "arg" ++ string_of_int(index)
      });

let declInner = (transformer, typeLident, {variables, body}, fullName) => {
  let rec loop = vbls => switch vbls {
    | [] => forBody(transformer,
    Typ.constr(
      Location.mknoloc(
        typeLident,
      ),
      makeTypArgs(variables)->Belt.List.map(name => Typ.var(name)),
    ),
    body, fullName, variables)
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

let decl = (transformer, ~moduleName, ~modulePath, ~name, decl) => {
  let lident = makeLident(~moduleName, ~modulePath, ~name);
  let typ = Typ.arrow(
        Nolabel,
        Typ.constr(
          Location.mknoloc(lident),
          decl.variables->makeTypArgs->Belt.List.map(Typ.var)
        ),
        Typ.constr(
          Location.mknoloc(Ldot(Ldot(Lident("Js"), "Json"), "t")),
          []
          )
      );
  let rec loop = (i, vbls) => switch vbls {
    | [] => typ
    | [_, ...rest] => Typ.arrow(
      Nolabel,
      Typ.arrow(Nolabel, Typ.var("arg" ++ string_of_int(i)), Typ.constr(
        Location.mknoloc(Ldot(Ldot(Lident("Js"), "Json"), "t")), []
      )),
      loop(i + 1, rest)
    )
  };
  let typ = loop(0, decl.variables);
  let typ = switch (decl.variables) {
    | [] => typ
    | args => Typ.poly(
      makeTypArgs(decl.variables)->Belt.List.map(Location.mknoloc),
      typ
    )
  };
  let fullName = transformerName(~moduleName, ~modulePath, ~name);

  Vb.mk(
    Pat.constraint_(
      Pat.var(Location.mknoloc(fullName)),
      typ,
    ),
    declInner(transformer,
        lident
    , decl, fullName)
  )
};
