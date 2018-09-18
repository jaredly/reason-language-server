
open Longident;

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
  "transform" ++ 
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
let makeJson = (kind, contents) => Exp.apply(makeIdent(Ldot(Ldot(Lident("Js"), "Json"), kind)), [
  (Nolabel, contents)
]);

let jsonObject = items => makeJson("object_", Exp.apply(
  makeIdent(Ldot(Ldot(Lident("Js"), "Dict"), "fromArray")),
  [(Nolabel, Exp.array(items))]
));

let jsonArray = items => makeJson(
  "array",
  Exp.apply(
    makeIdent(Ldot(Ldot(Lident("Belt"), "List"), "toArray")),
    [(Nolabel, items)]
  )
);

let failer = message => Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]);

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public({modulePath, moduleName, name}) =>
    makeIdent(Lident(transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("list") =>
        
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("transformer")),
      Exp.fun_(
        Nolabel,
        None,
        Pat.var(Location.mknoloc("list")),
        jsonArray(
            Exp.apply(
              makeIdent(Ldot(Ldot(Lident("Belt"), "List"), "map")),
              [
                (Nolabel, makeIdent(Lident("list"))),
                (Nolabel, makeIdent(Lident("transformer"))),
              ]
            )
        )
      )
    )
  | Builtin("string") => makeIdent(Ldot(Ldot(Lident("Js"), "Json"), "string"))
  | Builtin("bool") => makeIdent(Ldot(Ldot(Lident("Js"), "Json"), "boolean"))
  | Builtin("int") =>
    Exp.fun_(Nolabel, None, Pat.var(Location.mknoloc("int")),
      Exp.apply(
        makeIdent(Ldot(Ldot(Lident("Js"), "Json"), "number")),
        [(Nolabel, Exp.apply(
          makeIdent(Lident("float_of_int")),
          [(Nolabel, makeIdent(Lident("int")))]
        ))]
      )
    )
  | Builtin("float") => makeIdent(Ldot(Ldot(Lident("Js"), "Json"), "number"))
  | Builtin("option") => 
  Exp.fun_(
    Nolabel,
    None,
    Pat.var(Location.mknoloc("transformer")),
  Exp.function_(
    [
      Exp.case(Pat.construct(
        Location.mknoloc(Lident("None")),
        None
      ), Exp.construct(Location.mknoloc(Ldot(Ldot(Lident("Js"), "Json"), "null")), None)),
      Exp.case(Pat.construct(
        Location.mknoloc(Lident("Some")),
        Some(Pat.var(Location.mknoloc("value")))
      ), 
        Exp.apply(makeIdent(Lident("transformer")), [
          (Nolabel, makeIdent(Lident("value")))
        ])
      )
    ]
  )

  )
  | Builtin(name) => failer("Builtin: " ++ name)
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

let forBody = (sourceTransformer, coreType, body) => switch body {
  | Open => failer("Cannot transform an open type")
  | Abstract => failer("Cannot transform an abstract type")
  | Expr(e) =>
    forExpr(sourceTransformer, e)
  | Record(items) => 
    Exp.fun_(
      Nolabel,
      None,
      Pat.constraint_(
        Pat.var(Location.mknoloc("record")),
        coreType
      ),
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
  | Variant(constructors) =>
    Exp.fun_(
      Nolabel,
      None,
      Pat.constraint_(
        Pat.var(Location.mknoloc("constructor")),
        coreType
      ),
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
            makeJson("array", Exp.array(
              [
                makeJson("string", Exp.constant(Pconst_string(name, None))),

              ]
            ))
          )
          
        })
      )
    )
};

let declInner = (sourceTransformer, typeLident, {variables, body}) => {
  let rec loop = vbls => switch vbls {
    | [] => forBody(sourceTransformer,
    Typ.constr(
      Location.mknoloc(
        typeLident,
      ),
      variables->Belt.List.mapWithIndex((index, arg) => {
        Typ.var("arg" ++ string_of_int(index))
      })
    ),
    body)
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
    declInner(sourceTransformer, 
        makeLident(~moduleName, ~modulePath, ~name)
    , decl)
  )
};
