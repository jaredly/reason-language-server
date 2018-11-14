
open Longident;
open Location;
open TypeMap;

let makeLident = (~moduleName, ~modulePath, ~name) => {
  Lident(OutputType.makeLockedTypeName(moduleName, modulePath, name))
  /* let base = switch (Str.split(Str.regexp_string("-"), moduleName)) {
    | [one, two] => Ldot(Lident(two), one)
    | [one] => Lident(one)
    | _ => failwith("Bad modulename")
  };
  let base = modulePath->Belt.List.reduce(base, (base, item) => Ldot(base, item));
  Ldot(base, name) */
};

let range = (num, fn) => {
  let rec loop = i => {
    i >= num ? [] : [fn(i), ...loop(i + 1)]
  };
  loop(0)
};

let makeTypArgs = variables =>
      variables->Belt.List.mapWithIndex((index, arg) => {
        "arg" ++ string_of_int(index)
      });

let fullName = (~moduleName, ~modulePath, ~name) =>
  Str.global_replace(
    Str.regexp_string("-"),
    "__",
    moduleName,
  )  ++ "__" ++ String.concat("__", modulePath) ++ "__" ++ name;

let transformerName = (~moduleName, ~modulePath, ~name) =>
  "deserialize_" ++ fullName(~moduleName, ~modulePath, ~name);

open Parsetree;
open Ast_helper;
open Asttypes;
open SharedTypes.SimpleType;

type transformer('source) = {
  inputType: Parsetree.core_type,
  parseVersion: Parsetree.expression,
  tuple: (Parsetree.expression, list(Parsetree.pattern), Parsetree.expression) => Parsetree.expression,
  record: (list((string, Parsetree.expression))) => Parsetree.expression,
  source: ('source) => Parsetree.expression,
  variant: list((string, int, Parsetree.expression)) => Parsetree.expression,
  list: (Parsetree.expression, Parsetree.expression) => Parsetree.expression,
};

let loc = Location.none;

let makeIdent = lident => Exp.ident(Location.mknoloc(lident));
let ok = v => Exp.construct(mknoloc(Ldot(Ldot(Lident("Belt"), "Result"), "Ok")), Some(v));
let expString = message => Exp.constant(Pconst_string(message, None));
let expError = message =>
  Exp.construct(mknoloc(Lident("Error")), Some([%expr [[%e expString(message)]]]));
let expPassError = text => Exp.construct(mknoloc(Lident("Error")), Some([%expr [[%e expString(text)], ...[%e makeIdent(Lident("error"))]]]));
let patPassError = Pat.construct(mknoloc(Lident("Error")), Some(Pat.var(mknoloc("error"))));


let failer = message => Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, expString(message))
]);

let rec forArgs = (transformer, args, body, errorName) => {
  let (res, _) = args->Belt.List.reduce((body, 0), ((body, index), arg) => {
    let argname = "arg" ++ string_of_int(index);
    (Exp.match(
      Exp.apply(forExpr(transformer, arg), [(Nolabel, makeIdent(Lident(argname)))]),
      [
        Exp.case(
          Pat.construct(Location.mknoloc(Ldot(Ldot(Lident("Belt"), "Result"), "Ok")), Some(Pat.var(Location.mknoloc(argname)))),
          body
        ),
        Exp.case(
          patPassError,
          expPassError(errorName ++ " " ++ string_of_int(index))
        )
      ]
    ), index + 1)
  });
  res
} and forExpr = (transformer, t) => switch t {
  | Variable(string) => makeIdent(Lident(string ++ "Transformer"))
  | AnonVariable => failer("Non variable")
  | Reference(source, args) =>
    switch (source, args) {
      | (DigTypes.Builtin("list"), [arg]) =>
        let loc = Location.none;
        [%expr
          (list) => [%e transformer.list(forExpr(transformer, arg), [%expr list])]
        ]
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
    let patArgs = makeTypArgs(items)->Belt.List.map(name => Pat.var(mknoloc(name)));
    let body = ok(Exp.tuple(makeTypArgs(items)->Belt.List.map(name => makeIdent(Lident(name)))));
    let body = forArgs(transformer, items, body, "tuple element");
    let loc = Location.none;
    transformer.tuple([%expr json], patArgs, body)
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
    transformer.record(items->Belt.List.map(((label, expr)) => (label, forExpr(transformer, expr))))
  | Variant(constructors) =>

    let constructors = constructors->Belt.List.map(((name, args, result)) => {
              let body = ok(Exp.constraint_(Exp.construct(mknoloc(Lident(name)), switch args {
                | [] => None
                | args => Some(Exp.tuple(args->Belt.List.mapWithIndex((index, _) => {
                  makeIdent(Lident("arg" ++ string_of_int(index)))
                })))
              }), coreType));
      (name, List.length(args), forArgs(transformer, args, body, "constructor argument"))
    });

    transformer.variant(constructors)
};

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

let makeResult = t =>
  Typ.constr(
    Location.mknoloc(Ldot(Ldot(Lident("Belt"), "Result"), "t")),
    [
      t,
      Typ.constr(
        Location.mknoloc(Lident("list")),
        [Typ.constr(Location.mknoloc(Lident("string")), [])],
      ),
    ],
  );

let decl = (transformer, ~moduleName, ~modulePath, ~name, decl) => {
  let lident = makeLident(~moduleName, ~modulePath, ~name);
  let typ =
    Typ.arrow(
      Nolabel,
      transformer.inputType,
      makeResult(
        Typ.constr(
          Location.mknoloc(lident),
          decl.variables->makeTypArgs->Belt.List.map(Typ.var),
        ),
      ),
    );
  let rec loop = (i, vbls) =>
    switch (vbls) {
    | [] => typ
    | [_, ...rest] =>
      Typ.arrow(
        Nolabel,
        Typ.arrow(
          Nolabel,
          transformer.inputType,
          makeResult(Typ.var("arg" ++ string_of_int(i))),
        ),
        loop(i + 1, rest),
      )
    };
  let typ = loop(0, decl.variables);
  let typ =
    switch (decl.variables) {
    | [] => typ
    | args => Typ.poly(makeTypArgs(decl.variables)->Belt.List.map(Location.mknoloc), typ)
    };
  let fullName = transformerName(~moduleName, ~modulePath, ~name);

  Vb.mk(
    Pat.constraint_(Pat.var(Location.mknoloc(fullName)), typ),
    declInner(transformer, lident, decl, fullName),
  );
};



