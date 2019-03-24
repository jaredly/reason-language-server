
open Longident;
open Location;
open TypeMap;

let makeLident = (~moduleName, ~modulePath, ~name) => {
  Lident(OutputType.makeLockedTypeName(moduleName, modulePath, name))
};

let getRename = (~renames, name) => switch (renames->Belt.List.getAssoc(name, (==))) {
  | None => name
  | Some(l) => l
};

let range = (num, fn) => {
  let rec loop = i => {
    i >= num ? [] : [fn(i), ...loop(i + 1)]
  };
  loop(0)
};

let makeTypArgs = variables =>
      variables->Belt.List.mapWithIndex((index, _arg) => {
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
  record: (~renames: list((string, string)), ~typ: Parsetree.core_type, list((string, Parsetree.expression, bool))) => Parsetree.expression,
  source: ('source) => Parsetree.expression,
  variant: (~renames: list((string, string)), list((string, int, Parsetree.expression))) => Parsetree.expression,
  list: (Parsetree.expression, Parsetree.expression) => Parsetree.expression,
};

let loc = Location.none;

let makeIdent = lident => Exp.ident(Location.mknoloc(lident));
let ok = v => [%expr Belt.Result.Ok([%e v])];
let expString = message => Exp.constant(Pconst_string(message, None));
let expError = message => [%expr Belt.Result.Error([[%e expString(message)]])];
let expPassError = text => [%expr Belt.Result.Error([[%e expString(text)], ...error])];
let patPassError = [%pat? Error(error)];


let failer = message => Exp.apply(Exp.ident(Location.mknoloc(Lident("print_endline"))), [
  (Nolabel, expString(message))
]);

let rec forArgs = (~renames, transformer, args, body, errorName) => {
  let (res, _) = args->Belt.List.reduce((body, 0), ((body, index), arg) => {
    let argname = "arg" ++ string_of_int(index);
    (Exp.match(
      Exp.apply(forExpr(~renames, transformer, arg), [(Nolabel, makeIdent(Lident(argname)))]),
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
} and forExpr = (~renames, transformer, t) => switch t {
  | Variable(string) => makeIdent(Lident(string ++ "Transformer"))
  | AnonVariable => failer("Non variable")
  | Reference(source, args) =>
    switch (source, args) {
      | (DigTypes.Builtin("list"), [arg]) =>
        let loc = Location.none;
        [%expr
          (list) => [%e transformer.list(forExpr(~renames, transformer, arg), [%expr list])]
        ]
      | _ =>
        switch args {
          | [] => transformer.source(source)
          | args => Exp.apply(
            transformer.source(source),
            args->Belt.List.map(arg => (Nolabel, forExpr(~renames, transformer, arg)))
          )
        }
    }
  | Tuple(items) =>
    let patArgs = makeTypArgs(items)->Belt.List.map(name => Pat.var(mknoloc(name)));
    let body = ok(Exp.tuple(makeTypArgs(items)->Belt.List.map(name => makeIdent(Lident(name)))));
    let body = forArgs(~renames, transformer, items, body, "tuple element");
    let loc = Location.none;
    transformer.tuple([%expr json], patArgs, body)
  | RowVariant(rows, closed) =>
    let rows =
      rows->Belt.List.map(((name, arg)) => {
        let body = switch arg {
          | None => ok(Exp.variant(name, None))
          | Some(arg) => {
            [%expr switch ([%e forExpr(~renames, transformer, arg)](arg0)) {
              | Belt.Result.Ok(arg) => Belt.Result.Ok([%e Exp.variant(name, Some([%expr arg]))])
              | Error(error) => Belt.Result.Error(error)
            }]
          }
        };
        (name, arg == None ? 0 : 1, body);
      });

    transformer.variant(~renames, rows)
  | _ => failer("not impl expr")
};

let forBody = (~helpers, ~renames, transformer, coreType, body, fullName, variables) => switch body {
  | Open => failer("Cannot transform an open type")
  | Abstract =>
    let moduleName = switch helpers {
      | None => failwith("Abstract type found, but no 'helpers' module specified for this engine")
      | Some(name) => name;
    };
    let body = makeIdent(Ldot(Lident(moduleName), fullName));
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
      Exp.apply(forExpr(~renames, transformer, e), [
        (Nolabel, makeIdent(Lident("value")))])
    )
  | Record(items) =>
    transformer.record(~renames, ~typ=coreType, items->Belt.List.map(((label, expr)) => (label, forExpr(~renames, transformer, expr), {
      switch expr {
        | Reference(Builtin("option"), [_]) => true
        | _ => false
      }
    })))
  | Variant(constructors) =>
    let constructors =
      constructors->Belt.List.map(((name, args, _result)) => {
        let body =
          ok(
            Exp.constraint_(
              Exp.construct(
                mknoloc(Lident(name)),
                switch (args) {
                | [] => None
                | args =>
                  Some(
                    Exp.tuple(
                      args->Belt.List.mapWithIndex((index, _) =>
                        makeIdent(Lident("arg" ++ string_of_int(index)))
                      ),
                    ),
                  )
                },
              ),
              coreType,
            ),
          );
        (name, List.length(args), forArgs(~renames, transformer, args, body, "constructor argument"));
      });

    transformer.variant(~renames, constructors)
};

let declInner = (~helpers, ~renames, transformer, typeLident, {variables, body}, fullName) => {
  let rec loop = vbls => switch vbls {
    | [] => forBody(~helpers, ~renames, transformer,
    Typ.constr(
      Location.mknoloc(
        typeLident,
      ),
      makeTypArgs(variables)->Belt.List.map(name => Typ.constr(Location.mknoloc(Lident(name)), [])),
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

let rec makeFunctionType = (base, inputType, vbls) =>
  switch (vbls) {
  | [] => base
  | [vbl, ...rest] =>
    Typ.arrow(
      Nolabel,
      Typ.arrow(
        Nolabel,
        inputType,
        makeResult(vbl),
      ),
      makeFunctionType(base, inputType, rest),
    )
  };

let makeArrow = (inputType, lident, variables) => {
  Typ.arrow(
    Nolabel,
    inputType,
    makeResult(
      Typ.constr(
        Location.mknoloc(lident),
        variables
      ),
    ),
  );
};

let declOuter = (inputType, variables, ~moduleName, ~modulePath, ~name, inner) => {
  let lident = makeLident(~moduleName, ~modulePath, ~name);
  let asTypeVariables = 
          variables->makeTypArgs->Belt.List.map(Typ.var);
  let typ = makeArrow(inputType, lident, asTypeVariables);
  let typ = makeFunctionType(typ, inputType, asTypeVariables);
  let typ =
    switch (variables) {
    | [] => typ
    | _args => Typ.poly(makeTypArgs(variables)->Belt.List.map(Location.mknoloc), typ)
    };
  let fullName = transformerName(~moduleName, ~modulePath, ~name);

  let inner = variables == [] ? inner : {
    let asTypeConstrs =
      variables
      ->makeTypArgs
      ->Belt.List.map(name => Typ.constr(Location.mknoloc(Lident(name)), []));

    let inner = Ast_helper.Exp.constraint_(
      inner,
      makeFunctionType(
        makeArrow(inputType, lident, asTypeConstrs),
        inputType, asTypeConstrs
      )
    );

    makeTypArgs(variables)
    ->Belt.List.reduce(
      inner, (body, arg) =>
        Ast_helper.Exp.newtype(Location.mknoloc(arg), body)
      );
  };

  Vb.mk(
    Pat.constraint_(Pat.var(Location.mknoloc(fullName)), typ),
    inner,
  );
};

let decl = (transformer, ~helpers, ~renames, ~moduleName, ~modulePath, ~name, ~inner, decl) => {
  let lident = makeLident(~moduleName, ~modulePath, ~name);
  let fullName = transformerName(~moduleName, ~modulePath, ~name);
  declOuter(
    transformer.inputType,
    decl.variables,
    ~moduleName,
    ~modulePath,
    ~name,
    switch inner {
      | None => declInner(~helpers, ~renames, transformer, lident, decl, fullName)
      | Some(inner) => inner
    }
  )
};



