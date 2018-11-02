
open Longident;
open Location;
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

let transformerName = (~moduleName, ~modulePath, ~name) =>
  "deserialize_" ++
  Str.global_replace(
    Str.regexp_string("-"),
    "__",
    moduleName,
  )  ++ "__" ++ String.concat("__", modulePath) ++ "__" ++ name;

open Parsetree;
open Ast_helper;
open Asttypes;
open SharedTypes.SimpleType;

type transformer('source) = {
  inputType: Parsetree.core_type,
  record: (list((string, Parsetree.expression))) => Parsetree.expression,
  source: ('source) => Parsetree.expression,
  variant: list((string, int, Parsetree.expression)) => Parsetree.expression,
};

let makeIdent = lident => Exp.ident(Location.mknoloc(lident));
let ok = v => Exp.construct(mknoloc(Ldot(Ldot(Lident("Belt"), "Result"), "Ok")), Some(v));
let expString = message => Exp.constant(Pconst_string(message, None));
let expError = message => Exp.construct(mknoloc(Lident("Error")), Some(expString(message)));
let expPassError = Exp.construct(mknoloc(Lident("Error")), Some(makeIdent(Lident("error"))));
let patPassError = Pat.construct(mknoloc(Lident("Error")), Some(Pat.var(mknoloc("error"))));


let failer = message => Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, expString(message))
]);

let rec forArgs = (transformer, args, body) => {
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
          /* TODO annotate error */
          expPassError
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
          (list) => switch (Js.Json.classify(list)) {
            | JSONArray(items) =>
              let transformer = [%e forExpr(transformer, arg)];
              let rec loop = items => switch items {
                | [] => Belt.Result.Ok([])
                | [one, ...rest] => switch (transformer(one)) {
                  | Belt.Result.Error(error) => Belt.Result.Error(error)
                  | Ok(value) => switch (loop(rest)) {
                    | Belt.Result.Error(error) => Belt.Result.Error(error)
                    | Ok(rest) => Ok([value, ...rest])
                  }
                }
              };
              loop(Belt.List.fromArray(items))
            | _ => Belt.Result.Error("expected an array")
          }
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
    let body = forArgs(transformer, items, body);
    let loc = Location.none;
    [%expr json => switch (Js.Json.classify(json)) {
      | JSONArray([%p Pat.array(patArgs)]) => [%e body]
      | _ => Belt.Result.Error("Expected array")
    }]
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
      (name, List.length(args), forArgs(transformer, args, body))
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

let makeResult = t => Typ.constr(
  Location.mknoloc(Ldot(Ldot(Lident("Belt"), "Result"), "t")),
  [ t, Typ.constr(Location.mknoloc(Lident("string")), []) ]
  );

let decl = (transformer, ~moduleName, ~modulePath, ~name, decl) => {
  let lident = makeLident(~moduleName, ~modulePath, ~name);
  let typ = Typ.arrow(
        Nolabel,
        transformer.inputType,
        makeResult(
        Typ.constr(
          Location.mknoloc(lident),
          decl.variables->makeTypArgs->Belt.List.map(Typ.var)
        ),
        )
      );
  let rec loop = (i, vbls) => switch vbls {
    | [] => typ
    | [_, ...rest] => Typ.arrow(
      Nolabel,
      Typ.arrow(
        Nolabel,
        transformer.inputType,
        makeResult(Typ.var("arg" ++ string_of_int(i))),
      ),
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





let jsJson = Ldot(Lident("Js"), "Json");

let makeJson = (kind, contents) => Exp.apply(makeIdent(Ldot(jsJson, kind)), [
  (Nolabel, contents)
]);

let makeClassify = makeJson("classify");

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


let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public({DigTypes.modulePath, moduleName, name}) =>
    makeIdent(Lident(transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("array") =>
    let loc = Location.none;
    [%expr
      (transformer, array) => switch (Js.Json.classify(array)) {
        | JSONArray(items) =>
          let rec loop = items => switch items {
            | [] => Belt.Result.Ok([])
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(error)
              | Ok(value) => switch (loop(rest)) {
                | Belt.Result.Error(error) => Belt.Result.Error(error)
                | Ok(rest) => Ok([value, ...rest])
              }
            }
          };
          switch (loop(items->Belt.List.fromArray)) {
            | Belt.Result.Error(error) => Belt.Result.Error(error)
            | Ok(value) => Ok(Belt.List.toArray(value))
          }
        | _ => Belt.Result.Error("expected an array")
      }
    ];
  | Builtin("list") =>
    let loc = Location.none;
    [%expr
      (transformer, list) => switch (Js.Json.classify(list)) {
        | JSONArray(items) =>
          let rec loop = items => switch items {
            | [] => Ok([])
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(error)
              | Ok(value) => switch (loop(rest)) {
                | Belt.Result.Error(error) => Belt.Result.Error(error)
                | Ok(rest) => Ok([value, ...rest])
              }
            }
          };
          loop(items)
        | _ => Belt.Result.Error("expected an array")
      }
    ];
  | Builtin("string") =>
    let loc = Location.none;
    [%expr string => switch (Js.Json.classify(string)) {
      | JSONString(string) => Belt.Result.Ok(string)
      | _ => Error("epected a string")
    }]
  | Builtin("bool") =>
    let loc = Location.none;
    [%expr bool => switch (Js.Json.classify(bool)) {
    | JSONTrue => Belt.Result.Ok(true)
    | JSONFalse => Belt.Result.Ok(false)
    | _ => Belt.Result.Error("Expected a bool")
  }]
  | Builtin("int") =>
    let loc = Location.none;
    [%expr number => switch (Js.Json.classify(number)) {
      | JSONNumber(number) => Belt.Result.Ok(int_of_float(number))
      | _ => Error("Expected a float")
    }]
  | Builtin("float") =>
    let loc = Location.none;
    [%expr number => switch (Js.Json.classify(number)) {
      | JSONNumber(number) => Belt.Result.Ok(number)
      | _ => Error("Expected a float")
    }]
  | Builtin("option") =>
    let loc = Location.none;
    [%expr (transformer, option) => switch (Js.Json.classify(option)) {
      | JSONNull => Belt.Result.Ok(None)
      | _ => switch (transformer(option)) {
        | Belt.Result.Error(error) => Belt.Result.Error(error)
        | Ok(value) => Ok(Some(value))
      }
    }]
  | Builtin(name) => failer("Builtin: " ++ name)
};


let jsonT = Typ.constr(Location.mknoloc(Ldot(jsJson, "t")), []);

let transformer = {
  inputType: jsonT,
  source: sourceTransformer,
  record: (items) =>  {
    let body =
      ok(
        Exp.record(
          items->Belt.List.map(((label, _)) => (mknoloc(Lident(label)), makeIdent(Lident("attr_" ++ label)))),
          None,
        ),
      );
    let body = items->Belt.List.reduce(body, (body, (label, inner)) => {
      /* let inner = forExpr(expr); */
      let loc = Location.none;
      [%expr switch (Js.Dict.get(dict, [%e expString(label)])) {
        | None => Belt.Result.Error("No attribute " ++ [%e expString(label)])
        | Some(json) => switch ([%e inner](json)) {
          | Belt.Result.Error(error) => Belt.Result.Error(error)
          | Ok([%p Pat.var(mknoloc("attr_" ++ label))]) => [%e body]
        }
      }]
    });
    let loc = Location.none;
    [%expr record => switch (Js.Json.classify(record)) {
      | JSONObject(dict) => [%e body]
      | _ => Belt.Result.Error("Expected an object")
    }]
  },
  variant: (constructors) => {

    let cases = constructors->Belt.List.map(((name, argCount, argTransformer)) => {
          Exp.case(
            Pat.construct(Location.mknoloc(Lident("JSONArray")), Some(
              Pat.array(
                [
                  Pat.var(Location.mknoloc("tag")),
                  ...range(argCount, index => {
                    Pat.var(Location.mknoloc("arg" ++ string_of_int(index)))
                  })
                ]
              ),
            )),
            ~guard=Exp.apply(makeIdent(Lident("=")), [
              (Nolabel, Exp.construct(mknoloc(Ldot(jsJson, "JSONString")), Some(Exp.constant(Pconst_string(name, None))))),
              (Nolabel, makeClassify(makeIdent(Lident("tag"))))
            ]),
            argTransformer
          )
        });

    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("constructor")),
      Exp.match(
        makeClassify(makeIdent(Lident("constructor"))),
        cases->Belt.List.concat([
          Exp.case(Pat.any(), expError("Expected an array"))
        ])
      )
    )
  }
};