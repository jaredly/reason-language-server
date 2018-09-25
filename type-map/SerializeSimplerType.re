
open SharedTypes.SimpleType;
open Lib.Rpc.J;

let sourceToJson = source => switch source {
  | DigTypes.NotFound => s("NotFound")
  | Public({uri, modulePath, name, moduleName}) => o([
    ("uri", s(uri)),
    ("moduleName", s(moduleName)),
    ("modulePath", l(modulePath->Belt.List.map(s))),
    ("name", s(name)),
  ])
  | DigTypes.Builtin(name) => o([("builtin", s(name))])
};

let rec toJson = (sourceToJson, t) => switch t {
  | Variable(string) => l([s("Variable"), s(string)])
  | AnonVariable => s("AnonVariable")
  | Reference(source, args) => l([
    s("Reference"),
    sourceToJson(source),
    l(args->Belt.List.map(toJson(sourceToJson))),
  ])
  | Tuple(items) => l([s("Tuple"), l(items->Belt.List.map(toJson(sourceToJson)))])
  | Fn(args, result) => l([
    s("Fn"),
    l(args->Belt.List.map(((label, arg)) => o([
      ("arg", toJson(sourceToJson, arg)),
      ("label", switch label {
        | None => Vendor.Json.Null
        | Some(text) => s(text)
      })
    ]))),
    toJson(sourceToJson, result)
  ])
  | Other => s("*other*")
};

let rec exprToJson = (sourceToJson, body) => {
switch body {
    | Open => s("Open")
    | Abstract => s("Abstract")
    | Expr(expr) => l([s("Expr"), toJson(sourceToJson, expr)])
    | Record(items) => l([s("Record"), o(
      items->Belt.List.map(((label, typ)) => (label, toJson(sourceToJson, typ)))
    )])
    | Variant(constructors) => l([s("Variant"), l(
      constructors->Belt.List.map(((name, args, res)) => o([
        ("name", s(name)),
        ("args", l(args->Belt.List.map(toJson(sourceToJson)))),
        ("result", switch res {
          | None => Vendor.Json.Null
          | Some(arg) => toJson(sourceToJson, arg)
        })
      ]))
    )])
  }
};

let rec declToJson = (sourceToJson, {name, variables, body}) => o([
  ("name", s(name)),
  ("variables", l(variables->Belt.List.map(toJson(sourceToJson)))),
  ("body", exprToJson(sourceToJson, body))
]);


