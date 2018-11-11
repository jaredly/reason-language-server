
open Longident;
open Parsetree;
open Ast_helper;
open Asttypes;
open SharedTypes.SimpleType;
open TypeMap;

let makeIdent = MakeSerializer.makeIdent;

let loc = Location.none;

let failer = message => {
  [%expr (_) => failwith([%e Ast_helper.Exp.constant(Pconst_string(message, None))])];
};

let makeJson = (kind, contents) => Ast_helper.Exp.apply(makeIdent(Ldot(Ldot(Lident("Js"), "Json"), kind)), [
  (Nolabel, contents)
]);

let jsonObject = items => makeJson("object_", Ast_helper.Exp.apply(
  makeIdent(Ldot(Ldot(Lident("Js"), "Dict"), "fromArray")),
  [(Nolabel, Ast_helper.Exp.array(items))]
));

let jsonArray = items => makeJson(
  "array",
  Exp.apply(
    makeIdent(Ldot(Ldot(Lident("Belt"), "List"), "toArray")),
    [(Nolabel, items)]
  )
);

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => MakeSerializer.failer("Not found")
  | Public((moduleName, modulePath, name)) =>
    makeIdent(Lident(MakeSerializer.transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("array") =>
    [%expr (transformer, array) => Js.Json.array(array->Belt.Array.map(transformer))]
  | Builtin("list") =>
    [%expr (transformer, list) => Js.Json.array(
      list->Belt.List.map(transformer)->Belt.List.toArray
    )]
  | Builtin("string") => [%expr Js.Json.string]
  | Builtin("bool") => [%expr Js.Json.boolean]
  | Builtin("int") => [%expr int => Js.Json.number(float_of_int(int))]
  | Builtin("float") => [%expr Js.Json.number]
  | Builtin("option") => [%expr (transformer) => fun
    | Some(inner) => transformer(inner)
    | None => Js.Json.null
    ]
  | Builtin(name) => failer("Builtin: " ++ name)
};

let transformer = MakeSerializer.{
  outputType: Typ.constr(
          Location.mknoloc(Ldot(Ldot(Lident("Js"), "Json"), "t")),
          []
          ),
  source: sourceTransformer,
  list: jsonArray,
  tuple: exps => makeJson("array", Ast_helper.Exp.array(exps)),
  record: items => jsonObject(items->Belt.List.map(((label, expr)) =>
        Ast_helper.Exp.tuple([
          Ast_helper.Exp.constant(Pconst_string(label, None)),
          expr
        ])
       )),
  constructor: (name, args) => 
            makeJson("array", Ast_helper.Exp.array([
                makeJson("string", Ast_helper.Exp.constant(Pconst_string(name, None))),
              ] @ args
            ))
};

let declSerializer = MakeSerializer.decl(transformer);











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

let tuple = (value, patArgs, body) => {
  [%expr json => switch (Js.Json.classify([%e value])) {
    | JSONArray([%p Pat.array(patArgs)]) => [%e body]
    | _ => Belt.Result.Error("Expected array")
  }]
};

let list = (transformer, list) => {
    let loc = Location.none;
    [%expr 
      switch (Js.Json.classify([%e list])) {
        | JSONArray(items) =>
          let transformer = [%e transformer];
          let rec loop = items => switch items {
            | [] => Belt.Result.Ok([])
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(error)
              | Belt.Result.Ok(value) => switch (loop(rest)) {
                | Belt.Result.Error(error) => Belt.Result.Error(error)
                | Belt.Result.Ok(rest) => Belt.Result.Ok([value, ...rest])
              }
            }
          };
          loop(Belt.List.fromArray(items))
        | _ => Belt.Result.Error("expected an array")
      }
    ];
};

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public((moduleName, modulePath, name)) =>
    makeIdent(Lident(MakeDeserializer.transformerName(~moduleName, ~modulePath, ~name)))
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
    [%expr (transformer, list) => [%e list([%expr transformer], [%expr list])]]
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
  MakeDeserializer.inputType: jsonT,
  source: sourceTransformer,
  list,
  tuple,
  record: (items) =>  {
    let body =
      MakeDeserializer.ok(
        Exp.record(
          items->Belt.List.map(((label, _)) => (Location.mknoloc(Lident(label)), makeIdent(Lident("attr_" ++ label)))),
          None,
        ),
      );
    let body = items->Belt.List.reduce(body, (body, (label, inner)) => {
      /* let inner = forExpr(expr); */
      let loc = Location.none;
      [%expr switch (Js.Dict.get(dict, [%e MakeDeserializer.expString(label)])) {
        | None => Belt.Result.Error("No attribute " ++ [%e MakeDeserializer.expString(label)])
        | Some(json) => switch ([%e inner](json)) {
          | Belt.Result.Error(error) => Belt.Result.Error(error)
          | Ok([%p Pat.var(Location.mknoloc("attr_" ++ label))]) => [%e body]
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
                  ...MakeDeserializer.range(argCount, index => {
                    Pat.var(Location.mknoloc("arg" ++ string_of_int(index)))
                  })
                ]
              ),
            )),
            ~guard=Exp.apply(makeIdent(Lident("=")), [
              (Nolabel, Exp.construct(Location.mknoloc(Ldot(jsJson, "JSONString")), Some(Exp.constant(Pconst_string(name, None))))),
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
          Exp.case(Pat.any(), MakeDeserializer.expError("Expected an array"))
        ])
      )
    )
  }
};


let declDeserializer = MakeDeserializer.decl(transformer);
