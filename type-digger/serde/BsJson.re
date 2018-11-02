
open Longident;
open Parsetree;
open Ast_helper;
open Asttypes;
open SharedTypes.SimpleType;
open TypeMap;

let makeIdent = MakeSerializer.makeIdent;

let failer = message => Exp.fun_(Nolabel, None, Pat.any(),
Exp.apply(Exp.ident(Location.mknoloc(Lident("failwith"))), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]));

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

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => MakeSerializer.failer("Not found")
  | Public({DigTypes.modulePath, moduleName, name}) =>
    makeIdent(Lident(MakeSerializer.transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("array") =>

    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("transformer")),
      Exp.fun_(
        Nolabel,
        None,
        Pat.var(Location.mknoloc("array")),
        makeJson("array",
            Exp.apply(
              makeIdent(Ldot(Ldot(Lident("Belt"), "Array"), "map")),
              [
                (Nolabel, makeIdent(Lident("array"))),
                (Nolabel, makeIdent(Lident("transformer"))),
              ]
            )
        )
      )
    )
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

let transformer = MakeSerializer.{
  source: sourceTransformer,
  list: jsonArray,
  tuple: exps => makeJson("array", Exp.array(exps)),
  record: items => jsonObject(items->Belt.List.map(((label, expr)) =>
        Exp.tuple([
          Exp.constant(Pconst_string(label, None)),
          expr
        ])
       )),
  constructor: (name, args) =>
            makeJson("array", Exp.array([
                makeJson("string", Exp.constant(Pconst_string(name, None))),
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


let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public({DigTypes.modulePath, moduleName, name}) =>
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
  MakeDeserializer.inputType: jsonT,
  source: sourceTransformer,
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
