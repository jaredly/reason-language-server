
open Longident;
open Parsetree;
open Ast_helper;
open Asttypes;
open TypeMap;

let makeIdent = MakeSerializer.makeIdent;

let loc = Location.none;

let failer = message => {
  [%expr (_) => print_endline([%e Exp.constant(Pconst_string(message, None))])];
};

let makeJson = (kind, contents) => Exp.apply(makeIdent(Ldot(Ldot(Lident("Js"), "Json"), kind)), [
  (Nolabel, contents)
]);

let jsonObject = items => [%expr Js.Json.object_(Js.Dict.fromArray([%e Exp.array(items)]))];
let jsonArray = items => [%expr Js.Json.array(Belt.List.toArray([%e items]))];

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

let serializeTransformer =
  MakeSerializer.{
    outputType: [%type: Js.Json.t],
    wrapWithVersion: [%expr
      (version, payload) => {
        switch (Js.Json.classify(payload)) {
        | JSONObject(dict) =>
          Js.Dict.set(dict, "$schemaVersion", Js.Json.number(float_of_int(version)));
          Js.Json.object_(dict);
        | _ => Js.Json.array([|Js.Json.number(float_of_int(version)), payload|])
        };
      }
    ],
    source: sourceTransformer,
    list: jsonArray,
    tuple: exps => [%expr Js.Json.array([%e Exp.array(exps)])],
    record: (~renames, items) =>
      jsonObject(
        items->Belt.List.map(((label, expr)) =>
          Exp.tuple([
            Exp.constant(
              Pconst_string(MakeDeserializer.getRename(~renames, label), None),
            ),
            expr,
          ])
        ),
      ),
    constructor: (~renames, name, args) =>
      [%expr Js.Json.array([%e Exp.array([
        [%expr Js.Json.string(
              [%e Exp.constant(
                Pconst_string(MakeDeserializer.getRename(~renames, name), None),
              )]
        )],
        ...args
      ])])],
  };

let declSerializer = MakeSerializer.decl(serializeTransformer);











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
    | _ => Belt.Result.Error(["Expected an array"])
  }]
};

let list = (transformer, list) => {
    let loc = Location.none;
    [%expr 
      switch (Js.Json.classify([%e list])) {
        | JSONArray(items) =>
          let transformer = [%e transformer];
          let rec loop = (i, collected, items) => switch items {
            | [] => Belt.Result.Ok(Belt.List.reverse(collected))
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(["list element " ++ string_of_int(i), ...error])
              | Belt.Result.Ok(value) => loop(i + 1, [value, ...collected], rest)
            }
          };
          loop(0, [], Belt.List.fromArray(items))
        | _ => Belt.Result.Error(["expected an array"])
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
          let rec loop = (i, collected, items) => switch items {
            | [] => Belt.Result.Ok(Belt.List.reverse(collected))
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(["list element " ++ string_of_int(i), ...error])
              | Ok(value) => loop(i + 1, [value, ...collected], rest)
            }
          };
          switch (loop(0, [], items->Belt.List.fromArray)) {
            | Belt.Result.Error(error) => Belt.Result.Error(error)
            | Ok(value) => Ok(Belt.List.toArray(value))
          }
        | _ => Belt.Result.Error(["expected an array"])
      }
    ];
  | Builtin("list") =>
    [%expr (transformer, list) => [%e list([%expr transformer], [%expr list])]]
  | Builtin("string") =>
    let loc = Location.none;
    [%expr string => switch (Js.Json.classify(string)) {
      | JSONString(string) => Belt.Result.Ok(string)
      | _ => Error(["expected a string"])
    }]
  | Builtin("bool") =>
    let loc = Location.none;
    [%expr bool => switch (Js.Json.classify(bool)) {
      | JSONTrue => Belt.Result.Ok(true)
      | JSONFalse => Belt.Result.Ok(false)
      | _ => Belt.Result.Error(["Expected a bool"])
    }]
  | Builtin("int") =>
    let loc = Location.none;
    [%expr number => switch (Js.Json.classify(number)) {
      | JSONNumber(number) => Belt.Result.Ok(int_of_float(number))
      | _ => Error(["Expected a float"])
    }]
  | Builtin("float") =>
    let loc = Location.none;
    [%expr number => switch (Js.Json.classify(number)) {
      | JSONNumber(number) => Belt.Result.Ok(number)
      | _ => Error(["Expected a float"])
    }]
  | Builtin("option") =>
    let loc = Location.none;
    [%expr (transformer, option) => switch (Js.Json.classify(option)) {
      | JSONNull => Belt.Result.Ok(None)
      | _ => switch (transformer(option)) {
        | Belt.Result.Error(error) => Belt.Result.Error(["optional value", ...error])
        | Ok(value) => Ok(Some(value))
      }
    }]
  | Builtin(name) => failer("Builtin: " ++ name)
};

let jsonT = Typ.constr(Location.mknoloc(Ldot(jsJson, "t")), []);

let deserializeTransformer = {
  MakeDeserializer.inputType: jsonT,
  source: sourceTransformer,
  parseVersion: [%expr 
    json => switch (Js.Json.classify(json)) {
      | JSONObject(dict) => switch (Js.Dict.get(dict, "$schemaVersion")) {
        | Some(schemaVersion) => switch (Js.Json.classify(schemaVersion)) {
        | JSONNumber(version) => [@implicit_arity]Belt.Result.Ok((int_of_float(version), json))
        | _ => Belt.Result.Error("Invalid $schemaVersion")
        }
        | None => Belt.Result.Error("No $schemaVersion present")
      }
      | JSONArray([|version, payload|]) => switch (Js.Json.classify(version)) {
        | JSONNumber(version) => [@implicit_arity]Belt.Result.Ok((int_of_float(version), payload))
        | _ => Belt.Result.Error("Invalid wrapped version")
      }
      | _ => Belt.Result.Error("Must have a schema version")
    }
  ],
  list,
  tuple,
  record: (~renames, items) =>  {
    let body =
      MakeDeserializer.ok(
        Exp.record(
          items->Belt.List.map(((label, _, _)) => (Location.mknoloc(Lident(label)), makeIdent(Lident("attr_" ++ label)))),
          None,
        ),
      );
    let body = items->Belt.List.reduce(body, (body, (label, inner, isOptional)) => {
      /* let inner = forExpr(expr); */
      let loc = Location.none;
      let attrName = MakeDeserializer.getRename(~renames, label);
      [%expr {
        let inner = ([%p Pat.var(Location.mknoloc("attr_" ++ label))]) => [%e body];
        switch (Js.Dict.get(dict, [%e MakeDeserializer.expString(attrName)])) {
        | None =>
          if%e (isOptional) {
            [%expr inner(None)]
          } else {
            [%expr Belt.Result.Error([[%e MakeDeserializer.expString("No attribute '" ++ attrName ++ "'")]])]
          }
        | Some(json) => switch ([%e inner](json)) {
          | Belt.Result.Error(error) => Belt.Result.Error([[%e MakeDeserializer.expString("attribute '" ++ attrName ++ "'")], ...error])
          | Ok(data) => inner(data)
        }
      }
      }]
    });
    let loc = Location.none;
    [%expr record => switch (Js.Json.classify(record)) {
      | JSONObject(dict) => [%e body]
      | _ => Belt.Result.Error(["Expected an object"])
    }]
  },
  variant: (~renames, constructors) => {
    let cases = constructors->Belt.List.map(((name, argCount, argTransformer)) => {
      let pat =
        Pat.construct(
          Location.mknoloc(Lident("JSONArray")),
          Some(
            Pat.array([
              Pat.var(Location.mknoloc("tag")),
              ...MakeDeserializer.range(argCount, index =>
                   Pat.var(Location.mknoloc("arg" ++ string_of_int(index)))
                 ),
            ]),
          ),
        );
      Exp.case(
        pat,
        ~guard=Exp.apply(makeIdent(Lident("=")), [
          (Nolabel, [%expr Js.Json.JSONString([%e Exp.constant(Pconst_string(MakeDeserializer.getRename(~renames, name), None))])]),
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
  },
};


let declDeserializer = MakeDeserializer.decl(deserializeTransformer);
