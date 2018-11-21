
open Longident;
open Parsetree;
open Ast_helper;
open Asttypes;
open SharedTypes.SimpleType;
open TypeMap;

let loc = Location.none;

let makeIdent = MakeSerializer.makeIdent;

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

let makeJson = (kind, contents) => Exp.construct(Location.mknoloc(Ldot(Lident("Json"), kind)), contents);

let jsonObject = items => makeJson("Object", Some(makeList(items)));

let jsonArray = items => makeJson(
  "Array",
  Some(items)
);

let sourceTransformer = source => switch source {
  | DigTypes.NotFound => MakeSerializer.failer("Not found")
  | Public((moduleName, modulePath, name)) =>
    makeIdent(Lident(MakeSerializer.transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("array") =>
    [%expr (transformer, array) => Json.Array(
      Belt.List.fromArray(Belt.Array.map(array, transformer))
    )]
  | Builtin("list") =>
    [%expr (transformer, list) => Json.Array(Belt.List.map(list, transformer))]
  | Builtin("string") => [%expr s => Json.String(s)]
  | Builtin("bool") => [%expr b => b ? Json.True : Json.False]
  | Builtin("int") => [%expr i => Json.Number(float_of_int(i))]
  | Builtin("float") => [%expr f => Json.Number(f)]
  | Builtin("option") =>  [%expr (transformer) => fun
    | None => Json.Null
    | Some(v) => transformer(v)]
  | Builtin(name) => failer("Builtin: " ++ name)
};

let serializeTransformer = MakeSerializer.{
  outputType: Typ.constr(Location.mknoloc(Ldot(Lident("Json"), "t")), []),
  wrapWithVersion: [%expr 
    (version, payload) => Json.Array([Json.Number(version), payload])
  ],
  source: sourceTransformer,
  list: jsonArray,
  tuple: exps => makeJson("Array", Some(makeList(exps))),
  record: (~renames, items) => jsonObject(items->Belt.List.map(((label, expr)) =>
        Exp.tuple([
          Exp.constant(Pconst_string(MakeDeserializer.getRename(~renames, label), None)),
          expr
        ])
       )),
  constructor: (~renames, name, args) => 
            makeJson("Array", Some(makeList([
                makeJson("String", Some(Exp.constant(Pconst_string(MakeDeserializer.getRename(~renames, name), None)))),
              ] @ args
            )))
};

let declSerializer = MakeSerializer.decl(serializeTransformer);










let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public((moduleName, modulePath, name)) =>
    makeIdent(Lident(MakeDeserializer.transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("array") =>
    [%expr 
      (transformer, array) => switch (array) {
        | Json.Array(items) =>
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
          switch (loop(items)) {
            | Belt.Result.Error(error) => Belt.Result.Error(error)
            | Belt.Result.Ok(value) => Belt.Result.Ok(Belt.List.toArray(value))
          }
        | _ => Belt.Result.Error("expected an array")
      }
    ];
  | Builtin("list") =>
    [%expr 
      (transformer, list) => switch (list) {
        | Json.Array(items) =>
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
          loop(items)
        | _ => Belt.Result.Error("expected an array")
      }
    ];
  | Builtin("string") =>
    [%expr string => switch (string) {
      | Json.String(string) => Belt.Result.Ok(string)
      | _ => Error("epected a string")
    }]
  | Builtin("bool") => [%expr bool => switch (bool) {
    | Json.True => Belt.Result.Ok(true)
    | Json.False => Belt.Result.Ok(false)
    | _ => Belt.Result.Error("Expected a bool")
  }]
  | Builtin("int") =>
    [%expr number => switch (number) {
      | Json.Number(number) => Belt.Result.Ok(int_of_float(number))
      | _ => Error("Expected a float")
    }]
  | Builtin("float") => [%expr number => switch (number) {
    | Json.Number(number) => Belt.Result.Ok(number)
    | _ => Error("Expected a float")
  }]
  | Builtin("option") => 
    [%expr (transformer, option) => switch (option) {
      | Json.Null => Belt.Result.Ok(None)
      | _ => switch (transformer(option)) {
        | Belt.Result.Error(error) => Belt.Result.Error(error)
        | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
      }
    }]
  | Builtin(name) => failer("Builtin: " ++ name)
};

let rec makePatList = items => switch items {
  | [] => Pat.construct(Location.mknoloc(Lident("[]")), None)
  | [one, ...rest] => Pat.construct(Location.mknoloc(Lident("::")), Some(Pat.tuple([
    one, makePatList(rest)
  ])))
};


let jsonT = [%type: Json.t];

let deserializeTransformer = {
  MakeDeserializer.inputType: jsonT,
  source: sourceTransformer,
  parseVersion: [%expr
    json => switch json {
      | Json.Array([Json.Number(version), payload]) => Belt.Result.Ok((version, payload))
      | _ => Belt.Result.Error("Not wrapped in a version")
    }
  ],
  tuple: (value, patArgs, body) => [%expr json => switch ([%e value]) {
    | Json.Array([%p makePatList(patArgs)]) => [%e body]
    | _ => Belt.Result.Error("Expected array")
  }],
  list: (transformer, list) => {
    [%expr 
      switch ([%e list]) {
        | Json.Array(items) =>
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
          loop(items)
        | _ => Belt.Result.Error("expected an array")
      }
    ];

  },
  record: (~renames, items) =>  {
    let body =
      MakeDeserializer.ok(
        Exp.record(
          items->Belt.List.map(((label, _)) => (Location.mknoloc(Lident(label)), makeIdent(Lident("attr_" ++ label)))),
          None,
        ),
      );
    let body = items->Belt.List.reduce(body, (body, (label, inner)) => {
      let attrName = MakeDeserializer.getRename(~renames, label);
      /* let inner = forExpr(expr); */
      [%expr switch (Belt.List.getAssoc(items, [%e MakeDeserializer.expString(attrName)], (==))) {
        | None => Belt.Result.Error("No attribute " ++ [%e MakeDeserializer.expString(attrName)])
        | Some(json) => switch ([%e inner](json)) {
          | Belt.Result.Error(error) => Belt.Result.Error(error)
          | Belt.Result.Ok([%p Pat.var(Location.mknoloc("attr_" ++ label))]) => [%e body]
        }
      }]
    });
    [%expr record => switch (record) {
      | Json.Object(items) => [%e body]
      | _ => Belt.Result.Error("Expected an object")
    }]
  },
  variant: (~renames, constructors) => {

    let cases = constructors->Belt.List.map(((name, argCount, argTransformer)) => {
          let constrName = MakeDeserializer.getRename(~renames, name);
          Exp.case(
            Pat.construct(Location.mknoloc(Ldot(Lident("Json"), "Array")), Some(
              makePatList([
                Pat.var(Location.mknoloc("tag")),
                ...MakeDeserializer.range(argCount, index => {
                  Pat.var(Location.mknoloc("arg" ++ string_of_int(index)))
                })
              ])
            )),
            ~guard=Exp.apply(makeIdent(Lident("=")), [
              (Nolabel, Exp.construct(Location.mknoloc(Ldot(Lident("Json"), "String")), Some(Exp.constant(Pconst_string(constrName, None))))),
              (Nolabel, makeIdent(Lident("tag")))
            ]),
            argTransformer
          )
        });

    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("constructor")),
      Exp.match(
        makeIdent(Lident("constructor")),
        cases->Belt.List.concat([
          Exp.case(Pat.any(), MakeDeserializer.expError("Expected an array"))
        ])
      )
    )
  }
};


let declDeserializer = MakeDeserializer.decl(deserializeTransformer);
