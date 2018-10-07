
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

let makeJson = (kind, contents) => Exp.construct(Location.mknoloc(Ldot(Lident("Json"), kind)), contents);

let jsonObject = items => makeJson("Object", Some(Exp.array(items)));

let jsonArray = items => makeJson(
  "Array",
  Some(Exp.apply(
    makeIdent(Ldot(Ldot(Lident("Belt"), "List"), "toArray")),
    [(Nolabel, items)]
  ))
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
        makeJson("Array",
            Some(Exp.apply(
              makeIdent(Ldot(Ldot(Lident("Belt"), "Array"), "map")),
              [
                (Nolabel, makeIdent(Lident("array"))),
                (Nolabel, makeIdent(Lident("transformer"))),
              ]
            ))
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
  | Builtin("string") => [%expr s => Json.String(s)]
  | Builtin("bool") => [%expr b => b ? Json.True : Json.False]
  | Builtin("int") => [%expr i => Json.Number(float_of_int(i))]
  | Builtin("float") => [%expr f => Json.Number(f)]
  | Builtin("option") =>  [%expr (transformer) => fun
    | None => Js.Null
    | Some(v) => transformer(v)]
  | Builtin(name) => failer("Builtin: " ++ name)
};

let transformer = MakeSerializer.{
  source: sourceTransformer,
  list: jsonArray,
  tuple: exps => makeJson("Array", Some(Exp.array(exps))),
  record: items => jsonObject(items->Belt.List.map(((label, expr)) =>
        Exp.tuple([
          Exp.constant(Pconst_string(label, None)),
          expr
        ])
       )),
  constructor: (name, args) => 
            makeJson("Array", Some(Exp.array([
                makeJson("String", Some(Exp.constant(Pconst_string(name, None)))),
              ] @ args
            )))
};

let declSerializer = MakeSerializer.decl(transformer);











/* let jsJson = Ldot(Lident("Js"), "Json"); */

/* let makeJson = (kind, contents) => Exp.apply(makeIdent(Ldot(jsJson, kind)), [
  (Nolabel, contents)
]); */


/* let jsonObject = items => makeJson("object_", Exp.apply(
  makeIdent(Ldot(Ldot(Lident("Js"), "Dict"), "fromArray")),
  [(Nolabel, Exp.array(items))]
)); */

/* let jsonArray = items => makeJson(
  "array",
  Exp.apply(
    makeIdent(Ldot(Ldot(Lident("Belt"), "List"), "toArray")),
    [(Nolabel, items)]
  )
); */


let sourceTransformer = source => switch source {
  | DigTypes.NotFound => failer("Not found")
  | Public({DigTypes.modulePath, moduleName, name}) =>
    makeIdent(Lident(MakeDeserializer.transformerName(~moduleName, ~modulePath, ~name)))
  | Builtin("array") =>
    [%expr 
      (transformer, array) => switch (array) {
        | Json.Array(items) =>
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
    [%expr 
      (transformer, list) => switch (list) {
        | Json.Array(items) =>
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
        | Ok(value) => Ok(Some(value))
      }
    }]
  | Builtin(name) => failer("Builtin: " ++ name)
};


let jsonT = [%type: Json.t];

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
      [%expr switch (Belt.List.getAssoc(items, [%e MakeDeserializer.expString(label)]), (==)) {
        | None => Belt.Result.Error("No attribute " ++ [%e MakeDeserializer.expString(label)])
        | Some(json) => switch ([%e inner](json)) {
          | Belt.Result.Error(error) => Belt.Result.Error(error)
          | Ok([%p Pat.var(Location.mknoloc("attr_" ++ label))]) => [%e body]
        }
      }]
    });
    [%expr record => switch (record) {
      | Json.Object(items) => [%e body]
      | _ => Belt.Result.Error("Expected an object")
    }]
  },
  variant: (constructors) => {

    let cases = constructors->Belt.List.map(((name, argCount, argTransformer)) => {
          Exp.case(
            Pat.construct(Location.mknoloc(Ldot(Lident("Json"), "Array")), Some(
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
              (Nolabel, Exp.construct(Location.mknoloc(Ldot(Lident("Json"), "String")), Some(Exp.constant(Pconst_string(name, None))))),
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


let declDeserializer = MakeDeserializer.decl(transformer);
