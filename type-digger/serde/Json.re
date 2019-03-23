
open Longident;
open Parsetree;
open Ast_helper;
open Asttypes;
open TypeMap;

open Helpers;

let makeJson = (kind, contents) => Exp.construct(Location.mknoloc(Ldot(Lident("Json"), kind)), contents);
let jsonObject = items => makeJson("Object", Some(makeList(items)));
let jsonArray = items => makeJson("Array", Some(items));

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

let serializeTransformer =
  MakeSerializer.{
    outputType: [%type: Json.t],
    wrapWithVersion: [%expr
      (version, payload) =>
        switch (payload) {
        | Json.Object(items) =>
          Json.Object([("$schemaVersion", Json.Number(float_of_int(version))), ...items])
        | _ => Json.Array([Json.Number(float_of_int(version)), payload])
        }
    ],
    source: sourceTransformer,
    list: jsonArray,
    tuple: exps => makeJson("Array", Some(makeList(exps))),
    record: (~renames, items) =>
      jsonObject(
        items->Belt.List.map(((label, expr)) =>
          Exp.tuple([
            Exp.constant(Pconst_string(MakeDeserializer.getRename(~renames, label), None)),
            expr,
          ])
        ),
      ),
    constructor: (~renames, name, args) =>
      makeJson(
        "Array",
        Some(
          makeList(
            [
              makeJson(
                "String",
                Some(
                  Exp.constant(Pconst_string(MakeDeserializer.getRename(~renames, name), None)),
                ),
              ),
            ]
            @ args,
          ),
        ),
      ),
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
          let rec loop = (collected, items) => switch items {
            | [] => Belt.Result.Ok(Belt.List.reverse(collected))
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(["array element", ...error])
              | Belt.Result.Ok(value) => loop([value, ...collected], rest)
            }
          };
          switch (loop([], items)) {
            | Belt.Result.Error(error) => Belt.Result.Error(error)
            | Belt.Result.Ok(value) => Belt.Result.Ok(Belt.List.toArray(value))
          }
        | _ => Belt.Result.Error(["expected an array"])
      }
    ];
  | Builtin("list") =>
    [%expr
      (transformer, list) => switch (list) {
        | Json.Array(items) =>
          let rec loop = (collected, items) => switch items {
            | [] => Belt.Result.Ok(Belt.List.reverse(collected))
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(["list element", ...error])
              | Belt.Result.Ok(value) => loop([value, ...collected], rest)
            }
          };
          loop([], items)
        | _ => Belt.Result.Error(["expected an array"])
      }
    ];
  | Builtin("string") =>
    [%expr string => switch (string) {
      | Json.String(string) => Belt.Result.Ok(string)
      | _ => Error(["epected a string"])
    }]
  | Builtin("bool") => [%expr bool => switch (bool) {
    | Json.True => Belt.Result.Ok(true)
    | Json.False => Belt.Result.Ok(false)
    | _ => Belt.Result.Error(["Expected a bool"])
  }]
  | Builtin("int") =>
    [%expr number => switch (number) {
      | Json.Number(number) => Belt.Result.Ok(int_of_float(number))
      | _ => Error(["Expected a float"])
    }]
  | Builtin("float") => [%expr number => switch (number) {
    | Json.Number(number) => Belt.Result.Ok(number)
    | _ => Error(["Expected a float"])
  }]
  | Builtin("option") =>
    [%expr (transformer, option) => switch (option) {
      | Json.Null => Belt.Result.Ok(None)
      | _ => switch (transformer(option)) {
        | Belt.Result.Error(error) => Belt.Result.Error(["optional value", ...error])
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
      | Json.Object(items) => switch (items->Belt.List.getAssoc("$schemaVersion", (==))) {
        | Some(Json.Number(schemaVersion)) => [@implicit_arity]Belt.Result.Ok((int_of_float(schemaVersion), json))
        | Some(_) => Belt.Result.Error("Invalid schema version - expected number")
        | None => Belt.Result.Error("No $schemaVersion")
      }
      | Json.Array([Json.Number(version), payload]) => [@implicit_arity]Belt.Result.Ok((int_of_float(version), payload))
      | _ => Belt.Result.Error("Not wrapped in a version")
    }
  ],
  tuple: (value, patArgs, body) => [%expr json => switch ([%e value]) {
    | Json.Array([%p makePatList(patArgs)]) => [%e body]
    | _ => Belt.Result.Error(["Expected array"])
  }],
  list: (transformer, list) => {
    [%expr
      switch ([%e list]) {
        | Json.Array(items) =>
          let transformer = [%e transformer];
          let rec loop = (collected, items) => switch items {
            | [] => Belt.Result.Ok(Belt.List.reverse(collected))
            | [one, ...rest] => switch (transformer(one)) {
              | Belt.Result.Error(error) => Belt.Result.Error(["list item", ...error])
              | Belt.Result.Ok(value) => loop([value, ...collected], rest)
            }
          };
          loop([], items)
        | _ => Belt.Result.Error(["expected an array"])
      }
    ];

  },
  record: (~renames, items) =>  {
    let body =
      MakeDeserializer.ok(
        Exp.record(
          items->Belt.List.map(((label, _, _)) => (Location.mknoloc(Lident(label)), makeIdent(Lident("attr_" ++ label)))),
          None,
        ),
      );
    let body = items->Belt.List.reduce(body, (body, (label, inner, isOptional)) => {
      let attrName = MakeDeserializer.getRename(~renames, label);
      let pat = Pat.var(Location.mknoloc("attr_" ++ label));
      {
        let%expr inner = ([%p pat]) => [%e body];
        switch (Belt.List.getAssoc(items, [%e MakeDeserializer.expString(attrName)], (==))) {
        | None =>
          if%e (isOptional) {
            %expr
            inner(None);
          } else {
            %expr
            Belt.Result.Error([[%e MakeDeserializer.expString("No attribute '" ++ attrName ++ "'")]]);
          }
        | Some(json) =>
          switch ([%e inner](json)) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error([[%e MakeDeserializer.expString("attribute '" ++ attrName ++ "'")], ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      };
    });
    [%expr record => switch (record) {
      | Json.Object(items) => [%e body]
      | _ => Belt.Result.Error(["Expected an object"])
    }]
  },
  variant: (~renames, constructors) => {

    let cases =
      constructors->Belt.List.map(((name, argCount, argTransformer)) => {
        let constrName = MakeDeserializer.getRename(~renames, name);
        let pat =
          Pat.construct(
            Location.mknoloc(Ldot(Lident("Json"), "Array")),
            Some(
              makePatList([
                [%pat? Json.String([%p Pat.var(Location.mknoloc("tag"))])],
                ...MakeDeserializer.range(argCount, index =>
                     Pat.var(Location.mknoloc("arg" ++ string_of_int(index)))
                   ),
              ]),
            ),
          );
        Exp.case(
          if (argCount == 0) {
            Pat.or_(
              pat,
              [%pat? Json.String(tag)]
            )
          } else { pat },
          ~guard=
            Exp.apply(
              makeIdent(Lident("=")),
              [
                (
                  Nolabel,
                  Exp.constant(Pconst_string(constrName, None)),
                ),
                (Nolabel, makeIdent(Lident("tag"))),
              ],
            ),
          argTransformer,
        );
      });

    Exp.fun_(
      Nolabel,
      None,
      Pat.var(Location.mknoloc("constructor")),
      Exp.match(
        makeIdent(Lident("constructor")),
        cases->Belt.List.concat([
          Exp.case([%pat? Json.Array([Json.String(tag), ..._])], [%expr Belt.Result.Error(["Invalid constructor: " ++ tag])]),
          Exp.case(Pat.any(), MakeDeserializer.expError("Expected an array"))
        ])
      )
    )
  }
};


let declDeserializer = MakeDeserializer.decl(deserializeTransformer);
