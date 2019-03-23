[@ocaml.warning "-34"];
module Types1 = {
  type _Types__household =
    Types.household = {
      people: list(_Types__person),
      pets: list(_Types__pet),
    }
  and _Types__person =
    Types.person = {
      name: string,
      age: int,
      coords: (float, float),
      parents: option((_Types__person, _Types__person)),
    }
  and _Types__pet = Types.pet = | Dog | Cat;
};
let currentVersion = 1;
module Version1 = {
  open Types1;
  let rec deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_pets => {
          let inner = attr_people =>
            Belt.Result.Ok({people: attr_people, pets: attr_pets});
          switch (Js.Dict.get(dict, "people")) {
          | None => Belt.Result.Error(["No attribute 'people'"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____person;
                    let rec loop = (i, collected, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok(Belt.List.reverse(collected))
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          loop(i + 1, [value, ...collected], rest)
                        }
                      };
                    loop(0, [], Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute 'people'", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "pets")) {
        | None => Belt.Result.Error(["No attribute 'pets'"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (Js.Json.classify(list)) {
                | JSONArray(items) =>
                  let transformer = deserialize_Types____pet;
                  let rec loop = (i, collected, items) =>
                    switch (items) {
                    | [] => Belt.Result.Ok(Belt.List.reverse(collected))
                    | [one, ...rest] =>
                      switch (transformer(one)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error([
                          "list element " ++ string_of_int(i),
                          ...error,
                        ])
                      | Belt.Result.Ok(value) =>
                        loop(i + 1, [value, ...collected], rest)
                      }
                    };
                  loop(0, [], Belt.List.fromArray(items));
                | _ => Belt.Result.Error(["expected an array"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute 'pets'", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____person:
    Js.Json.t => Belt.Result.t(_Types__person, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_parents => {
          let inner = attr_coords => {
            let inner = attr_age => {
              let inner = attr_name =>
                Belt.Result.Ok({
                  name: attr_name,
                  age: attr_age,
                  coords: attr_coords,
                  parents: attr_parents,
                });
              switch (Js.Dict.get(dict, "name")) {
              | None => Belt.Result.Error(["No attribute 'name'"])
              | Some(json) =>
                switch (
                  (
                    string =>
                      switch (Js.Json.classify(string)) {
                      | JSONString(string) => Belt.Result.Ok(string)
                      | _ => Error(["expected a string"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute 'name'", ...error])
                | Ok(data) => inner(data)
                }
              };
            };
            switch (Js.Dict.get(dict, "age")) {
            | None => Belt.Result.Error(["No attribute 'age'"])
            | Some(json) =>
              switch (
                (
                  number =>
                    switch (Js.Json.classify(number)) {
                    | JSONNumber(number) =>
                      Belt.Result.Ok(int_of_float(number))
                    | _ => Error(["Expected a float"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute 'age'", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "coords")) {
          | None => Belt.Result.Error(["No attribute 'coords'"])
          | Some(json) =>
            switch (
              (
                json =>
                  switch (Js.Json.classify(json)) {
                  | JSONArray([|arg0, arg1|]) =>
                    switch (
                      (
                        number =>
                          switch (Js.Json.classify(number)) {
                          | JSONNumber(number) => Belt.Result.Ok(number)
                          | _ => Error(["Expected a float"])
                          }
                      )(
                        arg1,
                      )
                    ) {
                    | Belt.Result.Ok(arg1) =>
                      switch (
                        (
                          number =>
                            switch (Js.Json.classify(number)) {
                            | JSONNumber(number) => Belt.Result.Ok(number)
                            | _ => Error(["Expected a float"])
                            }
                        )(
                          arg0,
                        )
                      ) {
                      | Belt.Result.Ok(arg0) =>
                        [@implicit_arity] Belt.Result.Ok(arg0, arg1)
                      | Error(error) => Error(["tuple element 0", ...error])
                      }
                    | Error(error) => Error(["tuple element 1", ...error])
                    }
                  | _ => Belt.Result.Error(["Expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute 'coords'", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "parents")) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (Js.Json.classify(option)) {
                  | JSONNull => Belt.Result.Ok(None)
                  | _ =>
                    switch (transformer(option)) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["optional value", ...error])
                    | Ok(value) => Ok(Some(value))
                    }
                  }
              )(
                json =>
                switch (Js.Json.classify(json)) {
                | JSONArray([|arg0, arg1|]) =>
                  switch (deserialize_Types____person(arg1)) {
                  | Belt.Result.Ok(arg1) =>
                    switch (deserialize_Types____person(arg0)) {
                    | Belt.Result.Ok(arg0) =>
                      [@implicit_arity] Belt.Result.Ok(arg0, arg1)
                    | Error(error) => Error(["tuple element 0", ...error])
                    }
                  | Error(error) => Error(["tuple element 1", ...error])
                  }
                | _ => Belt.Result.Error(["Expected an array"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute 'parents'", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____pet:
    Js.Json.t => Belt.Result.t(_Types__pet, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag|])
          when Js.Json.JSONString("Dog") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Dog: _Types__pet)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Cat") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Cat: _Types__pet)
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and serialize_Types____household: _Types__household => Js.Json.t =
    record =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          (
            "people",
            (
              list =>
                Js.Json.array(
                  Belt.List.toArray(
                    Belt.List.map(list, serialize_Types____person),
                  ),
                )
            )(
              record.people,
            ),
          ),
          (
            "pets",
            (
              list =>
                Js.Json.array(
                  Belt.List.toArray(
                    Belt.List.map(list, serialize_Types____pet),
                  ),
                )
            )(
              record.pets,
            ),
          ),
        |]),
      )
  and serialize_Types____person: _Types__person => Js.Json.t =
    record =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          ("name", Js.Json.string(record.name)),
          ("age", (int => Js.Json.number(float_of_int(int)))(record.age)),
          (
            "coords",
            (
              ((arg0, arg1)) =>
                Js.Json.array([|
                  Js.Json.number(arg0),
                  Js.Json.number(arg1),
                |])
            )(
              record.coords,
            ),
          ),
          (
            "parents",
            (
              (
                transformer =>
                  fun
                  | Some(inner) => transformer(inner)
                  | None => Js.Json.null
              )(
                ((arg0, arg1)) =>
                Js.Json.array([|
                  serialize_Types____person(arg0),
                  serialize_Types____person(arg1),
                |])
              )
            )(
              record.parents,
            ),
          ),
        |]),
      )
  and serialize_Types____pet: _Types__pet => Js.Json.t =
    constructor =>
      switch (constructor) {
      | Dog => Js.Json.array([|Js.Json.string("Dog")|])
      | Cat => Js.Json.array([|Js.Json.string("Cat")|])
      };
};
module Current = Version1;
let parseVersion = json =>
  switch (Js.Json.classify(json)) {
  | JSONObject(dict) =>
    switch (Js.Dict.get(dict, "$schemaVersion")) {
    | Some(schemaVersion) =>
      switch (Js.Json.classify(schemaVersion)) {
      | JSONNumber(version) =>
        [@implicit_arity] Belt.Result.Ok(int_of_float(version), json)
      | _ => Belt.Result.Error("Invalid $schemaVersion")
      }
    | None => Belt.Result.Error("No $schemaVersion present")
    }
  | JSONArray([|version, payload|]) =>
    switch (Js.Json.classify(version)) {
    | JSONNumber(version) =>
      [@implicit_arity] Belt.Result.Ok(int_of_float(version), payload)
    | _ => Belt.Result.Error("Invalid wrapped version")
    }
  | _ => Belt.Result.Error("Must have a schema version")
  };
let wrapWithVersion = (version, payload) =>
  switch (Js.Json.classify(payload)) {
  | JSONObject(dict) =>
    Js.Dict.set(
      dict,
      "$schemaVersion",
      Js.Json.number(float_of_int(version)),
    );
    Js.Json.object_(dict);
  | _ => Js.Json.array([|Js.Json.number(float_of_int(version)), payload|])
  };
let serializeHousehold = data =>
  wrapWithVersion(
    currentVersion,
    Version1.serialize_Types____household(data),
  )
and deserializeHousehold = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 1 =>
      switch (Version1.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
