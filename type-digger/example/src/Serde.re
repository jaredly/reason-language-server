[@ocaml.warning "-34"];
module Version1 = {
  type _Types__household = {
    people: list(_Types__person),
    pets: list(_Types__pet),
  }
  and _Types__person = {
    name: string,
    age: int,
    coords: (float, float),
    parents: option((_Types__person, _Types__person)),
  }
  and _Types__pet =
    | Dog
    | Cat;
  let rec deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_pets => {
          let inner = attr_people =>
            Belt.Result.Ok({people: attr_people, pets: attr_pets});
          switch (Js.Dict.get(dict, "people")) {
          | None => Belt.Result.Error(["No attribute people"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____person;
                    let rec loop = (i, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok([])
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          switch (loop(i + 1, rest)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(error)
                          | Belt.Result.Ok(rest) =>
                            Belt.Result.Ok([value, ...rest])
                          }
                        }
                      };
                    loop(0, Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute people", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "pets")) {
        | None => Belt.Result.Error(["No attribute pets"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (Js.Json.classify(list)) {
                | JSONArray(items) =>
                  let transformer = deserialize_Types____pet;
                  let rec loop = (i, items) =>
                    switch (items) {
                    | [] => Belt.Result.Ok([])
                    | [one, ...rest] =>
                      switch (transformer(one)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error([
                          "list element " ++ string_of_int(i),
                          ...error,
                        ])
                      | Belt.Result.Ok(value) =>
                        switch (loop(i + 1, rest)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Belt.Result.Ok(rest) =>
                          Belt.Result.Ok([value, ...rest])
                        }
                      }
                    };
                  loop(0, Belt.List.fromArray(items));
                | _ => Belt.Result.Error(["expected an array"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute pets", ...error])
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
              | None => Belt.Result.Error(["No attribute name"])
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
                  Belt.Result.Error(["attribute name", ...error])
                | Ok(data) => inner(data)
                }
              };
            };
            switch (Js.Dict.get(dict, "age")) {
            | None => Belt.Result.Error(["No attribute age"])
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
                Belt.Result.Error(["attribute age", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "coords")) {
          | None => Belt.Result.Error(["No attribute coords"])
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
              Belt.Result.Error(["attribute coords", ...error])
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
            Belt.Result.Error(["attribute parents", ...error])
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
      };
};
module Version2 = {
  type _Types__household = {
    people: list(_Types__person),
    pets: list(_Types__pet),
  }
  and _Types__person = {
    name: string,
    age: float,
    coords: (float, float),
    parents: option((_Types__person, _Types__person)),
  }
  and _Types__pet = Types.pet = | Dog | Cat | Mouse;
  let rec deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_pets => {
          let inner = attr_people =>
            Belt.Result.Ok({people: attr_people, pets: attr_pets});
          switch (Js.Dict.get(dict, "people")) {
          | None => Belt.Result.Error(["No attribute people"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____person;
                    let rec loop = (i, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok([])
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          switch (loop(i + 1, rest)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(error)
                          | Belt.Result.Ok(rest) =>
                            Belt.Result.Ok([value, ...rest])
                          }
                        }
                      };
                    loop(0, Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute people", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "pets")) {
        | None => Belt.Result.Error(["No attribute pets"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (Js.Json.classify(list)) {
                | JSONArray(items) =>
                  let transformer = deserialize_Types____pet;
                  let rec loop = (i, items) =>
                    switch (items) {
                    | [] => Belt.Result.Ok([])
                    | [one, ...rest] =>
                      switch (transformer(one)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error([
                          "list element " ++ string_of_int(i),
                          ...error,
                        ])
                      | Belt.Result.Ok(value) =>
                        switch (loop(i + 1, rest)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Belt.Result.Ok(rest) =>
                          Belt.Result.Ok([value, ...rest])
                        }
                      }
                    };
                  loop(0, Belt.List.fromArray(items));
                | _ => Belt.Result.Error(["expected an array"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute pets", ...error])
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
              | None => Belt.Result.Error(["No attribute name"])
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
                  Belt.Result.Error(["attribute name", ...error])
                | Ok(data) => inner(data)
                }
              };
            };
            switch (Js.Dict.get(dict, "age")) {
            | None => Belt.Result.Error(["No attribute age"])
            | Some(json) =>
              switch (
                (
                  number =>
                    switch (Js.Json.classify(number)) {
                    | JSONNumber(number) => Belt.Result.Ok(number)
                    | _ => Error(["Expected a float"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute age", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "coords")) {
          | None => Belt.Result.Error(["No attribute coords"])
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
              Belt.Result.Error(["attribute coords", ...error])
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
            Belt.Result.Error(["attribute parents", ...error])
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
      | JSONArray([|tag|])
          when Js.Json.JSONString("Mouse") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Mouse: _Types__pet)
      | _ => Belt.Result.Error(["Expected an array"])
      };
  let rec migrate_Types____household:
    Version1._Types__household => _Types__household =
    _input_data => {
      let _converted_people =
        (Belt.List.map(_input_data.people))(_item =>
          migrate_Types____person(_item)
        );
      let _converted_pets =
        (Belt.List.map(_input_data.pets))(_item =>
          migrate_Types____pet(_item)
        );
      {pets: _converted_pets, people: _converted_people};
    }
  and migrate_Types____person: Version1._Types__person => _Types__person =
    _input_data => {
      let _converted_name = _input_data.name;
      let _converted_age =
        (
          person => float_of_int(person.age * 7):
            Version1._Types__person => float
        )(
          _input_data,
        );
      let _converted_coords = {
        let (arg0, arg1) = _input_data.coords;
        (arg0, arg1);
      };
      let _converted_parents =
        switch (_input_data.parents) {
        | None => None
        | Some(_item) =>
          Some(
            {
              let (arg0, arg1) = _item;
              (
                migrate_Types____person(arg0),
                migrate_Types____person(arg1),
              );
            },
          )
        };
      {
        parents: _converted_parents,
        coords: _converted_coords,
        age: _converted_age,
        name: _converted_name,
      };
    }
  and migrate_Types____pet: Version1._Types__pet => _Types__pet =
    _input_data =>
      switch (_input_data) {
      | Dog => Dog
      | Cat => Cat
      };
};
module Version3 = {
  type _Types__household = {
    people: list(_Types__person),
    pets: list(_Types__pet),
  }
  and _Types__person =
    Types.person = {
      name: string,
      age: float,
      coords: (float, float),
    }
  and _Types__pet = Types.pet = | Dog | Cat | Mouse;
  let rec deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_pets => {
          let inner = attr_people =>
            Belt.Result.Ok({people: attr_people, pets: attr_pets});
          switch (Js.Dict.get(dict, "people")) {
          | None => Belt.Result.Error(["No attribute people"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____person;
                    let rec loop = (i, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok([])
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          switch (loop(i + 1, rest)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(error)
                          | Belt.Result.Ok(rest) =>
                            Belt.Result.Ok([value, ...rest])
                          }
                        }
                      };
                    loop(0, Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute people", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "pets")) {
        | None => Belt.Result.Error(["No attribute pets"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (Js.Json.classify(list)) {
                | JSONArray(items) =>
                  let transformer = deserialize_Types____pet;
                  let rec loop = (i, items) =>
                    switch (items) {
                    | [] => Belt.Result.Ok([])
                    | [one, ...rest] =>
                      switch (transformer(one)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error([
                          "list element " ++ string_of_int(i),
                          ...error,
                        ])
                      | Belt.Result.Ok(value) =>
                        switch (loop(i + 1, rest)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Belt.Result.Ok(rest) =>
                          Belt.Result.Ok([value, ...rest])
                        }
                      }
                    };
                  loop(0, Belt.List.fromArray(items));
                | _ => Belt.Result.Error(["expected an array"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute pets", ...error])
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
        let inner = attr_coords => {
          let inner = attr_age => {
            let inner = attr_name =>
              Belt.Result.Ok({
                name: attr_name,
                age: attr_age,
                coords: attr_coords,
              });
            switch (Js.Dict.get(dict, "name")) {
            | None => Belt.Result.Error(["No attribute name"])
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
                Belt.Result.Error(["attribute name", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "age")) {
          | None => Belt.Result.Error(["No attribute age"])
          | Some(json) =>
            switch (
              (
                number =>
                  switch (Js.Json.classify(number)) {
                  | JSONNumber(number) => Belt.Result.Ok(number)
                  | _ => Error(["Expected a float"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute age", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "coords")) {
        | None => Belt.Result.Error(["No attribute coords"])
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
            Belt.Result.Error(["attribute coords", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____pet = Version2.deserialize_Types____pet;
  let rec migrate_Types____household:
    Version2._Types__household => _Types__household =
    _input_data => {
      let _converted_people =
        (Belt.List.map(_input_data.people))(_item =>
          migrate_Types____person(_item)
        );
      let _converted_pets =
        (Belt.List.map(_input_data.pets))(_item =>
          migrate_Types____pet(_item)
        );
      {pets: _converted_pets, people: _converted_people};
    }
  and migrate_Types____person: Version2._Types__person => _Types__person =
    _input_data => {
      let _converted_name = _input_data.name;
      let _converted_age = _input_data.age;
      let _converted_coords = {
        let (arg0, arg1) = _input_data.coords;
        (arg0, arg1);
      };
      {coords: _converted_coords, age: _converted_age, name: _converted_name};
    }
  and migrate_Types____pet: Version2._Types__pet => _Types__pet =
    _input_data => _input_data;
};
module Version4 = {
  type _Types__dogBreed =
    | Schnouser
    | Lab
    | Retriever
    | Poodle
  and _Types__household = {
    people: list(_Types__person),
    pets: list(_Types__pet),
  }
  and _Types__person =
    Types.person = {
      name: string,
      age: float,
      coords: (float, float),
    }
  and _Types__pet =
    | Dog(option(_Types__dogBreed))
    | Cat
    | Mouse;
  let rec deserialize_Types____dogBreed:
    Js.Json.t => Belt.Result.t(_Types__dogBreed, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag|])
          when Js.Json.JSONString("Schnouser") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Schnouser: _Types__dogBreed)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Lab") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Lab: _Types__dogBreed)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Retriever") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Retriever: _Types__dogBreed)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Poodle") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Poodle: _Types__dogBreed)
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_pets => {
          let inner = attr_people =>
            Belt.Result.Ok({people: attr_people, pets: attr_pets});
          switch (Js.Dict.get(dict, "people")) {
          | None => Belt.Result.Error(["No attribute people"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____person;
                    let rec loop = (i, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok([])
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          switch (loop(i + 1, rest)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(error)
                          | Belt.Result.Ok(rest) =>
                            Belt.Result.Ok([value, ...rest])
                          }
                        }
                      };
                    loop(0, Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute people", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "pets")) {
        | None => Belt.Result.Error(["No attribute pets"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (Js.Json.classify(list)) {
                | JSONArray(items) =>
                  let transformer = deserialize_Types____pet;
                  let rec loop = (i, items) =>
                    switch (items) {
                    | [] => Belt.Result.Ok([])
                    | [one, ...rest] =>
                      switch (transformer(one)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error([
                          "list element " ++ string_of_int(i),
                          ...error,
                        ])
                      | Belt.Result.Ok(value) =>
                        switch (loop(i + 1, rest)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Belt.Result.Ok(rest) =>
                          Belt.Result.Ok([value, ...rest])
                        }
                      }
                    };
                  loop(0, Belt.List.fromArray(items));
                | _ => Belt.Result.Error(["expected an array"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute pets", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____person = Version3.deserialize_Types____person
  and deserialize_Types____pet:
    Js.Json.t => Belt.Result.t(_Types__pet, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("Dog") == Js.Json.classify(tag) =>
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
              deserialize_Types____dogBreed,
            )
          )(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] Dog(arg0): _Types__pet)
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | JSONArray([|tag|])
          when Js.Json.JSONString("Cat") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Cat: _Types__pet)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Mouse") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Mouse: _Types__pet)
      | _ => Belt.Result.Error(["Expected an array"])
      };
  let rec migrate_Types____household:
    Version3._Types__household => _Types__household =
    _input_data => {
      let _converted_people =
        (Belt.List.map(_input_data.people))(_item =>
          migrate_Types____person(_item)
        );
      let _converted_pets =
        (Belt.List.map(_input_data.pets))(_item =>
          migrate_Types____pet(_item)
        );
      {pets: _converted_pets, people: _converted_people};
    }
  and migrate_Types____person: Version3._Types__person => _Types__person =
    _input_data => _input_data
  and migrate_Types____pet: Version3._Types__pet => _Types__pet =
    _input_data =>
      switch (_input_data) {
      | Dog => Dog(None)
      | Cat => Cat
      | Mouse => Mouse
      };
};
module Version5 = {
  type _Types__dogBreed =
    | Schnouser(string)
    | Lab
    | Retriever
    | Poodle
  and _Types__household = {
    people: list(_Types__person),
    pets: list(_Types__pet),
    county: _Types__named(int),
  }
  and _Types__named('a) = {
    name: string,
    contents: 'a,
  }
  and _Types__person =
    Types.person = {
      name: string,
      age: float,
      coords: (float, float),
    }
  and _Types__pet =
    | Dog(option(_Types__dogBreed))
    | Cat
    | Mouse;
  let rec deserialize_Types____dogBreed:
    Js.Json.t => Belt.Result.t(_Types__dogBreed, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("Schnouser") == Js.Json.classify(tag) =>
        switch (
          (
            string =>
              switch (Js.Json.classify(string)) {
              | JSONString(string) => Belt.Result.Ok(string)
              | _ => Error(["expected a string"])
              }
          )(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] Schnouser(arg0): _Types__dogBreed)
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | JSONArray([|tag|])
          when Js.Json.JSONString("Lab") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Lab: _Types__dogBreed)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Retriever") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Retriever: _Types__dogBreed)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Poodle") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Poodle: _Types__dogBreed)
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_county => {
          let inner = attr_pets => {
            let inner = attr_people =>
              Belt.Result.Ok({
                people: attr_people,
                pets: attr_pets,
                county: attr_county,
              });
            switch (Js.Dict.get(dict, "people")) {
            | None => Belt.Result.Error(["No attribute people"])
            | Some(json) =>
              switch (
                (
                  list =>
                    switch (Js.Json.classify(list)) {
                    | JSONArray(items) =>
                      let transformer = deserialize_Types____person;
                      let rec loop = (i, items) =>
                        switch (items) {
                        | [] => Belt.Result.Ok([])
                        | [one, ...rest] =>
                          switch (transformer(one)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error([
                              "list element " ++ string_of_int(i),
                              ...error,
                            ])
                          | Belt.Result.Ok(value) =>
                            switch (loop(i + 1, rest)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error(error)
                            | Belt.Result.Ok(rest) =>
                              Belt.Result.Ok([value, ...rest])
                            }
                          }
                        };
                      loop(0, Belt.List.fromArray(items));
                    | _ => Belt.Result.Error(["expected an array"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute people", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "pets")) {
          | None => Belt.Result.Error(["No attribute pets"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____pet;
                    let rec loop = (i, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok([])
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          switch (loop(i + 1, rest)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(error)
                          | Belt.Result.Ok(rest) =>
                            Belt.Result.Ok([value, ...rest])
                          }
                        }
                      };
                    loop(0, Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute pets", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "county")) {
        | None => Belt.Result.Error(["No attribute county"])
        | Some(json) =>
          switch (
            (
              deserialize_Types____named(number =>
                switch (Js.Json.classify(number)) {
                | JSONNumber(number) => Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute county", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____named:
    'arg0.
    (Js.Json.t => Belt.Result.t('arg0, list(string)), Js.Json.t) =>
    Belt.Result.t(_Types__named('arg0), list(string))
   =
    (type arg0, aTransformer, record) =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_contents => {
          let inner = attr_name =>
            Belt.Result.Ok({name: attr_name, contents: attr_contents});
          switch (Js.Dict.get(dict, "name")) {
          | None => Belt.Result.Error(["No attribute name"])
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
              Belt.Result.Error(["attribute name", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "contents")) {
        | None => Belt.Result.Error(["No attribute contents"])
        | Some(json) =>
          switch (aTransformer(json)) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute contents", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____person = Version4.deserialize_Types____person
  and deserialize_Types____pet:
    Js.Json.t => Belt.Result.t(_Types__pet, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("Dog") == Js.Json.classify(tag) =>
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
              deserialize_Types____dogBreed,
            )
          )(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] Dog(arg0): _Types__pet)
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | JSONArray([|tag|])
          when Js.Json.JSONString("Cat") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Cat: _Types__pet)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Mouse") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Mouse: _Types__pet)
      | _ => Belt.Result.Error(["Expected an array"])
      };
  let rec migrate_Types____dogBreed:
    Version4._Types__dogBreed => _Types__dogBreed =
    _input_data =>
      switch (_input_data) {
      | Schnouser => Schnouser("white")
      | Lab => Lab
      | Retriever => Retriever
      | Poodle => Poodle
      }
  and migrate_Types____household:
    Version4._Types__household => _Types__household =
    _input_data => {
      let _converted_people =
        (Belt.List.map(_input_data.people))(_item =>
          migrate_Types____person(_item)
        );
      let _converted_pets =
        (Belt.List.map(_input_data.pets))(_item =>
          migrate_Types____pet(_item)
        );
      let _converted_county =
        (
          household => {name: "Nowhere", contents: 0}:
            Version4._Types__household => _Types__named(int)
        )(
          _input_data,
        );
      {
        county: _converted_county,
        pets: _converted_pets,
        people: _converted_people,
      };
    }
  and migrate_Types____person: Version4._Types__person => _Types__person =
    _input_data => _input_data
  and migrate_Types____pet: Version4._Types__pet => _Types__pet =
    _input_data =>
      switch (_input_data) {
      | Dog(arg0) =>
        [@implicit_arity]
        Dog(
          switch (arg0) {
          | None => None
          | Some(_item) => Some(migrate_Types____dogBreed(_item))
          },
        )
      | Cat => Cat
      | Mouse => Mouse
      };
};
module Version6 = {
  type _Types__household =
    Types.household = {
      people: list(_Types__person),
      pets: list(_Types__pet),
      what: _Types__what(string),
      visitors: list(_Types__person),
      county: _Types__named(int),
    }
  and _Types__named('a) =
    Types.named('a) = {
      name: string,
      contents: 'a,
      isClosed: bool,
    }
  and _Types__person =
    Types.person = {
      name: string,
      age: float,
      coords: (float, float),
    }
  and _Types__pet = Types.pet = | Dog | Cat | Mouse
  and _Types__what('a) = Types.what('a) = | Now('a);
  let rec deserialize_Types____household:
    Js.Json.t => Belt.Result.t(_Types__household, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_county => {
          let inner = attr_visitors => {
            let inner = attr_what => {
              let inner = attr_pets => {
                let inner = attr_people =>
                  Belt.Result.Ok({
                    people: attr_people,
                    pets: attr_pets,
                    what: attr_what,
                    visitors: attr_visitors,
                    county: attr_county,
                  });
                switch (Js.Dict.get(dict, "people")) {
                | None => Belt.Result.Error(["No attribute people"])
                | Some(json) =>
                  switch (
                    (
                      list =>
                        switch (Js.Json.classify(list)) {
                        | JSONArray(items) =>
                          let transformer = deserialize_Types____person;
                          let rec loop = (i, items) =>
                            switch (items) {
                            | [] => Belt.Result.Ok([])
                            | [one, ...rest] =>
                              switch (transformer(one)) {
                              | Belt.Result.Error(error) =>
                                Belt.Result.Error([
                                  "list element " ++ string_of_int(i),
                                  ...error,
                                ])
                              | Belt.Result.Ok(value) =>
                                switch (loop(i + 1, rest)) {
                                | Belt.Result.Error(error) =>
                                  Belt.Result.Error(error)
                                | Belt.Result.Ok(rest) =>
                                  Belt.Result.Ok([value, ...rest])
                                }
                              }
                            };
                          loop(0, Belt.List.fromArray(items));
                        | _ => Belt.Result.Error(["expected an array"])
                        }
                    )(
                      json,
                    )
                  ) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute people", ...error])
                  | Ok(data) => inner(data)
                  }
                };
              };
              switch (Js.Dict.get(dict, "pets")) {
              | None => Belt.Result.Error(["No attribute pets"])
              | Some(json) =>
                switch (
                  (
                    list =>
                      switch (Js.Json.classify(list)) {
                      | JSONArray(items) =>
                        let transformer = deserialize_Types____pet;
                        let rec loop = (i, items) =>
                          switch (items) {
                          | [] => Belt.Result.Ok([])
                          | [one, ...rest] =>
                            switch (transformer(one)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error([
                                "list element " ++ string_of_int(i),
                                ...error,
                              ])
                            | Belt.Result.Ok(value) =>
                              switch (loop(i + 1, rest)) {
                              | Belt.Result.Error(error) =>
                                Belt.Result.Error(error)
                              | Belt.Result.Ok(rest) =>
                                Belt.Result.Ok([value, ...rest])
                              }
                            }
                          };
                        loop(0, Belt.List.fromArray(items));
                      | _ => Belt.Result.Error(["expected an array"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute pets", ...error])
                | Ok(data) => inner(data)
                }
              };
            };
            switch (Js.Dict.get(dict, "what")) {
            | None => Belt.Result.Error(["No attribute what"])
            | Some(json) =>
              switch (
                (
                  deserialize_Types____what(string =>
                    switch (Js.Json.classify(string)) {
                    | JSONString(string) => Belt.Result.Ok(string)
                    | _ => Error(["expected a string"])
                    }
                  )
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute what", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "visitors")) {
          | None => Belt.Result.Error(["No attribute visitors"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = deserialize_Types____person;
                    let rec loop = (i, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok([])
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error([
                            "list element " ++ string_of_int(i),
                            ...error,
                          ])
                        | Belt.Result.Ok(value) =>
                          switch (loop(i + 1, rest)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(error)
                          | Belt.Result.Ok(rest) =>
                            Belt.Result.Ok([value, ...rest])
                          }
                        }
                      };
                    loop(0, Belt.List.fromArray(items));
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute visitors", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "county")) {
        | None => Belt.Result.Error(["No attribute county"])
        | Some(json) =>
          switch (
            (
              deserialize_Types____named(number =>
                switch (Js.Json.classify(number)) {
                | JSONNumber(number) => Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute county", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____named:
    'arg0.
    (Js.Json.t => Belt.Result.t('arg0, list(string)), Js.Json.t) =>
    Belt.Result.t(_Types__named('arg0), list(string))
   =
    (type arg0, aTransformer, record) =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_isClosed => {
          let inner = attr_contents => {
            let inner = attr_name =>
              Belt.Result.Ok({
                name: attr_name,
                contents: attr_contents,
                isClosed: attr_isClosed,
              });
            switch (Js.Dict.get(dict, "the name")) {
            | None => Belt.Result.Error(["No attribute the name"])
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
                Belt.Result.Error(["attribute the name", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "contents")) {
          | None => Belt.Result.Error(["No attribute contents"])
          | Some(json) =>
            switch (aTransformer(json)) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute contents", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "isClosed")) {
        | None => Belt.Result.Error(["No attribute isClosed"])
        | Some(json) =>
          switch (
            (
              bool =>
                switch (Js.Json.classify(bool)) {
                | JSONTrue => Belt.Result.Ok(true)
                | JSONFalse => Belt.Result.Ok(false)
                | _ => Belt.Result.Error(["Expected a bool"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute isClosed", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Types____person = Version5.deserialize_Types____person
  and deserialize_Types____pet:
    Js.Json.t => Belt.Result.t(_Types__pet, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag|])
          when Js.Json.JSONString("a-cat") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Dog: _Types__pet)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Cat") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Cat: _Types__pet)
      | JSONArray([|tag|])
          when Js.Json.JSONString("Mouse") == Js.Json.classify(tag) =>
        Belt.Result.Ok(Mouse: _Types__pet)
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_Types____what:
    'arg0.
    (Js.Json.t => Belt.Result.t('arg0, list(string)), Js.Json.t) =>
    Belt.Result.t(_Types__what('arg0), list(string))
   =
    (type arg0, aTransformer, constructor) =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("Now") == Js.Json.classify(tag) =>
        switch (aTransformer(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] Now(arg0): _Types__what(arg0))
        | Error(error) => Error(["constructor argument 0", ...error])
        }
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
          ("what", (serialize_Types____what(Js.Json.string))(record.what)),
          (
            "visitors",
            (
              list =>
                Js.Json.array(
                  Belt.List.toArray(
                    Belt.List.map(list, serialize_Types____person),
                  ),
                )
            )(
              record.visitors,
            ),
          ),
          (
            "county",
            (
              serialize_Types____named(int =>
                Js.Json.number(float_of_int(int))
              )
            )(
              record.county,
            ),
          ),
        |]),
      )
  and serialize_Types____named:
    'arg0.
    ('arg0 => Js.Json.t, _Types__named('arg0)) => Js.Json.t
   =
    (aTransformer, record) =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          ("the name", Js.Json.string(record.name)),
          ("contents", aTransformer(record.contents)),
          ("isClosed", Js.Json.boolean(record.isClosed)),
        |]),
      )
  and serialize_Types____person: _Types__person => Js.Json.t =
    record =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          ("name", Js.Json.string(record.name)),
          ("age", Js.Json.number(record.age)),
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
        |]),
      )
  and serialize_Types____pet: _Types__pet => Js.Json.t =
    constructor =>
      switch (constructor) {
      | Dog => Js.Json.array([|Js.Json.string("a-cat")|])
      | Cat => Js.Json.array([|Js.Json.string("Cat")|])
      | Mouse => Js.Json.array([|Js.Json.string("Mouse")|])
      }
  and serialize_Types____what:
    'arg0.
    ('arg0 => Js.Json.t, _Types__what('arg0)) => Js.Json.t
   =
    (aTransformer, constructor) =>
      switch (constructor) {
      | Now(arg0) =>
        Js.Json.array([|Js.Json.string("Now"), aTransformer(arg0)|])
      };
  let rec migrate_Types____household:
    Version5._Types__household => _Types__household =
    _input_data => {
      let _converted_people =
        (Belt.List.map(_input_data.people))(_item =>
          migrate_Types____person(_item)
        );
      let _converted_pets =
        (Belt.List.map(_input_data.pets))(_item =>
          migrate_Types____pet(_item)
        );
      let _converted_what =
        (
          household => Now("4"):
            Version5._Types__household => _Types__what(string)
        )(
          _input_data,
        );
      let _converted_visitors =
        (household => []: Version5._Types__household => list(_Types__person))(
          _input_data,
        );
      let _converted_county =
        migrate_Types____named(arg => arg, _input_data.county);
      {
        county: _converted_county,
        visitors: _converted_visitors,
        what: _converted_what,
        pets: _converted_pets,
        people: _converted_people,
      };
    }
  and migrate_Types____named:
    ('a => 'a_migrated, Version5._Types__named('a)) =>
    _Types__named('a_migrated) =
    (contentsMigrator, named) => {
      name: named.name,
      contents: contentsMigrator(named.contents),
      isClosed: false,
    }
  and migrate_Types____person: Version5._Types__person => _Types__person =
    _input_data => _input_data
  and migrate_Types____pet: Version5._Types__pet => _Types__pet =
    _input_data =>
      switch (_input_data) {
      | Dog(dogBreed) => Dog
      | Cat => Cat
      | Mouse => Mouse
      };
};
let currentVersion = 6;
module Current = Version6;
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
    Version6.serialize_Types____household(data),
  )
and deserializeHousehold = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 6 =>
      switch (Version6.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | 5 =>
      switch (Version5.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version6.migrate_Types____household(data);
        Belt.Result.Ok(data);
      }
    | 4 =>
      switch (Version4.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version5.migrate_Types____household(data);
        let data = Version6.migrate_Types____household(data);
        Belt.Result.Ok(data);
      }
    | 3 =>
      switch (Version3.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version4.migrate_Types____household(data);
        let data = Version5.migrate_Types____household(data);
        let data = Version6.migrate_Types____household(data);
        Belt.Result.Ok(data);
      }
    | 2 =>
      switch (Version2.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version3.migrate_Types____household(data);
        let data = Version4.migrate_Types____household(data);
        let data = Version5.migrate_Types____household(data);
        let data = Version6.migrate_Types____household(data);
        Belt.Result.Ok(data);
      }
    | 1 =>
      switch (Version1.deserialize_Types____household(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version2.migrate_Types____household(data);
        let data = Version3.migrate_Types____household(data);
        let data = Version4.migrate_Types____household(data);
        let data = Version5.migrate_Types____household(data);
        let data = Version6.migrate_Types____household(data);
        Belt.Result.Ok(data);
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
