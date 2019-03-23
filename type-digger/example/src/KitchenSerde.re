[@ocaml.warning "-34"];
open KitchenTypes;
module Version1 = {
  open Types1;
  let rec deserialize_AllTypes__All__normalRecord:
    Js.Json.t => Belt.Result.t(_AllTypes__All__normalRecord, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_f => {
          let inner = attr_e => {
            let inner = attr_d => {
              let inner = attr_c => {
                let inner = attr_b => {
                  let inner = attr_a =>
                    Belt.Result.Ok({
                      a: attr_a,
                      b: attr_b,
                      c: attr_c,
                      d: attr_d,
                      e: attr_e,
                      f: attr_f,
                    });
                  switch (Js.Dict.get(dict, "a")) {
                  | None => Belt.Result.Error(["No attribute 'a'"])
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
                      Belt.Result.Error(["attribute 'a'", ...error])
                    | Ok(data) => inner(data)
                    }
                  };
                };
                switch (Js.Dict.get(dict, "b")) {
                | None => Belt.Result.Error(["No attribute 'b'"])
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
                    Belt.Result.Error(["attribute 'b'", ...error])
                  | Ok(data) => inner(data)
                  }
                };
              };
              switch (Js.Dict.get(dict, "c")) {
              | None => Belt.Result.Error(["No attribute 'c'"])
              | Some(json) =>
                switch (
                  (
                    json =>
                      switch (Js.Json.classify(json)) {
                      | JSONArray([|arg0, arg1|]) =>
                        switch (
                          (
                            json =>
                              switch (Js.Json.classify(json)) {
                              | JSONArray([|arg0, arg1|]) =>
                                switch (
                                  (
                                    number =>
                                      switch (Js.Json.classify(number)) {
                                      | JSONNumber(number) =>
                                        Belt.Result.Ok(number)
                                      | _ => Error(["Expected a float"])
                                      }
                                  )(
                                    arg1,
                                  )
                                ) {
                                | Belt.Result.Ok(arg1) =>
                                  switch (
                                    (
                                      string =>
                                        switch (Js.Json.classify(string)) {
                                        | JSONString(string) =>
                                          Belt.Result.Ok(string)
                                        | _ => Error(["expected a string"])
                                        }
                                    )(
                                      arg0,
                                    )
                                  ) {
                                  | Belt.Result.Ok(arg0) =>
                                    Belt.Result.Ok((arg0, arg1))
                                  | Error(error) =>
                                    Belt.Result.Error([
                                      "tuple element 0",
                                      ...error,
                                    ])
                                  }
                                | Error(error) =>
                                  Belt.Result.Error([
                                    "tuple element 1",
                                    ...error,
                                  ])
                                }
                              | _ => Belt.Result.Error(["Expected an array"])
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
                                | JSONNumber(number) =>
                                  Belt.Result.Ok(int_of_float(number))
                                | _ => Error(["Expected a float"])
                                }
                            )(
                              arg0,
                            )
                          ) {
                          | Belt.Result.Ok(arg0) =>
                            Belt.Result.Ok((arg0, arg1))
                          | Error(error) =>
                            Belt.Result.Error(["tuple element 0", ...error])
                          }
                        | Error(error) =>
                          Belt.Result.Error(["tuple element 1", ...error])
                        }
                      | _ => Belt.Result.Error(["Expected an array"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute 'c'", ...error])
                | Ok(data) => inner(data)
                }
              };
            };
            switch (Js.Dict.get(dict, "d")) {
            | None => Belt.Result.Error(["No attribute 'd'"])
            | Some(json) =>
              switch (
                (
                  (
                    (transformer, array) =>
                      switch (Js.Json.classify(array)) {
                      | JSONArray(items) =>
                        let rec loop = (i, collected, items) =>
                          switch (items) {
                          | [] =>
                            Belt.Result.Ok(Belt.List.reverse(collected))
                          | [one, ...rest] =>
                            switch (transformer(one)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error([
                                "list element " ++ string_of_int(i),
                                ...error,
                              ])
                            | Ok(value) =>
                              loop(i + 1, [value, ...collected], rest)
                            }
                          };
                        switch (loop(0, [], Belt.List.fromArray(items))) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Ok(value) => Ok(Belt.List.toArray(value))
                        };
                      | _ => Belt.Result.Error(["expected an array"])
                      }
                  )(
                    number =>
                    switch (Js.Json.classify(number)) {
                    | JSONNumber(number) =>
                      Belt.Result.Ok(int_of_float(number))
                    | _ => Error(["Expected a float"])
                    }
                  )
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute 'd'", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "e")) {
          | None => Belt.Result.Error(["No attribute 'e'"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (Js.Json.classify(list)) {
                  | JSONArray(items) =>
                    let transformer = number =>
                      switch (Js.Json.classify(number)) {
                      | JSONNumber(number) => Belt.Result.Ok(number)
                      | _ => Error(["Expected a float"])
                      };
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
              Belt.Result.Error(["attribute 'e'", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "f")) {
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
                number =>
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
            Belt.Result.Error(["attribute 'f'", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_AllTypes__All__normalVariant:
    Js.Json.t => Belt.Result.t(_AllTypes__All__normalVariant, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag|])
          when Js.Json.JSONString("A") == Js.Json.classify(tag) =>
        Belt.Result.Ok(A: _AllTypes__All__normalVariant)
      | JSONArray([|tag|])
          when Js.Json.JSONString("B") == Js.Json.classify(tag) =>
        Belt.Result.Ok(B: _AllTypes__All__normalVariant)
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("C") == Js.Json.classify(tag) =>
        switch (
          (
            number =>
              switch (Js.Json.classify(number)) {
              | JSONNumber(number) => Belt.Result.Ok(int_of_float(number))
              | _ => Error(["Expected a float"])
              }
          )(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] C(arg0): _AllTypes__All__normalVariant,
          )
        | Error(error) =>
          Belt.Result.Error(["constructor argument 0", ...error])
        }
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("D") == Js.Json.classify(tag) =>
        switch (deserialize_AllTypes__All__normalRecord(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] D(arg0): _AllTypes__All__normalVariant,
          )
        | Error(error) =>
          Belt.Result.Error(["constructor argument 0", ...error])
        }
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_AllTypes__All__parameterizedRecord:
    'arg0 'arg1.
    (
      Js.Json.t => Belt.Result.t('arg0, list(string)),
      Js.Json.t => Belt.Result.t('arg1, list(string)),
      Js.Json.t
    ) =>
    Belt.Result.t(
      _AllTypes__All__parameterizedRecord('arg0, 'arg1),
      list(string),
    )
   =
    (type arg1, type arg0) => (
      (aTransformer, bTransformer, record) =>
        switch (Js.Json.classify(record)) {
        | JSONObject(dict) =>
          let inner = attr_d => {
            let inner = attr_c => {
              let inner = attr_b => {
                let inner = attr_a =>
                  Belt.Result.Ok({
                    a: attr_a,
                    b: attr_b,
                    c: attr_c,
                    d: attr_d,
                  });
                switch (Js.Dict.get(dict, "a")) {
                | None => Belt.Result.Error(["No attribute 'a'"])
                | Some(json) =>
                  switch (aTransformer(json)) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute 'a'", ...error])
                  | Ok(data) => inner(data)
                  }
                };
              };
              switch (Js.Dict.get(dict, "b")) {
              | None => Belt.Result.Error(["No attribute 'b'"])
              | Some(json) =>
                switch (bTransformer(json)) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute 'b'", ...error])
                | Ok(data) => inner(data)
                }
              };
            };
            switch (Js.Dict.get(dict, "c")) {
            | None => Belt.Result.Error(["No attribute 'c'"])
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
                              | JSONNumber(number) =>
                                Belt.Result.Ok(int_of_float(number))
                              | _ => Error(["Expected a float"])
                              }
                          )(
                            arg0,
                          )
                        ) {
                        | Belt.Result.Ok(arg0) =>
                          Belt.Result.Ok((arg0, arg1))
                        | Error(error) =>
                          Belt.Result.Error(["tuple element 0", ...error])
                        }
                      | Error(error) =>
                        Belt.Result.Error(["tuple element 1", ...error])
                      }
                    | _ => Belt.Result.Error(["Expected an array"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute 'c'", ...error])
              | Ok(data) => inner(data)
              }
            };
          };
          switch (Js.Dict.get(dict, "d")) {
          | None => Belt.Result.Error(["No attribute 'd'"])
          | Some(json) =>
            switch (deserialize_AllTypes__All__recursive(json)) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute 'd'", ...error])
            | Ok(data) => inner(data)
            }
          };
        | _ => Belt.Result.Error(["Expected an object"])
        }:
        (
          Js.Json.t => Belt.Result.t(arg0, list(string)),
          Js.Json.t => Belt.Result.t(arg1, list(string)),
          Js.Json.t
        ) =>
        Belt.Result.t(
          _AllTypes__All__parameterizedRecord(arg0, arg1),
          list(string),
        )
    )
  and deserialize_AllTypes__All__parameterizedVariant:
    'arg0 'arg1.
    (
      Js.Json.t => Belt.Result.t('arg0, list(string)),
      Js.Json.t => Belt.Result.t('arg1, list(string)),
      Js.Json.t
    ) =>
    Belt.Result.t(
      _AllTypes__All__parameterizedVariant('arg0, 'arg1),
      list(string),
    )
   =
    (type arg1, type arg0) => (
      (aTransformer, bTransformer, constructor) =>
        switch (Js.Json.classify(constructor)) {
        | JSONArray([|tag|])
            when Js.Json.JSONString("PA") == Js.Json.classify(tag) =>
          Belt.Result.Ok(
            PA: _AllTypes__All__parameterizedVariant(arg0, arg1),
          )
        | JSONArray([|tag, arg0|])
            when Js.Json.JSONString("PB") == Js.Json.classify(tag) =>
          switch (aTransformer(arg0)) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] PB(arg0):
                                           _AllTypes__All__parameterizedVariant(
                                             arg0,
                                             arg1,
                                           ),
            )
          | Error(error) =>
            Belt.Result.Error(["constructor argument 0", ...error])
          }
        | JSONArray([|tag, arg0, arg1|])
            when Js.Json.JSONString("PC") == Js.Json.classify(tag) =>
          switch (bTransformer(arg1)) {
          | Belt.Result.Ok(arg1) =>
            switch (aTransformer(arg0)) {
            | Belt.Result.Ok(arg0) =>
              Belt.Result.Ok(
                [@implicit_arity] PC(arg0, arg1):
                                                   _AllTypes__All__parameterizedVariant(
                                                     arg0,
                                                     arg1,
                                                   ),
              )
            | Error(error) =>
              Belt.Result.Error(["constructor argument 0", ...error])
            }
          | Error(error) =>
            Belt.Result.Error(["constructor argument 1", ...error])
          }
        | JSONArray([|tag, arg0|])
            when Js.Json.JSONString("PD") == Js.Json.classify(tag) =>
          switch (
            (
              deserialize_AllTypes__All__parameterizedRecord(
                aTransformer,
                bTransformer,
              )
            )(
              arg0,
            )
          ) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] PD(arg0):
                                           _AllTypes__All__parameterizedVariant(
                                             arg0,
                                             arg1,
                                           ),
            )
          | Error(error) =>
            Belt.Result.Error(["constructor argument 0", ...error])
          }
        | JSONArray([|tag, arg0|])
            when Js.Json.JSONString("PE") == Js.Json.classify(tag) =>
          switch (deserialize_AllTypes__All__normalVariant(arg0)) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] PE(arg0):
                                           _AllTypes__All__parameterizedVariant(
                                             arg0,
                                             arg1,
                                           ),
            )
          | Error(error) =>
            Belt.Result.Error(["constructor argument 0", ...error])
          }
        | JSONArray([|tag, arg0|])
            when Js.Json.JSONString("PF") == Js.Json.classify(tag) =>
          switch (deserialize_AllTypes__All__normalRecord(arg0)) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] PF(arg0):
                                           _AllTypes__All__parameterizedVariant(
                                             arg0,
                                             arg1,
                                           ),
            )
          | Error(error) =>
            Belt.Result.Error(["constructor argument 0", ...error])
          }
        | _ => Belt.Result.Error(["Expected an array"])
        }:
        (
          Js.Json.t => Belt.Result.t(arg0, list(string)),
          Js.Json.t => Belt.Result.t(arg1, list(string)),
          Js.Json.t
        ) =>
        Belt.Result.t(
          _AllTypes__All__parameterizedVariant(arg0, arg1),
          list(string),
        )
    )
  and deserialize_AllTypes__All__recursive:
    Js.Json.t => Belt.Result.t(_AllTypes__All__recursive, list(string)) =
    constructor =>
      switch (Js.Json.classify(constructor)) {
      | JSONArray([|tag|])
          when Js.Json.JSONString("A") == Js.Json.classify(tag) =>
        Belt.Result.Ok(A: _AllTypes__All__recursive)
      | JSONArray([|tag, arg0|])
          when Js.Json.JSONString("B") == Js.Json.classify(tag) =>
        switch (deserialize_AllTypes__All__recursive(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] B(arg0): _AllTypes__All__recursive,
          )
        | Error(error) =>
          Belt.Result.Error(["constructor argument 0", ...error])
        }
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_AllTypes__All__rename:
    Js.Json.t => Belt.Result.t(_AllTypes__All__rename, list(string)) =
    value => deserialize_AllTypes__All__top(value)
  and deserialize_AllTypes__All__top:
    Js.Json.t => Belt.Result.t(_AllTypes__All__top, list(string)) =
    record =>
      switch (Js.Json.classify(record)) {
      | JSONObject(dict) =>
        let inner = attr_title => {
          let inner = attr_contents =>
            Belt.Result.Ok({contents: attr_contents, title: attr_title});
          switch (Js.Dict.get(dict, "contents")) {
          | None => Belt.Result.Error(["No attribute 'contents'"])
          | Some(json) =>
            switch (
              (
                deserialize_AllTypes__All__parameterizedVariant(
                  number =>
                    switch (Js.Json.classify(number)) {
                    | JSONNumber(number) =>
                      Belt.Result.Ok(int_of_float(number))
                    | _ => Error(["Expected a float"])
                    },
                  (
                    (transformer, array) =>
                      switch (Js.Json.classify(array)) {
                      | JSONArray(items) =>
                        let rec loop = (i, collected, items) =>
                          switch (items) {
                          | [] =>
                            Belt.Result.Ok(Belt.List.reverse(collected))
                          | [one, ...rest] =>
                            switch (transformer(one)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error([
                                "list element " ++ string_of_int(i),
                                ...error,
                              ])
                            | Ok(value) =>
                              loop(i + 1, [value, ...collected], rest)
                            }
                          };
                        switch (loop(0, [], Belt.List.fromArray(items))) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Ok(value) => Ok(Belt.List.toArray(value))
                        };
                      | _ => Belt.Result.Error(["expected an array"])
                      }
                  )(
                    number =>
                    switch (Js.Json.classify(number)) {
                    | JSONNumber(number) => Belt.Result.Ok(number)
                    | _ => Error(["Expected a float"])
                    }
                  ),
                )
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute 'contents'", ...error])
            | Ok(data) => inner(data)
            }
          };
        };
        switch (Js.Dict.get(dict, "title")) {
        | None => Belt.Result.Error(["No attribute 'title'"])
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
            Belt.Result.Error(["attribute 'title'", ...error])
          | Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and serialize_AllTypes__All__normalRecord:
    _AllTypes__All__normalRecord => Js.Json.t =
    record =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          ("a", (int => Js.Json.number(float_of_int(int)))(record.a)),
          ("b", Js.Json.string(record.b)),
          (
            "c",
            (
              ((arg0, arg1)) =>
                Js.Json.array([|
                  (int => Js.Json.number(float_of_int(int)))(arg0),
                  (
                    ((arg0, arg1)) =>
                      Js.Json.array([|
                        Js.Json.string(arg0),
                        Js.Json.number(arg1),
                      |])
                  )(
                    arg1,
                  ),
                |])
            )(
              record.c,
            ),
          ),
          (
            "d",
            (
              (
                (transformer, array) =>
                  Js.Json.array((Belt.Array.map(array))(transformer))
              )(
                int =>
                Js.Json.number(float_of_int(int))
              )
            )(
              record.d,
            ),
          ),
          (
            "e",
            (
              list =>
                Js.Json.array(
                  Belt.List.toArray(Belt.List.map(list, Js.Json.number)),
                )
            )(
              record.e,
            ),
          ),
          (
            "f",
            (
              (
                transformer =>
                  fun
                  | Some(inner) => transformer(inner)
                  | None => Js.Json.null
              )(
                int =>
                Js.Json.number(float_of_int(int))
              )
            )(
              record.f,
            ),
          ),
        |]),
      )
  and serialize_AllTypes__All__normalVariant:
    _AllTypes__All__normalVariant => Js.Json.t =
    constructor =>
      switch (constructor) {
      | A => Js.Json.array([|Js.Json.string("A")|])
      | B => Js.Json.array([|Js.Json.string("B")|])
      | C(arg0) =>
        Js.Json.array([|
          Js.Json.string("C"),
          (int => Js.Json.number(float_of_int(int)))(arg0),
        |])
      | D(arg0) =>
        Js.Json.array([|
          Js.Json.string("D"),
          serialize_AllTypes__All__normalRecord(arg0),
        |])
      }
  and serialize_AllTypes__All__parameterizedRecord:
    'arg0 'arg1.
    (
      'arg0 => Js.Json.t,
      'arg1 => Js.Json.t,
      _AllTypes__All__parameterizedRecord('arg0, 'arg1)
    ) =>
    Js.Json.t
   =
    (aTransformer, bTransformer, record) =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          ("a", aTransformer(record.a)),
          ("b", bTransformer(record.b)),
          (
            "c",
            (
              ((arg0, arg1)) =>
                Js.Json.array([|
                  (int => Js.Json.number(float_of_int(int)))(arg0),
                  Js.Json.number(arg1),
                |])
            )(
              record.c,
            ),
          ),
          ("d", serialize_AllTypes__All__recursive(record.d)),
        |]),
      )
  and serialize_AllTypes__All__parameterizedVariant:
    'arg0 'arg1.
    (
      'arg0 => Js.Json.t,
      'arg1 => Js.Json.t,
      _AllTypes__All__parameterizedVariant('arg0, 'arg1)
    ) =>
    Js.Json.t
   =
    (aTransformer, bTransformer, constructor) =>
      switch (constructor) {
      | PA => Js.Json.array([|Js.Json.string("PA")|])
      | PB(arg0) =>
        Js.Json.array([|Js.Json.string("PB"), aTransformer(arg0)|])
      | [@implicit_arity] PC(arg0, arg1) =>
        Js.Json.array([|
          Js.Json.string("PC"),
          aTransformer(arg0),
          bTransformer(arg1),
        |])
      | PD(arg0) =>
        Js.Json.array([|
          Js.Json.string("PD"),
          (
            serialize_AllTypes__All__parameterizedRecord(
              aTransformer,
              bTransformer,
            )
          )(
            arg0,
          ),
        |])
      | PE(arg0) =>
        Js.Json.array([|
          Js.Json.string("PE"),
          serialize_AllTypes__All__normalVariant(arg0),
        |])
      | PF(arg0) =>
        Js.Json.array([|
          Js.Json.string("PF"),
          serialize_AllTypes__All__normalRecord(arg0),
        |])
      }
  and serialize_AllTypes__All__recursive:
    _AllTypes__All__recursive => Js.Json.t =
    constructor =>
      switch (constructor) {
      | A => Js.Json.array([|Js.Json.string("A")|])
      | B(arg0) =>
        Js.Json.array([|
          Js.Json.string("B"),
          serialize_AllTypes__All__recursive(arg0),
        |])
      }
  and serialize_AllTypes__All__rename: _AllTypes__All__rename => Js.Json.t =
    value => serialize_AllTypes__All__top(value)
  and serialize_AllTypes__All__top: _AllTypes__All__top => Js.Json.t =
    record =>
      Js.Json.object_(
        Js.Dict.fromArray([|
          (
            "contents",
            (
              serialize_AllTypes__All__parameterizedVariant(
                int => Js.Json.number(float_of_int(int)),
                (
                  (transformer, array) =>
                    Js.Json.array((Belt.Array.map(array))(transformer))
                )(
                  Js.Json.number,
                ),
              )
            )(
              record.contents,
            ),
          ),
          ("title", Js.Json.string(record.title)),
        |]),
      );
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
let serializeKitchenSink = data =>
  wrapWithVersion(
    currentVersion,
    Version1.serialize_AllTypes__All__rename(data),
  )
and deserializeKitchenSink = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 1 =>
      switch (Version1.deserialize_AllTypes__All__rename(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
