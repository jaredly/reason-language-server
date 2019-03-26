[@ocaml.warning "-34"];
open KitchenTypes;
type target = Ezjsonm.value;
module Version1 = {
  open Types1;
  let rec deserialize_AllTypes__All__normalRecord:
    Ezjsonm.value =>
    Belt.Result.t(_AllTypes__All__normalRecord, list(string)) =
    record =>
      switch (record) {
      | `O(items) =>
        let inner = attr_f => {
          let inner = attr_e => {
            let inner = attr_d => {
              let inner = attr_c => {
                let inner = attr_b => {
                  let inner = attr_a =>
                    Belt.Result.Ok(
                      {
                        a: attr_a,
                        b: attr_b,
                        c: attr_c,
                        d: attr_d,
                        e: attr_e,
                        f: attr_f,
                      }: _AllTypes__All__normalRecord,
                    );
                  switch (Belt.List.getAssoc(items, "a", (==))) {
                  | None => Belt.Result.Error(["No attribute 'a'"])
                  | Some(json) =>
                    switch (
                      (
                        number =>
                          switch (number) {
                          | `Float(number) =>
                            Belt.Result.Ok(int_of_float(number))
                          | _ => Error(["Expected a float"])
                          }
                      )(
                        json,
                      )
                    ) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["attribute 'a'", ...error])
                    | Belt.Result.Ok(data) => inner(data)
                    }
                  };
                };
                switch (Belt.List.getAssoc(items, "b", (==))) {
                | None => Belt.Result.Error(["No attribute 'b'"])
                | Some(json) =>
                  switch (
                    (
                      string =>
                        switch (string) {
                        | `String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
                        }
                    )(
                      json,
                    )
                  ) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute 'b'", ...error])
                  | Belt.Result.Ok(data) => inner(data)
                  }
                };
              };
              switch (Belt.List.getAssoc(items, "c", (==))) {
              | None => Belt.Result.Error(["No attribute 'c'"])
              | Some(json) =>
                switch (
                  (
                    json =>
                      switch (json) {
                      | `A([arg0, arg1]) =>
                        switch (
                          (
                            json =>
                              switch (json) {
                              | `A([arg0, arg1]) =>
                                switch (
                                  (
                                    number =>
                                      switch (number) {
                                      | `Float(number) =>
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
                                        switch (string) {
                                        | `String(string) =>
                                          Belt.Result.Ok(string)
                                        | _ => Error(["epected a string"])
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
                              | _ => Belt.Result.Error(["Expected array"])
                              }
                          )(
                            arg1,
                          )
                        ) {
                        | Belt.Result.Ok(arg1) =>
                          switch (
                            (
                              number =>
                                switch (number) {
                                | `Float(number) =>
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
                      | _ => Belt.Result.Error(["Expected array"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute 'c'", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "d", (==))) {
            | None => Belt.Result.Error(["No attribute 'd'"])
            | Some(json) =>
              switch (
                (
                  (
                    (transformer, array) =>
                      switch (array) {
                      | `A(items) =>
                        let rec loop = (collected, items) =>
                          switch (items) {
                          | [] =>
                            Belt.Result.Ok(Belt.List.reverse(collected))
                          | [one, ...rest] =>
                            switch (transformer(one)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error(["array element", ...error])
                            | Belt.Result.Ok(value) =>
                              loop([value, ...collected], rest)
                            }
                          };
                        switch (loop([], items)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Belt.Result.Ok(value) =>
                          Belt.Result.Ok(Belt.List.toArray(value))
                        };
                      | _ => Belt.Result.Error(["expected an array"])
                      }
                  )(
                    number =>
                    switch (number) {
                    | `Float(number) => Belt.Result.Ok(int_of_float(number))
                    | _ => Error(["Expected a float"])
                    }
                  )
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute 'd'", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "e", (==))) {
          | None => Belt.Result.Error(["No attribute 'e'"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | `A(items) =>
                    let transformer = number =>
                      switch (number) {
                      | `Float(number) => Belt.Result.Ok(number)
                      | _ => Error(["Expected a float"])
                      };
                    let rec loop = (collected, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok(Belt.List.reverse(collected))
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(["list item", ...error])
                        | Belt.Result.Ok(value) =>
                          loop([value, ...collected], rest)
                        }
                      };
                    loop([], items);
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute 'e'", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "f", (==))) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | `Null => Belt.Result.Ok(None)
                  | _ =>
                    switch (transformer(option)) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["optional value", ...error])
                    | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                    }
                  }
              )(
                number =>
                switch (number) {
                | `Float(number) => Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute 'f'", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_AllTypes__All__normalVariant:
    Ezjsonm.value =>
    Belt.Result.t(_AllTypes__All__normalVariant, list(string)) =
    constructor =>
      switch (constructor) {
      | `A([`String(tag)])
      | `String(tag) when "A" == tag =>
        Belt.Result.Ok(A: _AllTypes__All__normalVariant)
      | `A([`String(tag)])
      | `String(tag) when "B" == tag =>
        Belt.Result.Ok(B: _AllTypes__All__normalVariant)
      | `A([`String(tag), arg0]) when "C" == tag =>
        switch (
          (
            number =>
              switch (number) {
              | `Float(number) => Belt.Result.Ok(int_of_float(number))
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
      | `A([`String(tag), arg0]) when "D" == tag =>
        switch (deserialize_AllTypes__All__normalRecord(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] D(arg0): _AllTypes__All__normalVariant,
          )
        | Error(error) =>
          Belt.Result.Error(["constructor argument 0", ...error])
        }
      | `A([`String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_AllTypes__All__parameterizedRecord:
    'arg0 'arg1.
    (
      Ezjsonm.value => Belt.Result.t('arg0, list(string)),
      Ezjsonm.value => Belt.Result.t('arg1, list(string)),
      Ezjsonm.value
    ) =>
    Belt.Result.t(
      _AllTypes__All__parameterizedRecord('arg0, 'arg1),
      list(string),
    )
   =
    (type arg1, type arg0) => (
      (aTransformer, bTransformer, record) =>
        switch (record) {
        | `O(items) =>
          let inner = attr_d => {
            let inner = attr_c => {
              let inner = attr_b => {
                let inner = attr_a =>
                  Belt.Result.Ok(
                    {a: attr_a, b: attr_b, c: attr_c, d: attr_d}:
                                                                   _AllTypes__All__parameterizedRecord(
                                                                    arg0,
                                                                    arg1,
                                                                   ),
                  );
                switch (Belt.List.getAssoc(items, "a", (==))) {
                | None => Belt.Result.Error(["No attribute 'a'"])
                | Some(json) =>
                  switch (aTransformer(json)) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute 'a'", ...error])
                  | Belt.Result.Ok(data) => inner(data)
                  }
                };
              };
              switch (Belt.List.getAssoc(items, "b", (==))) {
              | None => Belt.Result.Error(["No attribute 'b'"])
              | Some(json) =>
                switch (bTransformer(json)) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute 'b'", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "c", (==))) {
            | None => Belt.Result.Error(["No attribute 'c'"])
            | Some(json) =>
              switch (
                (
                  json =>
                    switch (json) {
                    | `A([arg0, arg1]) =>
                      switch (
                        (
                          number =>
                            switch (number) {
                            | `Float(number) => Belt.Result.Ok(number)
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
                              switch (number) {
                              | `Float(number) =>
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
                    | _ => Belt.Result.Error(["Expected array"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute 'c'", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "d", (==))) {
          | None => Belt.Result.Error(["No attribute 'd'"])
          | Some(json) =>
            switch (deserialize_AllTypes__All__recursive(json)) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute 'd'", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        | _ => Belt.Result.Error(["Expected an object"])
        }:
        (
          Ezjsonm.value => Belt.Result.t(arg0, list(string)),
          Ezjsonm.value => Belt.Result.t(arg1, list(string)),
          Ezjsonm.value
        ) =>
        Belt.Result.t(
          _AllTypes__All__parameterizedRecord(arg0, arg1),
          list(string),
        )
    )
  and deserialize_AllTypes__All__parameterizedVariant:
    'arg0 'arg1.
    (
      Ezjsonm.value => Belt.Result.t('arg0, list(string)),
      Ezjsonm.value => Belt.Result.t('arg1, list(string)),
      Ezjsonm.value
    ) =>
    Belt.Result.t(
      _AllTypes__All__parameterizedVariant('arg0, 'arg1),
      list(string),
    )
   =
    (type arg1, type arg0) => (
      (aTransformer, bTransformer, constructor) =>
        switch (constructor) {
        | `A([`String(tag)])
        | `String(tag) when "PA" == tag =>
          Belt.Result.Ok(
            PA: _AllTypes__All__parameterizedVariant(arg0, arg1),
          )
        | `A([`String(tag), arg0]) when "PB" == tag =>
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
        | `A([`String(tag), arg0, arg1]) when "PC" == tag =>
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
        | `A([`String(tag), arg0]) when "PD" == tag =>
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
        | `A([`String(tag), arg0]) when "PE" == tag =>
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
        | `A([`String(tag), arg0]) when "PF" == tag =>
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
        | `A([`String(tag), ..._]) =>
          Belt.Result.Error(["Invalid constructor: " ++ tag])
        | _ => Belt.Result.Error(["Expected an array"])
        }:
        (
          Ezjsonm.value => Belt.Result.t(arg0, list(string)),
          Ezjsonm.value => Belt.Result.t(arg1, list(string)),
          Ezjsonm.value
        ) =>
        Belt.Result.t(
          _AllTypes__All__parameterizedVariant(arg0, arg1),
          list(string),
        )
    )
  and deserialize_AllTypes__All__recursive:
    Ezjsonm.value => Belt.Result.t(_AllTypes__All__recursive, list(string)) =
    constructor =>
      switch (constructor) {
      | `A([`String(tag)])
      | `String(tag) when "A" == tag =>
        Belt.Result.Ok(A: _AllTypes__All__recursive)
      | `A([`String(tag), arg0]) when "B" == tag =>
        switch (deserialize_AllTypes__All__recursive(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] B(arg0): _AllTypes__All__recursive,
          )
        | Error(error) =>
          Belt.Result.Error(["constructor argument 0", ...error])
        }
      | `A([`String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_AllTypes__All__rename:
    Ezjsonm.value => Belt.Result.t(_AllTypes__All__rename, list(string)) =
    value => deserialize_AllTypes__All__top(value)
  and deserialize_AllTypes__All__top:
    Ezjsonm.value => Belt.Result.t(_AllTypes__All__top, list(string)) =
    record =>
      switch (record) {
      | `O(items) =>
        let inner = attr_title => {
          let inner = attr_contents =>
            Belt.Result.Ok(
              {contents: attr_contents, title: attr_title}: _AllTypes__All__top,
            );
          switch (Belt.List.getAssoc(items, "contents", (==))) {
          | None => Belt.Result.Error(["No attribute 'contents'"])
          | Some(json) =>
            switch (
              (
                deserialize_AllTypes__All__parameterizedVariant(
                  number =>
                    switch (number) {
                    | `Float(number) => Belt.Result.Ok(int_of_float(number))
                    | _ => Error(["Expected a float"])
                    },
                  (
                    (transformer, array) =>
                      switch (array) {
                      | `A(items) =>
                        let rec loop = (collected, items) =>
                          switch (items) {
                          | [] =>
                            Belt.Result.Ok(Belt.List.reverse(collected))
                          | [one, ...rest] =>
                            switch (transformer(one)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error(["array element", ...error])
                            | Belt.Result.Ok(value) =>
                              loop([value, ...collected], rest)
                            }
                          };
                        switch (loop([], items)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(error)
                        | Belt.Result.Ok(value) =>
                          Belt.Result.Ok(Belt.List.toArray(value))
                        };
                      | _ => Belt.Result.Error(["expected an array"])
                      }
                  )(
                    number =>
                    switch (number) {
                    | `Float(number) => Belt.Result.Ok(number)
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
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "title", (==))) {
        | None => Belt.Result.Error(["No attribute 'title'"])
        | Some(json) =>
          switch (
            (
              string =>
                switch (string) {
                | `String(string) => Belt.Result.Ok(string)
                | _ => Error(["epected a string"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute 'title'", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and serialize_AllTypes__All__normalRecord:
    _AllTypes__All__normalRecord => Ezjsonm.value =
    record =>
      `O([
        ("a", (i => `Float(float_of_int(i)))(record.a)),
        ("b", (s => `String(s))(record.b)),
        (
          "c",
          (
            ((arg0, arg1)) =>
              `A([
                (i => `Float(float_of_int(i)))(arg0),
                (
                  ((arg0, arg1)) =>
                    `A([(s => `String(s))(arg0), (f => `Float(f))(arg1)])
                )(
                  arg1,
                ),
              ])
          )(
            record.c,
          ),
        ),
        (
          "d",
          (
            (
              (transformer, array) =>
                `A(Belt.List.fromArray(Belt.Array.map(array, transformer)))
            )(
              i =>
              `Float(float_of_int(i))
            )
          )(
            record.d,
          ),
        ),
        ("e", (list => `A(Belt.List.map(list, f => `Float(f))))(record.e)),
        (
          "f",
          (
            (
              transformer =>
                fun
                | None => `Null
                | Some(v) => transformer(v)
            )(
              i =>
              `Float(float_of_int(i))
            )
          )(
            record.f,
          ),
        ),
      ])
  and serialize_AllTypes__All__normalVariant:
    _AllTypes__All__normalVariant => Ezjsonm.value =
    constructor =>
      switch (constructor) {
      | A => `A([`String("A")])
      | B => `A([`String("B")])
      | C(arg0) =>
        `A([`String("C"), (i => `Float(float_of_int(i)))(arg0)])
      | D(arg0) =>
        `A([`String("D"), serialize_AllTypes__All__normalRecord(arg0)])
      }
  and serialize_AllTypes__All__parameterizedRecord:
    'arg0 'arg1.
    (
      'arg0 => Ezjsonm.value,
      'arg1 => Ezjsonm.value,
      _AllTypes__All__parameterizedRecord('arg0, 'arg1)
    ) =>
    Ezjsonm.value
   =
    (aTransformer, bTransformer, record) =>
      `O([
        ("a", aTransformer(record.a)),
        ("b", bTransformer(record.b)),
        (
          "c",
          (
            ((arg0, arg1)) =>
              `A([
                (i => `Float(float_of_int(i)))(arg0),
                (f => `Float(f))(arg1),
              ])
          )(
            record.c,
          ),
        ),
        ("d", serialize_AllTypes__All__recursive(record.d)),
      ])
  and serialize_AllTypes__All__parameterizedVariant:
    'arg0 'arg1.
    (
      'arg0 => Ezjsonm.value,
      'arg1 => Ezjsonm.value,
      _AllTypes__All__parameterizedVariant('arg0, 'arg1)
    ) =>
    Ezjsonm.value
   =
    (aTransformer, bTransformer, constructor) =>
      switch (constructor) {
      | PA => `A([`String("PA")])
      | PB(arg0) => `A([`String("PB"), aTransformer(arg0)])
      | [@implicit_arity] PC(arg0, arg1) =>
        `A([`String("PC"), aTransformer(arg0), bTransformer(arg1)])
      | PD(arg0) =>
        `A([
          `String("PD"),
          (
            serialize_AllTypes__All__parameterizedRecord(
              aTransformer,
              bTransformer,
            )
          )(
            arg0,
          ),
        ])
      | PE(arg0) =>
        `A([`String("PE"), serialize_AllTypes__All__normalVariant(arg0)])
      | PF(arg0) =>
        `A([`String("PF"), serialize_AllTypes__All__normalRecord(arg0)])
      }
  and serialize_AllTypes__All__recursive:
    _AllTypes__All__recursive => Ezjsonm.value =
    constructor =>
      switch (constructor) {
      | A => `A([`String("A")])
      | B(arg0) =>
        `A([`String("B"), serialize_AllTypes__All__recursive(arg0)])
      }
  and serialize_AllTypes__All__rename: _AllTypes__All__rename => Ezjsonm.value =
    value => serialize_AllTypes__All__top(value)
  and serialize_AllTypes__All__top: _AllTypes__All__top => Ezjsonm.value =
    record =>
      `O([
        (
          "contents",
          (
            serialize_AllTypes__All__parameterizedVariant(
              i => `Float(float_of_int(i)),
              (
                (transformer, array) =>
                  `A(
                    Belt.List.fromArray(Belt.Array.map(array, transformer)),
                  )
              )(
                f =>
                `Float(f)
              ),
            )
          )(
            record.contents,
          ),
        ),
        ("title", (s => `String(s))(record.title)),
      ]);
};
module Current = Version1;
let parseVersion = json =>
  switch (json) {
  | `O(items) =>
    switch ((Belt.List.getAssoc(items))("$schemaVersion", (==))) {
    | Some(`Float(schemaVersion)) =>
      [@implicit_arity] Belt.Result.Ok(int_of_float(schemaVersion), json)
    | Some(_) => Belt.Result.Error("Invalid schema version - expected number")
    | None => Belt.Result.Error("No $schemaVersion")
    }
  | `A([`Float(version), payload]) =>
    [@implicit_arity] Belt.Result.Ok(int_of_float(version), payload)
  | _ => Belt.Result.Error("Not wrapped in a version")
  };
let wrapWithVersion = (version, payload) =>
  switch (payload) {
  | `O(items) =>
    `O([("$schemaVersion", `Float(float_of_int(version))), ...items])
  | _ => `A([`Float(float_of_int(version)), payload])
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
module Modules = {
  module KitchenSink = {
    type t = Types1._AllTypes__All__rename;
    let serialize = serializeKitchenSink;
    let deserialize = deserializeKitchenSink;
  };
};
