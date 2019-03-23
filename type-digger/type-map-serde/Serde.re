[@ocaml.warning "-34"];
module Version1 = {
  type _Analyze__TopTypes__moduleName = string
  and _Asttypes__loc('a) =
    Asttypes.loc('a) = {
      txt: 'a,
      loc: _Location__t,
    }
  and _Location__t =
    Location.t = {
      loc_start: _Stdlib__lexing__position,
      loc_end: _Stdlib__lexing__position,
      loc_ghost: bool,
    }
  and _Parsetree__attribute = (_Asttypes__loc(string), _Parsetree__payload)
  and _Parsetree__attributes = list(_Parsetree__attribute)
  and _Parsetree__core_type = Parsetree.core_type
  and _Parsetree__expression = Parsetree.expression
  and _Parsetree__pattern = Parsetree.pattern
  and _Parsetree__payload =
    Parsetree.payload =
      | PStr(_Parsetree__structure)
      | PSig(_Parsetree__signature)
      | PTyp(_Parsetree__core_type)
      | PPat(_Parsetree__pattern, option(_Parsetree__expression))
  and _Parsetree__signature = Parsetree.signature
  and _Parsetree__structure = Parsetree.structure
  and _SharedTypes__SimpleType__body('source) =
    SharedTypes.SimpleType.body('source) =
      | Open
      | Abstract
      | Expr(_SharedTypes__SimpleType__expr('source))
      | Record(list((string, _SharedTypes__SimpleType__expr('source))))
      | Variant(
          list(
            (
              string,
              list(_SharedTypes__SimpleType__expr('source)),
              option(_SharedTypes__SimpleType__expr('source)),
            ),
          ),
        )
  and _SharedTypes__SimpleType__declaration('source) =
    SharedTypes.SimpleType.declaration('source) = {
      name: string,
      variables: list(_SharedTypes__SimpleType__expr('source)),
      body: _SharedTypes__SimpleType__body('source),
    }
  and _SharedTypes__SimpleType__expr('source) =
    SharedTypes.SimpleType.expr('source) =
      | Variable(string)
      | AnonVariable
      | RowVariant(
          list((string, option(_SharedTypes__SimpleType__expr('source)))),
          bool,
        )
      | Reference('source, list(_SharedTypes__SimpleType__expr('source)))
      | Tuple(list(_SharedTypes__SimpleType__expr('source)))
      | Fn(
          list((option(string), _SharedTypes__SimpleType__expr('source))),
          _SharedTypes__SimpleType__expr('source),
        )
      | Other
  and _Stdlib__hashtbl__t('a, 'b) = Stdlib__hashtbl.t('a, 'b)
  and _Stdlib__lexing__position =
    Stdlib__lexing.position = {
      pos_fname: string,
      pos_lnum: int,
      pos_bol: int,
      pos_cnum: int,
    }
  and _TypeMapSerde__Config__custom = {
    module_: string,
    path: list(string),
    name: string,
    args: int,
  }
  and _TypeMapSerde__Config__engine =
    TypeMapSerde__Config.engine = | Rex_json | Bs_json
  and _TypeMapSerde__Config__entry = {
    file: string,
    type_: string,
    publicName: option(string),
  }
  and _TypeMapSerde__Config__serializableLockfile =
    _TypeMapSerde__Config__Locked__lockfile(
      _TypeMap__DigTypes__shortReference,
    )
  and _TypeMapSerde__Config__simpleDeclaration =
    _SharedTypes__SimpleType__declaration(
      _TypeMap__DigTypes__typeSource(_TypeMap__DigTypes__shortReference),
    )
  and _TypeMapSerde__Config__simpleExpr =
    _SharedTypes__SimpleType__expr(
      _TypeMap__DigTypes__typeSource(_TypeMap__DigTypes__shortReference),
    )
  and _TypeMapSerde__Config__t = {
    version: int,
    output: string,
    engine: _TypeMapSerde__Config__engine,
    entries: list(_TypeMapSerde__Config__entry),
    custom: list(_TypeMapSerde__Config__custom),
  }
  and _TypeMapSerde__Config__Locked__lockedConfig('reference) = {
    entries: list(_TypeMapSerde__Config__Locked__lockedEntry),
    engineVersion: int,
    typeMap: _TypeMap__DigTypes__typeMap('reference),
  }
  and _TypeMapSerde__Config__Locked__lockedEntry = {
    moduleName: string,
    modulePath: list(string),
    name: string,
  }
  and _TypeMapSerde__Config__Locked__lockfile('reference) = {
    engine: _TypeMapSerde__Config__engine,
    versions: array(_TypeMapSerde__Config__Locked__lockedConfig('reference)),
  }
  and _TypeMap__DigTypes__shortReference = (
    _Analyze__TopTypes__moduleName,
    list(string),
    string,
  )
  and _TypeMap__DigTypes__typeMap('reference) =
    _Stdlib__hashtbl__t(
      _TypeMap__DigTypes__shortReference,
      (
        _Parsetree__attributes,
        _SharedTypes__SimpleType__declaration(
          _TypeMap__DigTypes__typeSource('reference),
        ),
      ),
    )
  and _TypeMap__DigTypes__typeSource('reference) =
    TypeMap__DigTypes.typeSource('reference) =
      | Builtin(string) | Public('reference) | NotFound;
  let rec deserialize_Analyze__TopTypes____moduleName:
    Json.t => Belt.Result.t(_Analyze__TopTypes__moduleName, list(string)) =
    value =>
      (
        string =>
          switch (string) {
          | Json.String(string) => Belt.Result.Ok(string)
          | _ => Error(["epected a string"])
          }
      )(
        value,
      )
  and deserialize_Asttypes____loc:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_Asttypes__loc(arg0), list(string)) =
    (aTransformer, record) =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_loc => {
          let inner = attr_txt =>
            Belt.Result.Ok({txt: attr_txt, loc: attr_loc});
          switch (Belt.List.getAssoc(items, "txt", (==))) {
          | None => Belt.Result.Error(["No attribute txt"])
          | Some(json) =>
            switch (aTransformer(json)) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute txt", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "loc", (==))) {
        | None => Belt.Result.Error(["No attribute loc"])
        | Some(json) =>
          switch (deserialize_Location____t(json)) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute loc", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Location____t:
    Json.t => Belt.Result.t(_Location__t, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_loc_ghost => {
          let inner = attr_loc_end => {
            let inner = attr_loc_start =>
              Belt.Result.Ok({
                loc_start: attr_loc_start,
                loc_end: attr_loc_end,
                loc_ghost: attr_loc_ghost,
              });
            switch (Belt.List.getAssoc(items, "loc_start", (==))) {
            | None => Belt.Result.Error(["No attribute loc_start"])
            | Some(json) =>
              switch (deserialize_Stdlib__lexing____position(json)) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute loc_start", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "loc_end", (==))) {
          | None => Belt.Result.Error(["No attribute loc_end"])
          | Some(json) =>
            switch (deserialize_Stdlib__lexing____position(json)) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute loc_end", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "loc_ghost", (==))) {
        | None => Belt.Result.Error(["No attribute loc_ghost"])
        | Some(json) =>
          switch (
            (
              bool =>
                switch (bool) {
                | Json.True => Belt.Result.Ok(true)
                | Json.False => Belt.Result.Ok(false)
                | _ => Belt.Result.Error(["Expected a bool"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute loc_ghost", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_Parsetree____attribute:
    Json.t => Belt.Result.t(_Parsetree__attribute, list(string)) =
    value =>
      (
        json =>
          switch (json) {
          | Json.Array([arg0, arg1]) =>
            switch (deserialize_Parsetree____payload(arg1)) {
            | Belt.Result.Ok(arg1) =>
              switch (
                (
                  deserialize_Asttypes____loc(string =>
                    switch (string) {
                    | Json.String(string) => Belt.Result.Ok(string)
                    | _ => Error(["epected a string"])
                    }
                  )
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
          | _ => Belt.Result.Error(["Expected array"])
          }
      )(
        value,
      )
  and deserialize_Parsetree____attributes:
    Json.t => Belt.Result.t(_Parsetree__attributes, list(string)) =
    value =>
      (
        list =>
          switch (list) {
          | Json.Array(items) =>
            let transformer = deserialize_Parsetree____attribute;
            let rec loop = (collected, items) =>
              switch (items) {
              | [] => Belt.Result.Ok(Belt.List.reverse(collected))
              | [one, ...rest] =>
                switch (transformer(one)) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["list item", ...error])
                | Belt.Result.Ok(value) => loop([value, ...collected], rest)
                }
              };
            loop([], items);
          | _ => Belt.Result.Error(["expected an array"])
          }
      )(
        value,
      )
  and deserialize_Parsetree____core_type:
    Json.t => Belt.Result.t(_Parsetree__core_type, list(string)) = TransformHelpers.deserialize_Parsetree____core_type
  and deserialize_Parsetree____expression:
    Json.t => Belt.Result.t(_Parsetree__expression, list(string)) = TransformHelpers.deserialize_Parsetree____expression
  and deserialize_Parsetree____pattern:
    Json.t => Belt.Result.t(_Parsetree__pattern, list(string)) = TransformHelpers.deserialize_Parsetree____pattern
  and deserialize_Parsetree____payload:
    Json.t => Belt.Result.t(_Parsetree__payload, list(string)) =
    constructor =>
      switch (constructor) {
      | Json.Array([Json.String(tag), arg0]) when "PStr" == tag =>
        switch (deserialize_Parsetree____structure(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] PStr(arg0): _Parsetree__payload)
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0]) when "PSig" == tag =>
        switch (deserialize_Parsetree____signature(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] PSig(arg0): _Parsetree__payload)
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0]) when "PTyp" == tag =>
        switch (deserialize_Parsetree____core_type(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok([@implicit_arity] PTyp(arg0): _Parsetree__payload)
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0, arg1]) when "PPat" == tag =>
        switch (
          (
            (
              (transformer, option) =>
                switch (option) {
                | Json.Null => Belt.Result.Ok(None)
                | _ =>
                  switch (transformer(option)) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["optional value", ...error])
                  | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                  }
                }
            )(
              deserialize_Parsetree____expression,
            )
          )(
            arg1,
          )
        ) {
        | Belt.Result.Ok(arg1) =>
          switch (deserialize_Parsetree____pattern(arg0)) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] PPat(arg0, arg1): _Parsetree__payload,
            )
          | Error(error) => Error(["constructor argument 0", ...error])
          }
        | Error(error) => Error(["constructor argument 1", ...error])
        }
      | Json.Array([Json.String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_Parsetree____signature:
    Json.t => Belt.Result.t(_Parsetree__signature, list(string)) = TransformHelpers.deserialize_Parsetree____signature
  and deserialize_Parsetree____structure:
    Json.t => Belt.Result.t(_Parsetree__structure, list(string)) = TransformHelpers.deserialize_Parsetree____structure
  and deserialize_SharedTypes__SimpleType__body:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_SharedTypes__SimpleType__body(arg0), list(string)) =
    (sourceTransformer, constructor) =>
      switch (constructor) {
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "Open" == tag =>
        Belt.Result.Ok(Open: _SharedTypes__SimpleType__body(arg0))
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "Abstract" == tag =>
        Belt.Result.Ok(Abstract: _SharedTypes__SimpleType__body(arg0))
      | Json.Array([Json.String(tag), arg0]) when "Expr" == tag =>
        switch (
          (deserialize_SharedTypes__SimpleType__expr(sourceTransformer))(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Expr(arg0):
                                           _SharedTypes__SimpleType__body(
                                             arg0,
                                           ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0]) when "Record" == tag =>
        switch (
          (
            list =>
              switch (list) {
              | Json.Array(items) =>
                let transformer = json =>
                  switch (json) {
                  | Json.Array([arg0, arg1]) =>
                    switch (
                      (
                        deserialize_SharedTypes__SimpleType__expr(
                          sourceTransformer,
                        )
                      )(
                        arg1,
                      )
                    ) {
                    | Belt.Result.Ok(arg1) =>
                      switch (
                        (
                          string =>
                            switch (string) {
                            | Json.String(string) => Belt.Result.Ok(string)
                            | _ => Error(["epected a string"])
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
                  | _ => Belt.Result.Error(["Expected array"])
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
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Record(arg0):
                                             _SharedTypes__SimpleType__body(
                                               arg0,
                                             ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0]) when "Variant" == tag =>
        switch (
          (
            list =>
              switch (list) {
              | Json.Array(items) =>
                let transformer = json =>
                  switch (json) {
                  | Json.Array([arg0, arg1, arg2]) =>
                    switch (
                      (
                        (
                          (transformer, option) =>
                            switch (option) {
                            | Json.Null => Belt.Result.Ok(None)
                            | _ =>
                              switch (transformer(option)) {
                              | Belt.Result.Error(error) =>
                                Belt.Result.Error([
                                  "optional value",
                                  ...error,
                                ])
                              | Belt.Result.Ok(value) =>
                                Belt.Result.Ok(Some(value))
                              }
                            }
                        )(
                          deserialize_SharedTypes__SimpleType__expr(
                            sourceTransformer,
                          ),
                        )
                      )(
                        arg2,
                      )
                    ) {
                    | Belt.Result.Ok(arg2) =>
                      switch (
                        (
                          list =>
                            switch (list) {
                            | Json.Array(items) =>
                              let transformer =
                                deserialize_SharedTypes__SimpleType__expr(
                                  sourceTransformer,
                                );
                              let rec loop = (collected, items) =>
                                switch (items) {
                                | [] =>
                                  Belt.Result.Ok(
                                    Belt.List.reverse(collected),
                                  )
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
                          arg1,
                        )
                      ) {
                      | Belt.Result.Ok(arg1) =>
                        switch (
                          (
                            string =>
                              switch (string) {
                              | Json.String(string) => Belt.Result.Ok(string)
                              | _ => Error(["epected a string"])
                              }
                          )(
                            arg0,
                          )
                        ) {
                        | Belt.Result.Ok(arg0) =>
                          [@implicit_arity] Belt.Result.Ok(arg0, arg1, arg2)
                        | Error(error) =>
                          Error(["tuple element 0", ...error])
                        }
                      | Error(error) => Error(["tuple element 1", ...error])
                      }
                    | Error(error) => Error(["tuple element 2", ...error])
                    }
                  | _ => Belt.Result.Error(["Expected array"])
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
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Variant(arg0):
                                              _SharedTypes__SimpleType__body(
                                                arg0,
                                              ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_SharedTypes__SimpleType__declaration:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(
        _SharedTypes__SimpleType__declaration(arg0),
        list(string),
      ) =
    (sourceTransformer, record) =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_body => {
          let inner = attr_variables => {
            let inner = attr_name =>
              Belt.Result.Ok({
                name: attr_name,
                variables: attr_variables,
                body: attr_body,
              });
            switch (Belt.List.getAssoc(items, "name", (==))) {
            | None => Belt.Result.Error(["No attribute name"])
            | Some(json) =>
              switch (
                (
                  string =>
                    switch (string) {
                    | Json.String(string) => Belt.Result.Ok(string)
                    | _ => Error(["epected a string"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute name", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "variables", (==))) {
          | None => Belt.Result.Error(["No attribute variables"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | Json.Array(items) =>
                    let transformer =
                      deserialize_SharedTypes__SimpleType__expr(
                        sourceTransformer,
                      );
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
              Belt.Result.Error(["attribute variables", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "body", (==))) {
        | None => Belt.Result.Error(["No attribute body"])
        | Some(json) =>
          switch (
            (deserialize_SharedTypes__SimpleType__body(sourceTransformer))(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute body", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_SharedTypes__SimpleType__expr:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_SharedTypes__SimpleType__expr(arg0), list(string)) =
    (sourceTransformer, constructor) =>
      switch (constructor) {
      | Json.Array([Json.String(tag), arg0]) when "Variable" == tag =>
        switch (
          (
            string =>
              switch (string) {
              | Json.String(string) => Belt.Result.Ok(string)
              | _ => Error(["epected a string"])
              }
          )(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Variable(arg0):
                                               _SharedTypes__SimpleType__expr(
                                                 arg0,
                                               ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "AnonVariable" == tag =>
        Belt.Result.Ok(AnonVariable: _SharedTypes__SimpleType__expr(arg0))
      | Json.Array([Json.String(tag), arg0, arg1]) when "RowVariant" == tag =>
        switch (
          (
            bool =>
              switch (bool) {
              | Json.True => Belt.Result.Ok(true)
              | Json.False => Belt.Result.Ok(false)
              | _ => Belt.Result.Error(["Expected a bool"])
              }
          )(
            arg1,
          )
        ) {
        | Belt.Result.Ok(arg1) =>
          switch (
            (
              list =>
                switch (list) {
                | Json.Array(items) =>
                  let transformer = json =>
                    switch (json) {
                    | Json.Array([arg0, arg1]) =>
                      switch (
                        (
                          (
                            (transformer, option) =>
                              switch (option) {
                              | Json.Null => Belt.Result.Ok(None)
                              | _ =>
                                switch (transformer(option)) {
                                | Belt.Result.Error(error) =>
                                  Belt.Result.Error([
                                    "optional value",
                                    ...error,
                                  ])
                                | Belt.Result.Ok(value) =>
                                  Belt.Result.Ok(Some(value))
                                }
                              }
                          )(
                            deserialize_SharedTypes__SimpleType__expr(
                              sourceTransformer,
                            ),
                          )
                        )(
                          arg1,
                        )
                      ) {
                      | Belt.Result.Ok(arg1) =>
                        switch (
                          (
                            string =>
                              switch (string) {
                              | Json.String(string) => Belt.Result.Ok(string)
                              | _ => Error(["epected a string"])
                              }
                          )(
                            arg0,
                          )
                        ) {
                        | Belt.Result.Ok(arg0) =>
                          [@implicit_arity] Belt.Result.Ok(arg0, arg1)
                        | Error(error) =>
                          Error(["tuple element 0", ...error])
                        }
                      | Error(error) => Error(["tuple element 1", ...error])
                      }
                    | _ => Belt.Result.Error(["Expected array"])
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
              arg0,
            )
          ) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] RowVariant(arg0, arg1):
                                                         _SharedTypes__SimpleType__expr(
                                                           arg0,
                                                         ),
            )
          | Error(error) => Error(["constructor argument 0", ...error])
          }
        | Error(error) => Error(["constructor argument 1", ...error])
        }
      | Json.Array([Json.String(tag), arg0, arg1]) when "Reference" == tag =>
        switch (
          (
            list =>
              switch (list) {
              | Json.Array(items) =>
                let transformer =
                  deserialize_SharedTypes__SimpleType__expr(
                    sourceTransformer,
                  );
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
            arg1,
          )
        ) {
        | Belt.Result.Ok(arg1) =>
          switch (sourceTransformer(arg0)) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] Reference(arg0, arg1):
                                                        _SharedTypes__SimpleType__expr(
                                                          arg0,
                                                        ),
            )
          | Error(error) => Error(["constructor argument 0", ...error])
          }
        | Error(error) => Error(["constructor argument 1", ...error])
        }
      | Json.Array([Json.String(tag), arg0]) when "Tuple" == tag =>
        switch (
          (
            list =>
              switch (list) {
              | Json.Array(items) =>
                let transformer =
                  deserialize_SharedTypes__SimpleType__expr(
                    sourceTransformer,
                  );
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
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Tuple(arg0):
                                            _SharedTypes__SimpleType__expr(
                                              arg0,
                                            ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0, arg1]) when "Fn" == tag =>
        switch (
          (deserialize_SharedTypes__SimpleType__expr(sourceTransformer))(
            arg1,
          )
        ) {
        | Belt.Result.Ok(arg1) =>
          switch (
            (
              list =>
                switch (list) {
                | Json.Array(items) =>
                  let transformer = json =>
                    switch (json) {
                    | Json.Array([arg0, arg1]) =>
                      switch (
                        (
                          deserialize_SharedTypes__SimpleType__expr(
                            sourceTransformer,
                          )
                        )(
                          arg1,
                        )
                      ) {
                      | Belt.Result.Ok(arg1) =>
                        switch (
                          (
                            (
                              (transformer, option) =>
                                switch (option) {
                                | Json.Null => Belt.Result.Ok(None)
                                | _ =>
                                  switch (transformer(option)) {
                                  | Belt.Result.Error(error) =>
                                    Belt.Result.Error([
                                      "optional value",
                                      ...error,
                                    ])
                                  | Belt.Result.Ok(value) =>
                                    Belt.Result.Ok(Some(value))
                                  }
                                }
                            )(
                              string =>
                              switch (string) {
                              | Json.String(string) => Belt.Result.Ok(string)
                              | _ => Error(["epected a string"])
                              }
                            )
                          )(
                            arg0,
                          )
                        ) {
                        | Belt.Result.Ok(arg0) =>
                          [@implicit_arity] Belt.Result.Ok(arg0, arg1)
                        | Error(error) =>
                          Error(["tuple element 0", ...error])
                        }
                      | Error(error) => Error(["tuple element 1", ...error])
                      }
                    | _ => Belt.Result.Error(["Expected array"])
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
              arg0,
            )
          ) {
          | Belt.Result.Ok(arg0) =>
            Belt.Result.Ok(
              [@implicit_arity] Fn(arg0, arg1):
                                                 _SharedTypes__SimpleType__expr(
                                                   arg0,
                                                 ),
            )
          | Error(error) => Error(["constructor argument 0", ...error])
          }
        | Error(error) => Error(["constructor argument 1", ...error])
        }
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "Other" == tag =>
        Belt.Result.Ok(Other: _SharedTypes__SimpleType__expr(arg0))
      | Json.Array([Json.String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_Stdlib__hashtbl____t:
    'arg0 'arg1.
    (
      Json.t => Belt.Result.t('arg0, list(string)),
      Json.t => Belt.Result.t('arg1, list(string)),
      Json.t
    ) =>
    Belt.Result.t(_Stdlib__hashtbl__t('arg0, 'arg1), list(string))
   =
    (type arg1, type arg0) => (
      (aTransformer, bTransformer) =>
        TransformHelpers.deserialize_Stdlib__hashtbl____t(
          aTransformer,
          bTransformer,
        ):
        (
          Json.t => Belt.Result.t(arg0, list(string)),
          Json.t => Belt.Result.t(arg1, list(string)),
          Json.t
        ) =>
        Belt.Result.t(_Stdlib__hashtbl__t(arg0, arg1), list(string))
    )
  and deserialize_Stdlib__lexing____position:
    Json.t => Belt.Result.t(_Stdlib__lexing__position, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_pos_cnum => {
          let inner = attr_pos_bol => {
            let inner = attr_pos_lnum => {
              let inner = attr_pos_fname =>
                Belt.Result.Ok({
                  pos_fname: attr_pos_fname,
                  pos_lnum: attr_pos_lnum,
                  pos_bol: attr_pos_bol,
                  pos_cnum: attr_pos_cnum,
                });
              switch (Belt.List.getAssoc(items, "pos_fname", (==))) {
              | None => Belt.Result.Error(["No attribute pos_fname"])
              | Some(json) =>
                switch (
                  (
                    string =>
                      switch (string) {
                      | Json.String(string) => Belt.Result.Ok(string)
                      | _ => Error(["epected a string"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute pos_fname", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "pos_lnum", (==))) {
            | None => Belt.Result.Error(["No attribute pos_lnum"])
            | Some(json) =>
              switch (
                (
                  number =>
                    switch (number) {
                    | Json.Number(number) =>
                      Belt.Result.Ok(int_of_float(number))
                    | _ => Error(["Expected a float"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute pos_lnum", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "pos_bol", (==))) {
          | None => Belt.Result.Error(["No attribute pos_bol"])
          | Some(json) =>
            switch (
              (
                number =>
                  switch (number) {
                  | Json.Number(number) =>
                    Belt.Result.Ok(int_of_float(number))
                  | _ => Error(["Expected a float"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute pos_bol", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "pos_cnum", (==))) {
        | None => Belt.Result.Error(["No attribute pos_cnum"])
        | Some(json) =>
          switch (
            (
              number =>
                switch (number) {
                | Json.Number(number) =>
                  Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute pos_cnum", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____custom:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__custom, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_args => {
          let inner = attr_name => {
            let inner = attr_path => {
              let inner = attr_module_ =>
                Belt.Result.Ok({
                  module_: attr_module_,
                  path: attr_path,
                  name: attr_name,
                  args: attr_args,
                });
              switch (Belt.List.getAssoc(items, "module", (==))) {
              | None => Belt.Result.Error(["No attribute module"])
              | Some(json) =>
                switch (
                  (
                    string =>
                      switch (string) {
                      | Json.String(string) => Belt.Result.Ok(string)
                      | _ => Error(["epected a string"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute module", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "path", (==))) {
            | None => Belt.Result.Error(["No attribute path"])
            | Some(json) =>
              switch (
                (
                  list =>
                    switch (list) {
                    | Json.Array(items) =>
                      let transformer = string =>
                        switch (string) {
                        | Json.String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
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
                Belt.Result.Error(["attribute path", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "name", (==))) {
          | None => Belt.Result.Error(["No attribute name"])
          | Some(json) =>
            switch (
              (
                string =>
                  switch (string) {
                  | Json.String(string) => Belt.Result.Ok(string)
                  | _ => Error(["epected a string"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute name", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "args", (==))) {
        | None => Belt.Result.Error(["No attribute args"])
        | Some(json) =>
          switch (
            (
              number =>
                switch (number) {
                | Json.Number(number) =>
                  Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute args", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____engine:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__engine, list(string)) =
    constructor =>
      switch (constructor) {
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "rex-json" == tag =>
        Belt.Result.Ok(Rex_json: _TypeMapSerde__Config__engine)
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "Js.Json" == tag =>
        Belt.Result.Ok(Bs_json: _TypeMapSerde__Config__engine)
      | Json.Array([Json.String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      }
  and deserialize_TypeMapSerde__Config____entry:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__entry, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_publicName => {
          let inner = attr_type_ => {
            let inner = attr_file =>
              Belt.Result.Ok({
                file: attr_file,
                type_: attr_type_,
                publicName: attr_publicName,
              });
            switch (Belt.List.getAssoc(items, "file", (==))) {
            | None => Belt.Result.Error(["No attribute file"])
            | Some(json) =>
              switch (
                (
                  string =>
                    switch (string) {
                    | Json.String(string) => Belt.Result.Ok(string)
                    | _ => Error(["epected a string"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute file", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "type", (==))) {
          | None => Belt.Result.Error(["No attribute type"])
          | Some(json) =>
            switch (
              (
                string =>
                  switch (string) {
                  | Json.String(string) => Belt.Result.Ok(string)
                  | _ => Error(["epected a string"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute type", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "publicName", (==))) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | Json.Null => Belt.Result.Ok(None)
                  | _ =>
                    switch (transformer(option)) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["optional value", ...error])
                    | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                    }
                  }
              )(
                string =>
                switch (string) {
                | Json.String(string) => Belt.Result.Ok(string)
                | _ => Error(["epected a string"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute publicName", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____serializableLockfile:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__serializableLockfile, list(string)) =
    value =>
      (
        deserialize_TypeMapSerde__Config__Locked__lockfile(
          deserialize_TypeMap__DigTypes____shortReference,
        )
      )(
        value,
      )
  and deserialize_TypeMapSerde__Config____simpleDeclaration:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__simpleDeclaration, list(string)) =
    value =>
      (
        deserialize_SharedTypes__SimpleType__declaration(
          deserialize_TypeMap__DigTypes____typeSource(
            deserialize_TypeMap__DigTypes____shortReference,
          ),
        )
      )(
        value,
      )
  and deserialize_TypeMapSerde__Config____simpleExpr:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__simpleExpr, list(string)) =
    value =>
      (
        deserialize_SharedTypes__SimpleType__expr(
          deserialize_TypeMap__DigTypes____typeSource(
            deserialize_TypeMap__DigTypes____shortReference,
          ),
        )
      )(
        value,
      )
  and deserialize_TypeMapSerde__Config____t:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__t, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_custom => {
          let inner = attr_entries => {
            let inner = attr_engine => {
              let inner = attr_output => {
                let inner = attr_version =>
                  Belt.Result.Ok({
                    version: attr_version,
                    output: attr_output,
                    engine: attr_engine,
                    entries: attr_entries,
                    custom: attr_custom,
                  });
                switch (Belt.List.getAssoc(items, "version", (==))) {
                | None => Belt.Result.Error(["No attribute version"])
                | Some(json) =>
                  switch (
                    (
                      number =>
                        switch (number) {
                        | Json.Number(number) =>
                          Belt.Result.Ok(int_of_float(number))
                        | _ => Error(["Expected a float"])
                        }
                    )(
                      json,
                    )
                  ) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute version", ...error])
                  | Belt.Result.Ok(data) => inner(data)
                  }
                };
              };
              switch (Belt.List.getAssoc(items, "output", (==))) {
              | None => Belt.Result.Error(["No attribute output"])
              | Some(json) =>
                switch (
                  (
                    string =>
                      switch (string) {
                      | Json.String(string) => Belt.Result.Ok(string)
                      | _ => Error(["epected a string"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute output", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "engine", (==))) {
            | None => Belt.Result.Error(["No attribute engine"])
            | Some(json) =>
              switch (deserialize_TypeMapSerde__Config____engine(json)) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute engine", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "entries", (==))) {
          | None => Belt.Result.Error(["No attribute entries"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | Json.Array(items) =>
                    let transformer = deserialize_TypeMapSerde__Config____entry;
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
              Belt.Result.Error(["attribute entries", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "custom", (==))) {
        | None => Belt.Result.Error(["No attribute custom"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (list) {
                | Json.Array(items) =>
                  let transformer = deserialize_TypeMapSerde__Config____custom;
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
            Belt.Result.Error(["attribute custom", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config__Locked__lockedConfig:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(
        _TypeMapSerde__Config__Locked__lockedConfig(arg0),
        list(string),
      ) =
    (referenceTransformer, record) =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_typeMap => {
          let inner = attr_engineVersion => {
            let inner = attr_entries =>
              Belt.Result.Ok({
                entries: attr_entries,
                engineVersion: attr_engineVersion,
                typeMap: attr_typeMap,
              });
            switch (Belt.List.getAssoc(items, "entries", (==))) {
            | None => Belt.Result.Error(["No attribute entries"])
            | Some(json) =>
              switch (
                (
                  list =>
                    switch (list) {
                    | Json.Array(items) =>
                      let transformer = deserialize_TypeMapSerde__Config__Locked__lockedEntry;
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
                Belt.Result.Error(["attribute entries", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "engineVersion", (==))) {
          | None => Belt.Result.Error(["No attribute engineVersion"])
          | Some(json) =>
            switch (
              (
                number =>
                  switch (number) {
                  | Json.Number(number) =>
                    Belt.Result.Ok(int_of_float(number))
                  | _ => Error(["Expected a float"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute engineVersion", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "typeMap", (==))) {
        | None => Belt.Result.Error(["No attribute typeMap"])
        | Some(json) =>
          switch (
            (deserialize_TypeMap__DigTypes____typeMap(referenceTransformer))(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute typeMap", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config__Locked__lockedEntry:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__Locked__lockedEntry, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_name => {
          let inner = attr_modulePath => {
            let inner = attr_moduleName =>
              Belt.Result.Ok({
                moduleName: attr_moduleName,
                modulePath: attr_modulePath,
                name: attr_name,
              });
            switch (Belt.List.getAssoc(items, "moduleName", (==))) {
            | None => Belt.Result.Error(["No attribute moduleName"])
            | Some(json) =>
              switch (
                (
                  string =>
                    switch (string) {
                    | Json.String(string) => Belt.Result.Ok(string)
                    | _ => Error(["epected a string"])
                    }
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute moduleName", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "modulePath", (==))) {
          | None => Belt.Result.Error(["No attribute modulePath"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | Json.Array(items) =>
                    let transformer = string =>
                      switch (string) {
                      | Json.String(string) => Belt.Result.Ok(string)
                      | _ => Error(["epected a string"])
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
              Belt.Result.Error(["attribute modulePath", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "name", (==))) {
        | None => Belt.Result.Error(["No attribute name"])
        | Some(json) =>
          switch (
            (
              string =>
                switch (string) {
                | Json.String(string) => Belt.Result.Ok(string)
                | _ => Error(["epected a string"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute name", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config__Locked__lockfile:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(
        _TypeMapSerde__Config__Locked__lockfile(arg0),
        list(string),
      ) =
    (referenceTransformer, record) =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_versions => {
          let inner = attr_engine =>
            Belt.Result.Ok({engine: attr_engine, versions: attr_versions});
          switch (Belt.List.getAssoc(items, "engine", (==))) {
          | None => Belt.Result.Error(["No attribute engine"])
          | Some(json) =>
            switch (deserialize_TypeMapSerde__Config____engine(json)) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute engine", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "versions", (==))) {
        | None => Belt.Result.Error(["No attribute versions"])
        | Some(json) =>
          switch (
            (
              (
                (transformer, array) =>
                  switch (array) {
                  | Json.Array(items) =>
                    let rec loop = (collected, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok(Belt.List.reverse(collected))
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(["array element", ...error])
                        | Belt.Result.Ok(value) =>
                          loop([value, ...collected], rest)
                        }
                      };
                    switch (loop([], items)) {
                    | Belt.Result.Error(error) => Belt.Result.Error(error)
                    | Belt.Result.Ok(value) =>
                      Belt.Result.Ok(Belt.List.toArray(value))
                    };
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                deserialize_TypeMapSerde__Config__Locked__lockedConfig(
                  referenceTransformer,
                ),
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute versions", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMap__DigTypes____shortReference:
    Json.t => Belt.Result.t(_TypeMap__DigTypes__shortReference, list(string)) =
    value =>
      (
        json =>
          switch (json) {
          | Json.Array([arg0, arg1, arg2]) =>
            switch (
              (
                string =>
                  switch (string) {
                  | Json.String(string) => Belt.Result.Ok(string)
                  | _ => Error(["epected a string"])
                  }
              )(
                arg2,
              )
            ) {
            | Belt.Result.Ok(arg2) =>
              switch (
                (
                  list =>
                    switch (list) {
                    | Json.Array(items) =>
                      let transformer = string =>
                        switch (string) {
                        | Json.String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
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
                  arg1,
                )
              ) {
              | Belt.Result.Ok(arg1) =>
                switch (deserialize_Analyze__TopTypes____moduleName(arg0)) {
                | Belt.Result.Ok(arg0) =>
                  [@implicit_arity] Belt.Result.Ok(arg0, arg1, arg2)
                | Error(error) => Error(["tuple element 0", ...error])
                }
              | Error(error) => Error(["tuple element 1", ...error])
              }
            | Error(error) => Error(["tuple element 2", ...error])
            }
          | _ => Belt.Result.Error(["Expected array"])
          }
      )(
        value,
      )
  and deserialize_TypeMap__DigTypes____typeMap:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_TypeMap__DigTypes__typeMap(arg0), list(string)) =
    (referenceTransformer, value) =>
      (
        deserialize_Stdlib__hashtbl____t(
          deserialize_TypeMap__DigTypes____shortReference, json =>
          switch (json) {
          | Json.Array([arg0, arg1]) =>
            switch (
              (
                deserialize_SharedTypes__SimpleType__declaration(
                  deserialize_TypeMap__DigTypes____typeSource(
                    referenceTransformer,
                  ),
                )
              )(
                arg1,
              )
            ) {
            | Belt.Result.Ok(arg1) =>
              switch (deserialize_Parsetree____attributes(arg0)) {
              | Belt.Result.Ok(arg0) =>
                [@implicit_arity] Belt.Result.Ok(arg0, arg1)
              | Error(error) => Error(["tuple element 0", ...error])
              }
            | Error(error) => Error(["tuple element 1", ...error])
            }
          | _ => Belt.Result.Error(["Expected array"])
          }
        )
      )(
        value,
      )
  and deserialize_TypeMap__DigTypes____typeSource:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_TypeMap__DigTypes__typeSource(arg0), list(string)) =
    (referenceTransformer, constructor) =>
      switch (constructor) {
      | Json.Array([Json.String(tag), arg0]) when "Builtin" == tag =>
        switch (
          (
            string =>
              switch (string) {
              | Json.String(string) => Belt.Result.Ok(string)
              | _ => Error(["epected a string"])
              }
          )(
            arg0,
          )
        ) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Builtin(arg0):
                                              _TypeMap__DigTypes__typeSource(
                                                arg0,
                                              ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag), arg0]) when "Public" == tag =>
        switch (referenceTransformer(arg0)) {
        | Belt.Result.Ok(arg0) =>
          Belt.Result.Ok(
            [@implicit_arity] Public(arg0):
                                             _TypeMap__DigTypes__typeSource(
                                               arg0,
                                             ),
          )
        | Error(error) => Error(["constructor argument 0", ...error])
        }
      | Json.Array([Json.String(tag)])
      | Json.String(tag) when "NotFound" == tag =>
        Belt.Result.Ok(NotFound: _TypeMap__DigTypes__typeSource(arg0))
      | Json.Array([Json.String(tag), ..._]) =>
        Belt.Result.Error(["Invalid constructor: " ++ tag])
      | _ => Belt.Result.Error(["Expected an array"])
      };
};
module Version2 = {
  type _Analyze__TopTypes__moduleName = string
  and _Asttypes__loc('a) =
    Asttypes.loc('a) = {
      txt: 'a,
      loc: _Location__t,
    }
  and _Location__t =
    Location.t = {
      loc_start: _Stdlib__lexing__position,
      loc_end: _Stdlib__lexing__position,
      loc_ghost: bool,
    }
  and _Parsetree__attribute = (_Asttypes__loc(string), _Parsetree__payload)
  and _Parsetree__attributes = list(_Parsetree__attribute)
  and _Parsetree__core_type = Parsetree.core_type
  and _Parsetree__expression = Parsetree.expression
  and _Parsetree__pattern = Parsetree.pattern
  and _Parsetree__payload =
    Parsetree.payload =
      | PStr(_Parsetree__structure)
      | PSig(_Parsetree__signature)
      | PTyp(_Parsetree__core_type)
      | PPat(_Parsetree__pattern, option(_Parsetree__expression))
  and _Parsetree__signature = Parsetree.signature
  and _Parsetree__structure = Parsetree.structure
  and _SharedTypes__SimpleType__body('source) =
    SharedTypes.SimpleType.body('source) =
      | Open
      | Abstract
      | Expr(_SharedTypes__SimpleType__expr('source))
      | Record(list((string, _SharedTypes__SimpleType__expr('source))))
      | Variant(
          list(
            (
              string,
              list(_SharedTypes__SimpleType__expr('source)),
              option(_SharedTypes__SimpleType__expr('source)),
            ),
          ),
        )
  and _SharedTypes__SimpleType__declaration('source) =
    SharedTypes.SimpleType.declaration('source) = {
      name: string,
      variables: list(_SharedTypes__SimpleType__expr('source)),
      body: _SharedTypes__SimpleType__body('source),
    }
  and _SharedTypes__SimpleType__expr('source) =
    SharedTypes.SimpleType.expr('source) =
      | Variable(string)
      | AnonVariable
      | RowVariant(
          list((string, option(_SharedTypes__SimpleType__expr('source)))),
          bool,
        )
      | Reference('source, list(_SharedTypes__SimpleType__expr('source)))
      | Tuple(list(_SharedTypes__SimpleType__expr('source)))
      | Fn(
          list((option(string), _SharedTypes__SimpleType__expr('source))),
          _SharedTypes__SimpleType__expr('source),
        )
      | Other
  and _Stdlib__hashtbl__t('a, 'b) = Stdlib__hashtbl.t('a, 'b)
  and _Stdlib__lexing__position =
    Stdlib__lexing.position = {
      pos_fname: string,
      pos_lnum: int,
      pos_bol: int,
      pos_cnum: int,
    }
  and _TypeMapSerde__Config__custom =
    TypeMapSerde__Config.custom = {
      module_: string,
      path: list(string),
      name: string,
      args: option(int),
    }
  and _TypeMapSerde__Config__engine =
    TypeMapSerde__Config.engine = | Rex_json | Bs_json
  and _TypeMapSerde__Config__engineConfig =
    TypeMapSerde__Config.engineConfig = {
      output: string,
      helpers: option(string),
    }
  and _TypeMapSerde__Config__engines =
    TypeMapSerde__Config.engines = {
      rex_json: option(_TypeMapSerde__Config__engineConfig),
      bs_json: option(_TypeMapSerde__Config__engineConfig),
    }
  and _TypeMapSerde__Config__entry =
    TypeMapSerde__Config.entry = {
      file: string,
      type_: string,
      engines: option(list(_TypeMapSerde__Config__engine)),
      publicName: option(string),
      history: option(bool),
      minVersion: option(int),
    }
  and _TypeMapSerde__Config__serializableLockfile =
    _TypeMapSerde__Config__Locked__lockfile(
      _TypeMap__DigTypes__shortReference,
    )
  and _TypeMapSerde__Config__simpleDeclaration =
    _SharedTypes__SimpleType__declaration(
      _TypeMap__DigTypes__typeSource(_TypeMap__DigTypes__shortReference),
    )
  and _TypeMapSerde__Config__simpleExpr =
    _SharedTypes__SimpleType__expr(
      _TypeMap__DigTypes__typeSource(_TypeMap__DigTypes__shortReference),
    )
  and _TypeMapSerde__Config__t =
    TypeMapSerde__Config.t = {
      version: int,
      minVersion: option(int),
      outputTypes: option(string),
      engines: _TypeMapSerde__Config__engines,
      globalEngines: option(list(_TypeMapSerde__Config__engine)),
      entries: list(_TypeMapSerde__Config__entry),
      custom: list(_TypeMapSerde__Config__custom),
    }
  and _TypeMapSerde__Config__Locked__lockedConfig('reference) =
    TypeMapSerde__Config.Locked.lockedConfig('reference) = {
      entries: list(_TypeMapSerde__Config__Locked__lockedEntry),
      typeMap: _TypeMap__DigTypes__typeMap('reference),
    }
  and _TypeMapSerde__Config__Locked__lockedEntry =
    TypeMapSerde__Config.Locked.lockedEntry = {
      moduleName: string,
      modulePath: list(string),
      engines: list((_TypeMapSerde__Config__engine, int)),
      name: string,
    }
  and _TypeMapSerde__Config__Locked__lockfile('reference) =
    TypeMapSerde__Config.Locked.lockfile('reference) = {
      versions:
        array(_TypeMapSerde__Config__Locked__lockedConfig('reference)),
    }
  and _TypeMap__DigTypes__shortReference = (
    _Analyze__TopTypes__moduleName,
    list(string),
    string,
  )
  and _TypeMap__DigTypes__typeMap('reference) =
    _Stdlib__hashtbl__t(
      _TypeMap__DigTypes__shortReference,
      (
        _Parsetree__attributes,
        _SharedTypes__SimpleType__declaration(
          _TypeMap__DigTypes__typeSource('reference),
        ),
      ),
    )
  and _TypeMap__DigTypes__typeSource('reference) =
    TypeMap__DigTypes.typeSource('reference) =
      | Builtin(string) | Public('reference) | NotFound;
  let rec deserialize_Analyze__TopTypes____moduleName:
    Json.t => Belt.Result.t(_Analyze__TopTypes__moduleName, list(string)) = Version1.deserialize_Analyze__TopTypes____moduleName
  and deserialize_Asttypes____loc:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_Asttypes__loc(arg0), list(string)) = Version1.deserialize_Asttypes____loc
  and deserialize_Location____t:
    Json.t => Belt.Result.t(_Location__t, list(string)) = Version1.deserialize_Location____t
  and deserialize_Parsetree____attribute:
    Json.t => Belt.Result.t(_Parsetree__attribute, list(string)) = Version1.deserialize_Parsetree____attribute
  and deserialize_Parsetree____attributes:
    Json.t => Belt.Result.t(_Parsetree__attributes, list(string)) = Version1.deserialize_Parsetree____attributes
  and deserialize_Parsetree____core_type:
    Json.t => Belt.Result.t(_Parsetree__core_type, list(string)) = Version1.deserialize_Parsetree____core_type
  and deserialize_Parsetree____expression:
    Json.t => Belt.Result.t(_Parsetree__expression, list(string)) = Version1.deserialize_Parsetree____expression
  and deserialize_Parsetree____pattern:
    Json.t => Belt.Result.t(_Parsetree__pattern, list(string)) = Version1.deserialize_Parsetree____pattern
  and deserialize_Parsetree____payload:
    Json.t => Belt.Result.t(_Parsetree__payload, list(string)) = Version1.deserialize_Parsetree____payload
  and deserialize_Parsetree____signature:
    Json.t => Belt.Result.t(_Parsetree__signature, list(string)) = Version1.deserialize_Parsetree____signature
  and deserialize_Parsetree____structure:
    Json.t => Belt.Result.t(_Parsetree__structure, list(string)) = Version1.deserialize_Parsetree____structure
  and deserialize_SharedTypes__SimpleType__body:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_SharedTypes__SimpleType__body(arg0), list(string)) = Version1.deserialize_SharedTypes__SimpleType__body
  and deserialize_SharedTypes__SimpleType__declaration:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(
        _SharedTypes__SimpleType__declaration(arg0),
        list(string),
      ) = Version1.deserialize_SharedTypes__SimpleType__declaration
  and deserialize_SharedTypes__SimpleType__expr:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_SharedTypes__SimpleType__expr(arg0), list(string)) = Version1.deserialize_SharedTypes__SimpleType__expr
  and deserialize_Stdlib__hashtbl____t:
    'arg0 'arg1.
    (
      Json.t => Belt.Result.t('arg0, list(string)),
      Json.t => Belt.Result.t('arg1, list(string)),
      Json.t
    ) =>
    Belt.Result.t(_Stdlib__hashtbl__t('arg0, 'arg1), list(string))
   =
    (type arg1, type arg0) => (
      Version1.deserialize_Stdlib__hashtbl____t:
        (
          Json.t => Belt.Result.t(arg0, list(string)),
          Json.t => Belt.Result.t(arg1, list(string)),
          Json.t
        ) =>
        Belt.Result.t(_Stdlib__hashtbl__t(arg0, arg1), list(string))
    )
  and deserialize_Stdlib__lexing____position:
    Json.t => Belt.Result.t(_Stdlib__lexing__position, list(string)) = Version1.deserialize_Stdlib__lexing____position
  and deserialize_TypeMapSerde__Config____custom:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__custom, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_args => {
          let inner = attr_name => {
            let inner = attr_path => {
              let inner = attr_module_ =>
                Belt.Result.Ok({
                  module_: attr_module_,
                  path: attr_path,
                  name: attr_name,
                  args: attr_args,
                });
              switch (Belt.List.getAssoc(items, "module", (==))) {
              | None => Belt.Result.Error(["No attribute module"])
              | Some(json) =>
                switch (
                  (
                    string =>
                      switch (string) {
                      | Json.String(string) => Belt.Result.Ok(string)
                      | _ => Error(["epected a string"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute module", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "path", (==))) {
            | None => Belt.Result.Error(["No attribute path"])
            | Some(json) =>
              switch (
                (
                  list =>
                    switch (list) {
                    | Json.Array(items) =>
                      let transformer = string =>
                        switch (string) {
                        | Json.String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
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
                Belt.Result.Error(["attribute path", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "name", (==))) {
          | None => Belt.Result.Error(["No attribute name"])
          | Some(json) =>
            switch (
              (
                string =>
                  switch (string) {
                  | Json.String(string) => Belt.Result.Ok(string)
                  | _ => Error(["epected a string"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute name", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "args", (==))) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | Json.Null => Belt.Result.Ok(None)
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
                | Json.Number(number) =>
                  Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute args", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____engine:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__engine, list(string)) = Version1.deserialize_TypeMapSerde__Config____engine
  and deserialize_TypeMapSerde__Config____engineConfig:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__engineConfig, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_helpers => {
          let inner = attr_output =>
            Belt.Result.Ok({output: attr_output, helpers: attr_helpers});
          switch (Belt.List.getAssoc(items, "output", (==))) {
          | None => Belt.Result.Error(["No attribute output"])
          | Some(json) =>
            switch (
              (
                string =>
                  switch (string) {
                  | Json.String(string) => Belt.Result.Ok(string)
                  | _ => Error(["epected a string"])
                  }
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute output", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "helpers", (==))) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | Json.Null => Belt.Result.Ok(None)
                  | _ =>
                    switch (transformer(option)) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["optional value", ...error])
                    | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                    }
                  }
              )(
                string =>
                switch (string) {
                | Json.String(string) => Belt.Result.Ok(string)
                | _ => Error(["epected a string"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute helpers", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____engines:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__engines, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_bs_json => {
          let inner = attr_rex_json =>
            Belt.Result.Ok({rex_json: attr_rex_json, bs_json: attr_bs_json});
          switch (Belt.List.getAssoc(items, "rex-json", (==))) {
          | None => inner(None)
          | Some(json) =>
            switch (
              (
                (
                  (transformer, option) =>
                    switch (option) {
                    | Json.Null => Belt.Result.Ok(None)
                    | _ =>
                      switch (transformer(option)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error(["optional value", ...error])
                      | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                      }
                    }
                )(
                  deserialize_TypeMapSerde__Config____engineConfig,
                )
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute rex-json", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "Js.Json", (==))) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | Json.Null => Belt.Result.Ok(None)
                  | _ =>
                    switch (transformer(option)) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["optional value", ...error])
                    | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                    }
                  }
              )(
                deserialize_TypeMapSerde__Config____engineConfig,
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute Js.Json", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____entry:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__entry, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_minVersion => {
          let inner = attr_history => {
            let inner = attr_publicName => {
              let inner = attr_engines => {
                let inner = attr_type_ => {
                  let inner = attr_file =>
                    Belt.Result.Ok({
                      file: attr_file,
                      type_: attr_type_,
                      engines: attr_engines,
                      publicName: attr_publicName,
                      history: attr_history,
                      minVersion: attr_minVersion,
                    });
                  switch (Belt.List.getAssoc(items, "file", (==))) {
                  | None => Belt.Result.Error(["No attribute file"])
                  | Some(json) =>
                    switch (
                      (
                        string =>
                          switch (string) {
                          | Json.String(string) => Belt.Result.Ok(string)
                          | _ => Error(["epected a string"])
                          }
                      )(
                        json,
                      )
                    ) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["attribute file", ...error])
                    | Belt.Result.Ok(data) => inner(data)
                    }
                  };
                };
                switch (Belt.List.getAssoc(items, "type", (==))) {
                | None => Belt.Result.Error(["No attribute type"])
                | Some(json) =>
                  switch (
                    (
                      string =>
                        switch (string) {
                        | Json.String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
                        }
                    )(
                      json,
                    )
                  ) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute type", ...error])
                  | Belt.Result.Ok(data) => inner(data)
                  }
                };
              };
              switch (Belt.List.getAssoc(items, "engines", (==))) {
              | None => inner(None)
              | Some(json) =>
                switch (
                  (
                    (
                      (transformer, option) =>
                        switch (option) {
                        | Json.Null => Belt.Result.Ok(None)
                        | _ =>
                          switch (transformer(option)) {
                          | Belt.Result.Error(error) =>
                            Belt.Result.Error(["optional value", ...error])
                          | Belt.Result.Ok(value) =>
                            Belt.Result.Ok(Some(value))
                          }
                        }
                    )(
                      list =>
                      switch (list) {
                      | Json.Array(items) =>
                        let transformer = deserialize_TypeMapSerde__Config____engine;
                        let rec loop = (collected, items) =>
                          switch (items) {
                          | [] =>
                            Belt.Result.Ok(Belt.List.reverse(collected))
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
                    )
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute engines", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "publicName", (==))) {
            | None => inner(None)
            | Some(json) =>
              switch (
                (
                  (
                    (transformer, option) =>
                      switch (option) {
                      | Json.Null => Belt.Result.Ok(None)
                      | _ =>
                        switch (transformer(option)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(["optional value", ...error])
                        | Belt.Result.Ok(value) =>
                          Belt.Result.Ok(Some(value))
                        }
                      }
                  )(
                    string =>
                    switch (string) {
                    | Json.String(string) => Belt.Result.Ok(string)
                    | _ => Error(["epected a string"])
                    }
                  )
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute publicName", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "history", (==))) {
          | None => inner(None)
          | Some(json) =>
            switch (
              (
                (
                  (transformer, option) =>
                    switch (option) {
                    | Json.Null => Belt.Result.Ok(None)
                    | _ =>
                      switch (transformer(option)) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error(["optional value", ...error])
                      | Belt.Result.Ok(value) => Belt.Result.Ok(Some(value))
                      }
                    }
                )(
                  bool =>
                  switch (bool) {
                  | Json.True => Belt.Result.Ok(true)
                  | Json.False => Belt.Result.Ok(false)
                  | _ => Belt.Result.Error(["Expected a bool"])
                  }
                )
              )(
                json,
              )
            ) {
            | Belt.Result.Error(error) =>
              Belt.Result.Error(["attribute history", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "minVersion", (==))) {
        | None => inner(None)
        | Some(json) =>
          switch (
            (
              (
                (transformer, option) =>
                  switch (option) {
                  | Json.Null => Belt.Result.Ok(None)
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
                | Json.Number(number) =>
                  Belt.Result.Ok(int_of_float(number))
                | _ => Error(["Expected a float"])
                }
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute minVersion", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config____serializableLockfile:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__serializableLockfile, list(string)) =
    value =>
      (
        deserialize_TypeMapSerde__Config__Locked__lockfile(
          deserialize_TypeMap__DigTypes____shortReference,
        )
      )(
        value,
      )
  and deserialize_TypeMapSerde__Config____simpleDeclaration:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__simpleDeclaration, list(string)) = Version1.deserialize_TypeMapSerde__Config____simpleDeclaration
  and deserialize_TypeMapSerde__Config____simpleExpr:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__simpleExpr, list(string)) = Version1.deserialize_TypeMapSerde__Config____simpleExpr
  and deserialize_TypeMapSerde__Config____t:
    Json.t => Belt.Result.t(_TypeMapSerde__Config__t, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_custom => {
          let inner = attr_entries => {
            let inner = attr_globalEngines => {
              let inner = attr_engines => {
                let inner = attr_outputTypes => {
                  let inner = attr_minVersion => {
                    let inner = attr_version =>
                      Belt.Result.Ok({
                        version: attr_version,
                        minVersion: attr_minVersion,
                        outputTypes: attr_outputTypes,
                        engines: attr_engines,
                        globalEngines: attr_globalEngines,
                        entries: attr_entries,
                        custom: attr_custom,
                      });
                    switch (Belt.List.getAssoc(items, "version", (==))) {
                    | None => Belt.Result.Error(["No attribute version"])
                    | Some(json) =>
                      switch (
                        (
                          number =>
                            switch (number) {
                            | Json.Number(number) =>
                              Belt.Result.Ok(int_of_float(number))
                            | _ => Error(["Expected a float"])
                            }
                        )(
                          json,
                        )
                      ) {
                      | Belt.Result.Error(error) =>
                        Belt.Result.Error(["attribute version", ...error])
                      | Belt.Result.Ok(data) => inner(data)
                      }
                    };
                  };
                  switch (Belt.List.getAssoc(items, "minVersion", (==))) {
                  | None => inner(None)
                  | Some(json) =>
                    switch (
                      (
                        (
                          (transformer, option) =>
                            switch (option) {
                            | Json.Null => Belt.Result.Ok(None)
                            | _ =>
                              switch (transformer(option)) {
                              | Belt.Result.Error(error) =>
                                Belt.Result.Error([
                                  "optional value",
                                  ...error,
                                ])
                              | Belt.Result.Ok(value) =>
                                Belt.Result.Ok(Some(value))
                              }
                            }
                        )(
                          number =>
                          switch (number) {
                          | Json.Number(number) =>
                            Belt.Result.Ok(int_of_float(number))
                          | _ => Error(["Expected a float"])
                          }
                        )
                      )(
                        json,
                      )
                    ) {
                    | Belt.Result.Error(error) =>
                      Belt.Result.Error(["attribute minVersion", ...error])
                    | Belt.Result.Ok(data) => inner(data)
                    }
                  };
                };
                switch (Belt.List.getAssoc(items, "outputTypes", (==))) {
                | None => inner(None)
                | Some(json) =>
                  switch (
                    (
                      (
                        (transformer, option) =>
                          switch (option) {
                          | Json.Null => Belt.Result.Ok(None)
                          | _ =>
                            switch (transformer(option)) {
                            | Belt.Result.Error(error) =>
                              Belt.Result.Error(["optional value", ...error])
                            | Belt.Result.Ok(value) =>
                              Belt.Result.Ok(Some(value))
                            }
                          }
                      )(
                        string =>
                        switch (string) {
                        | Json.String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
                        }
                      )
                    )(
                      json,
                    )
                  ) {
                  | Belt.Result.Error(error) =>
                    Belt.Result.Error(["attribute outputTypes", ...error])
                  | Belt.Result.Ok(data) => inner(data)
                  }
                };
              };
              switch (Belt.List.getAssoc(items, "engines", (==))) {
              | None => Belt.Result.Error(["No attribute engines"])
              | Some(json) =>
                switch (deserialize_TypeMapSerde__Config____engines(json)) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute engines", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "globalEngines", (==))) {
            | None => inner(None)
            | Some(json) =>
              switch (
                (
                  (
                    (transformer, option) =>
                      switch (option) {
                      | Json.Null => Belt.Result.Ok(None)
                      | _ =>
                        switch (transformer(option)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(["optional value", ...error])
                        | Belt.Result.Ok(value) =>
                          Belt.Result.Ok(Some(value))
                        }
                      }
                  )(
                    list =>
                    switch (list) {
                    | Json.Array(items) =>
                      let transformer = deserialize_TypeMapSerde__Config____engine;
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
                  )
                )(
                  json,
                )
              ) {
              | Belt.Result.Error(error) =>
                Belt.Result.Error(["attribute globalEngines", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "entries", (==))) {
          | None => Belt.Result.Error(["No attribute entries"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | Json.Array(items) =>
                    let transformer = deserialize_TypeMapSerde__Config____entry;
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
              Belt.Result.Error(["attribute entries", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "custom", (==))) {
        | None => Belt.Result.Error(["No attribute custom"])
        | Some(json) =>
          switch (
            (
              list =>
                switch (list) {
                | Json.Array(items) =>
                  let transformer = deserialize_TypeMapSerde__Config____custom;
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
            Belt.Result.Error(["attribute custom", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config__Locked__lockedConfig:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(
        _TypeMapSerde__Config__Locked__lockedConfig(arg0),
        list(string),
      ) =
    (referenceTransformer, record) =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_typeMap => {
          let inner = attr_entries =>
            Belt.Result.Ok({entries: attr_entries, typeMap: attr_typeMap});
          switch (Belt.List.getAssoc(items, "entries", (==))) {
          | None => Belt.Result.Error(["No attribute entries"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | Json.Array(items) =>
                    let transformer = deserialize_TypeMapSerde__Config__Locked__lockedEntry;
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
              Belt.Result.Error(["attribute entries", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "typeMap", (==))) {
        | None => Belt.Result.Error(["No attribute typeMap"])
        | Some(json) =>
          switch (
            (deserialize_TypeMap__DigTypes____typeMap(referenceTransformer))(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute typeMap", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config__Locked__lockedEntry:
    Json.t =>
    Belt.Result.t(_TypeMapSerde__Config__Locked__lockedEntry, list(string)) =
    record =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_name => {
          let inner = attr_engines => {
            let inner = attr_modulePath => {
              let inner = attr_moduleName =>
                Belt.Result.Ok({
                  moduleName: attr_moduleName,
                  modulePath: attr_modulePath,
                  engines: attr_engines,
                  name: attr_name,
                });
              switch (Belt.List.getAssoc(items, "moduleName", (==))) {
              | None => Belt.Result.Error(["No attribute moduleName"])
              | Some(json) =>
                switch (
                  (
                    string =>
                      switch (string) {
                      | Json.String(string) => Belt.Result.Ok(string)
                      | _ => Error(["epected a string"])
                      }
                  )(
                    json,
                  )
                ) {
                | Belt.Result.Error(error) =>
                  Belt.Result.Error(["attribute moduleName", ...error])
                | Belt.Result.Ok(data) => inner(data)
                }
              };
            };
            switch (Belt.List.getAssoc(items, "modulePath", (==))) {
            | None => Belt.Result.Error(["No attribute modulePath"])
            | Some(json) =>
              switch (
                (
                  list =>
                    switch (list) {
                    | Json.Array(items) =>
                      let transformer = string =>
                        switch (string) {
                        | Json.String(string) => Belt.Result.Ok(string)
                        | _ => Error(["epected a string"])
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
                Belt.Result.Error(["attribute modulePath", ...error])
              | Belt.Result.Ok(data) => inner(data)
              }
            };
          };
          switch (Belt.List.getAssoc(items, "engines", (==))) {
          | None => Belt.Result.Error(["No attribute engines"])
          | Some(json) =>
            switch (
              (
                list =>
                  switch (list) {
                  | Json.Array(items) =>
                    let transformer = json =>
                      switch (json) {
                      | Json.Array([arg0, arg1]) =>
                        switch (
                          (
                            number =>
                              switch (number) {
                              | Json.Number(number) =>
                                Belt.Result.Ok(int_of_float(number))
                              | _ => Error(["Expected a float"])
                              }
                          )(
                            arg1,
                          )
                        ) {
                        | Belt.Result.Ok(arg1) =>
                          switch (
                            deserialize_TypeMapSerde__Config____engine(arg0)
                          ) {
                          | Belt.Result.Ok(arg0) =>
                            [@implicit_arity] Belt.Result.Ok(arg0, arg1)
                          | Error(error) =>
                            Error(["tuple element 0", ...error])
                          }
                        | Error(error) =>
                          Error(["tuple element 1", ...error])
                        }
                      | _ => Belt.Result.Error(["Expected array"])
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
              Belt.Result.Error(["attribute engines", ...error])
            | Belt.Result.Ok(data) => inner(data)
            }
          };
        };
        switch (Belt.List.getAssoc(items, "name", (==))) {
        | None => Belt.Result.Error(["No attribute name"])
        | Some(json) =>
          switch (
            (
              string =>
                switch (string) {
                | Json.String(string) => Belt.Result.Ok(string)
                | _ => Error(["epected a string"])
                }
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute name", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMapSerde__Config__Locked__lockfile:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(
        _TypeMapSerde__Config__Locked__lockfile(arg0),
        list(string),
      ) =
    (referenceTransformer, record) =>
      switch (record) {
      | Json.Object(items) =>
        let inner = attr_versions =>
          Belt.Result.Ok({versions: attr_versions});
        switch (Belt.List.getAssoc(items, "versions", (==))) {
        | None => Belt.Result.Error(["No attribute versions"])
        | Some(json) =>
          switch (
            (
              (
                (transformer, array) =>
                  switch (array) {
                  | Json.Array(items) =>
                    let rec loop = (collected, items) =>
                      switch (items) {
                      | [] => Belt.Result.Ok(Belt.List.reverse(collected))
                      | [one, ...rest] =>
                        switch (transformer(one)) {
                        | Belt.Result.Error(error) =>
                          Belt.Result.Error(["array element", ...error])
                        | Belt.Result.Ok(value) =>
                          loop([value, ...collected], rest)
                        }
                      };
                    switch (loop([], items)) {
                    | Belt.Result.Error(error) => Belt.Result.Error(error)
                    | Belt.Result.Ok(value) =>
                      Belt.Result.Ok(Belt.List.toArray(value))
                    };
                  | _ => Belt.Result.Error(["expected an array"])
                  }
              )(
                deserialize_TypeMapSerde__Config__Locked__lockedConfig(
                  referenceTransformer,
                ),
              )
            )(
              json,
            )
          ) {
          | Belt.Result.Error(error) =>
            Belt.Result.Error(["attribute versions", ...error])
          | Belt.Result.Ok(data) => inner(data)
          }
        };
      | _ => Belt.Result.Error(["Expected an object"])
      }
  and deserialize_TypeMap__DigTypes____shortReference:
    Json.t => Belt.Result.t(_TypeMap__DigTypes__shortReference, list(string)) = Version1.deserialize_TypeMap__DigTypes____shortReference
  and deserialize_TypeMap__DigTypes____typeMap:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_TypeMap__DigTypes__typeMap(arg0), list(string)) = Version1.deserialize_TypeMap__DigTypes____typeMap
  and deserialize_TypeMap__DigTypes____typeSource:
    type arg0.
      (Json.t => Belt.Result.t(arg0, list(string)), Json.t) =>
      Belt.Result.t(_TypeMap__DigTypes__typeSource(arg0), list(string)) = Version1.deserialize_TypeMap__DigTypes____typeSource
  and serialize_Analyze__TopTypes____moduleName:
    _Analyze__TopTypes__moduleName => Json.t =
    value => (s => Json.String(s))(value)
  and serialize_Asttypes____loc:
    'arg0.
    ('arg0 => Json.t, _Asttypes__loc('arg0)) => Json.t
   =
    (aTransformer, record) =>
      Json.Object([
        ("txt", aTransformer(record.txt)),
        ("loc", serialize_Location____t(record.loc)),
      ])
  and serialize_Location____t: _Location__t => Json.t =
    record =>
      Json.Object([
        (
          "loc_start",
          serialize_Stdlib__lexing____position(record.loc_start),
        ),
        ("loc_end", serialize_Stdlib__lexing____position(record.loc_end)),
        ("loc_ghost", (b => b ? Json.True : Json.False)(record.loc_ghost)),
      ])
  and serialize_Parsetree____attribute: _Parsetree__attribute => Json.t =
    value =>
      (
        ((arg0, arg1)) =>
          Json.Array([
            (serialize_Asttypes____loc(s => Json.String(s)))(arg0),
            serialize_Parsetree____payload(arg1),
          ])
      )(
        value,
      )
  and serialize_Parsetree____attributes: _Parsetree__attributes => Json.t =
    value =>
      (
        list =>
          Json.Array(Belt.List.map(list, serialize_Parsetree____attribute))
      )(
        value,
      )
  and serialize_Parsetree____core_type: _Parsetree__core_type => Json.t = TransformHelpers.serialize_Parsetree____core_type
  and serialize_Parsetree____expression: _Parsetree__expression => Json.t = TransformHelpers.serialize_Parsetree____expression
  and serialize_Parsetree____pattern: _Parsetree__pattern => Json.t = TransformHelpers.serialize_Parsetree____pattern
  and serialize_Parsetree____payload: _Parsetree__payload => Json.t =
    constructor =>
      switch (constructor) {
      | PStr(arg0) =>
        Json.Array([
          Json.String("PStr"),
          serialize_Parsetree____structure(arg0),
        ])
      | PSig(arg0) =>
        Json.Array([
          Json.String("PSig"),
          serialize_Parsetree____signature(arg0),
        ])
      | PTyp(arg0) =>
        Json.Array([
          Json.String("PTyp"),
          serialize_Parsetree____core_type(arg0),
        ])
      | [@implicit_arity] PPat(arg0, arg1) =>
        Json.Array([
          Json.String("PPat"),
          serialize_Parsetree____pattern(arg0),
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              serialize_Parsetree____expression,
            )
          )(
            arg1,
          ),
        ])
      }
  and serialize_Parsetree____signature: _Parsetree__signature => Json.t = TransformHelpers.serialize_Parsetree____signature
  and serialize_Parsetree____structure: _Parsetree__structure => Json.t = TransformHelpers.serialize_Parsetree____structure
  and serialize_SharedTypes__SimpleType__body:
    'arg0.
    ('arg0 => Json.t, _SharedTypes__SimpleType__body('arg0)) => Json.t
   =
    (sourceTransformer, constructor) =>
      switch (constructor) {
      | Open => Json.Array([Json.String("Open")])
      | Abstract => Json.Array([Json.String("Abstract")])
      | Expr(arg0) =>
        Json.Array([
          Json.String("Expr"),
          (serialize_SharedTypes__SimpleType__expr(sourceTransformer))(arg0),
        ])
      | Record(arg0) =>
        Json.Array([
          Json.String("Record"),
          (
            list =>
              Json.Array(
                Belt.List.map(list, ((arg0, arg1)) =>
                  Json.Array([
                    (s => Json.String(s))(arg0),
                    (
                      serialize_SharedTypes__SimpleType__expr(
                        sourceTransformer,
                      )
                    )(
                      arg1,
                    ),
                  ])
                ),
              )
          )(
            arg0,
          ),
        ])
      | Variant(arg0) =>
        Json.Array([
          Json.String("Variant"),
          (
            list =>
              Json.Array(
                Belt.List.map(list, ((arg0, arg1, arg2)) =>
                  Json.Array([
                    (s => Json.String(s))(arg0),
                    (
                      list =>
                        Json.Array(
                          Belt.List.map(
                            list,
                            serialize_SharedTypes__SimpleType__expr(
                              sourceTransformer,
                            ),
                          ),
                        )
                    )(
                      arg1,
                    ),
                    (
                      (
                        transformer =>
                          fun
                          | None => Json.Null
                          | Some(v) => transformer(v)
                      )(
                        serialize_SharedTypes__SimpleType__expr(
                          sourceTransformer,
                        ),
                      )
                    )(
                      arg2,
                    ),
                  ])
                ),
              )
          )(
            arg0,
          ),
        ])
      }
  and serialize_SharedTypes__SimpleType__declaration:
    'arg0.
    ('arg0 => Json.t, _SharedTypes__SimpleType__declaration('arg0)) => Json.t
   =
    (sourceTransformer, record) =>
      Json.Object([
        ("name", (s => Json.String(s))(record.name)),
        (
          "variables",
          (
            list =>
              Json.Array(
                Belt.List.map(
                  list,
                  serialize_SharedTypes__SimpleType__expr(sourceTransformer),
                ),
              )
          )(
            record.variables,
          ),
        ),
        (
          "body",
          (serialize_SharedTypes__SimpleType__body(sourceTransformer))(
            record.body,
          ),
        ),
      ])
  and serialize_SharedTypes__SimpleType__expr:
    'arg0.
    ('arg0 => Json.t, _SharedTypes__SimpleType__expr('arg0)) => Json.t
   =
    (sourceTransformer, constructor) =>
      switch (constructor) {
      | Variable(arg0) =>
        Json.Array([Json.String("Variable"), (s => Json.String(s))(arg0)])
      | AnonVariable => Json.Array([Json.String("AnonVariable")])
      | [@implicit_arity] RowVariant(arg0, arg1) =>
        Json.Array([
          Json.String("RowVariant"),
          (
            list =>
              Json.Array(
                Belt.List.map(list, ((arg0, arg1)) =>
                  Json.Array([
                    (s => Json.String(s))(arg0),
                    (
                      (
                        transformer =>
                          fun
                          | None => Json.Null
                          | Some(v) => transformer(v)
                      )(
                        serialize_SharedTypes__SimpleType__expr(
                          sourceTransformer,
                        ),
                      )
                    )(
                      arg1,
                    ),
                  ])
                ),
              )
          )(
            arg0,
          ),
          (b => b ? Json.True : Json.False)(arg1),
        ])
      | [@implicit_arity] Reference(arg0, arg1) =>
        Json.Array([
          Json.String("Reference"),
          sourceTransformer(arg0),
          (
            list =>
              Json.Array(
                Belt.List.map(
                  list,
                  serialize_SharedTypes__SimpleType__expr(sourceTransformer),
                ),
              )
          )(
            arg1,
          ),
        ])
      | Tuple(arg0) =>
        Json.Array([
          Json.String("Tuple"),
          (
            list =>
              Json.Array(
                Belt.List.map(
                  list,
                  serialize_SharedTypes__SimpleType__expr(sourceTransformer),
                ),
              )
          )(
            arg0,
          ),
        ])
      | [@implicit_arity] Fn(arg0, arg1) =>
        Json.Array([
          Json.String("Fn"),
          (
            list =>
              Json.Array(
                Belt.List.map(list, ((arg0, arg1)) =>
                  Json.Array([
                    (
                      (
                        transformer =>
                          fun
                          | None => Json.Null
                          | Some(v) => transformer(v)
                      )(
                        s =>
                        Json.String(s)
                      )
                    )(
                      arg0,
                    ),
                    (
                      serialize_SharedTypes__SimpleType__expr(
                        sourceTransformer,
                      )
                    )(
                      arg1,
                    ),
                  ])
                ),
              )
          )(
            arg0,
          ),
          (serialize_SharedTypes__SimpleType__expr(sourceTransformer))(arg1),
        ])
      | Other => Json.Array([Json.String("Other")])
      }
  and serialize_Stdlib__hashtbl____t:
    'arg0 'arg1.
    ('arg0 => Json.t, 'arg1 => Json.t, _Stdlib__hashtbl__t('arg0, 'arg1)) =>
    Json.t
   =
    (aTransformer, bTransformer) =>
      TransformHelpers.serialize_Stdlib__hashtbl____t(
        aTransformer,
        bTransformer,
      )
  and serialize_Stdlib__lexing____position: _Stdlib__lexing__position => Json.t =
    record =>
      Json.Object([
        ("pos_fname", (s => Json.String(s))(record.pos_fname)),
        ("pos_lnum", (i => Json.Number(float_of_int(i)))(record.pos_lnum)),
        ("pos_bol", (i => Json.Number(float_of_int(i)))(record.pos_bol)),
        ("pos_cnum", (i => Json.Number(float_of_int(i)))(record.pos_cnum)),
      ])
  and serialize_TypeMapSerde__Config____custom:
    _TypeMapSerde__Config__custom => Json.t =
    record =>
      Json.Object([
        ("module", (s => Json.String(s))(record.module_)),
        (
          "path",
          (list => Json.Array(Belt.List.map(list, s => Json.String(s))))(
            record.path,
          ),
        ),
        ("name", (s => Json.String(s))(record.name)),
        (
          "args",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              i =>
              Json.Number(float_of_int(i))
            )
          )(
            record.args,
          ),
        ),
      ])
  and serialize_TypeMapSerde__Config____engine:
    _TypeMapSerde__Config__engine => Json.t =
    constructor =>
      switch (constructor) {
      | Rex_json => Json.Array([Json.String("rex-json")])
      | Bs_json => Json.Array([Json.String("Js.Json")])
      }
  and serialize_TypeMapSerde__Config____engineConfig:
    _TypeMapSerde__Config__engineConfig => Json.t =
    record =>
      Json.Object([
        ("output", (s => Json.String(s))(record.output)),
        (
          "helpers",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              s =>
              Json.String(s)
            )
          )(
            record.helpers,
          ),
        ),
      ])
  and serialize_TypeMapSerde__Config____engines:
    _TypeMapSerde__Config__engines => Json.t =
    record =>
      Json.Object([
        (
          "rex-json",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              serialize_TypeMapSerde__Config____engineConfig,
            )
          )(
            record.rex_json,
          ),
        ),
        (
          "Js.Json",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              serialize_TypeMapSerde__Config____engineConfig,
            )
          )(
            record.bs_json,
          ),
        ),
      ])
  and serialize_TypeMapSerde__Config____entry:
    _TypeMapSerde__Config__entry => Json.t =
    record =>
      Json.Object([
        ("file", (s => Json.String(s))(record.file)),
        ("type", (s => Json.String(s))(record.type_)),
        (
          "engines",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              list =>
              Json.Array(
                Belt.List.map(list, serialize_TypeMapSerde__Config____engine),
              )
            )
          )(
            record.engines,
          ),
        ),
        (
          "publicName",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              s =>
              Json.String(s)
            )
          )(
            record.publicName,
          ),
        ),
        (
          "history",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              b =>
              b ? Json.True : Json.False
            )
          )(
            record.history,
          ),
        ),
        (
          "minVersion",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              i =>
              Json.Number(float_of_int(i))
            )
          )(
            record.minVersion,
          ),
        ),
      ])
  and serialize_TypeMapSerde__Config____serializableLockfile:
    _TypeMapSerde__Config__serializableLockfile => Json.t =
    value =>
      (
        serialize_TypeMapSerde__Config__Locked__lockfile(
          serialize_TypeMap__DigTypes____shortReference,
        )
      )(
        value,
      )
  and serialize_TypeMapSerde__Config____simpleDeclaration:
    _TypeMapSerde__Config__simpleDeclaration => Json.t =
    value =>
      (
        serialize_SharedTypes__SimpleType__declaration(
          serialize_TypeMap__DigTypes____typeSource(
            serialize_TypeMap__DigTypes____shortReference,
          ),
        )
      )(
        value,
      )
  and serialize_TypeMapSerde__Config____simpleExpr:
    _TypeMapSerde__Config__simpleExpr => Json.t =
    value =>
      (
        serialize_SharedTypes__SimpleType__expr(
          serialize_TypeMap__DigTypes____typeSource(
            serialize_TypeMap__DigTypes____shortReference,
          ),
        )
      )(
        value,
      )
  and serialize_TypeMapSerde__Config____t: _TypeMapSerde__Config__t => Json.t =
    record =>
      Json.Object([
        ("version", (i => Json.Number(float_of_int(i)))(record.version)),
        (
          "minVersion",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              i =>
              Json.Number(float_of_int(i))
            )
          )(
            record.minVersion,
          ),
        ),
        (
          "outputTypes",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              s =>
              Json.String(s)
            )
          )(
            record.outputTypes,
          ),
        ),
        (
          "engines",
          serialize_TypeMapSerde__Config____engines(record.engines),
        ),
        (
          "globalEngines",
          (
            (
              transformer =>
                fun
                | None => Json.Null
                | Some(v) => transformer(v)
            )(
              list =>
              Json.Array(
                Belt.List.map(list, serialize_TypeMapSerde__Config____engine),
              )
            )
          )(
            record.globalEngines,
          ),
        ),
        (
          "entries",
          (
            list =>
              Json.Array(
                Belt.List.map(list, serialize_TypeMapSerde__Config____entry),
              )
          )(
            record.entries,
          ),
        ),
        (
          "custom",
          (
            list =>
              Json.Array(
                Belt.List.map(list, serialize_TypeMapSerde__Config____custom),
              )
          )(
            record.custom,
          ),
        ),
      ])
  and serialize_TypeMapSerde__Config__Locked__lockedConfig:
    'arg0.
    ('arg0 => Json.t, _TypeMapSerde__Config__Locked__lockedConfig('arg0)) =>
    Json.t
   =
    (referenceTransformer, record) =>
      Json.Object([
        (
          "entries",
          (
            list =>
              Json.Array(
                Belt.List.map(
                  list,
                  serialize_TypeMapSerde__Config__Locked__lockedEntry,
                ),
              )
          )(
            record.entries,
          ),
        ),
        (
          "typeMap",
          (serialize_TypeMap__DigTypes____typeMap(referenceTransformer))(
            record.typeMap,
          ),
        ),
      ])
  and serialize_TypeMapSerde__Config__Locked__lockedEntry:
    _TypeMapSerde__Config__Locked__lockedEntry => Json.t =
    record =>
      Json.Object([
        ("moduleName", (s => Json.String(s))(record.moduleName)),
        (
          "modulePath",
          (list => Json.Array(Belt.List.map(list, s => Json.String(s))))(
            record.modulePath,
          ),
        ),
        (
          "engines",
          (
            list =>
              Json.Array(
                Belt.List.map(list, ((arg0, arg1)) =>
                  Json.Array([
                    serialize_TypeMapSerde__Config____engine(arg0),
                    (i => Json.Number(float_of_int(i)))(arg1),
                  ])
                ),
              )
          )(
            record.engines,
          ),
        ),
        ("name", (s => Json.String(s))(record.name)),
      ])
  and serialize_TypeMapSerde__Config__Locked__lockfile:
    'arg0.
    ('arg0 => Json.t, _TypeMapSerde__Config__Locked__lockfile('arg0)) =>
    Json.t
   =
    (referenceTransformer, record) =>
      Json.Object([
        (
          "versions",
          (
            (
              (transformer, array) =>
                Json.Array(
                  Belt.List.fromArray(Belt.Array.map(array, transformer)),
                )
            )(
              serialize_TypeMapSerde__Config__Locked__lockedConfig(
                referenceTransformer,
              ),
            )
          )(
            record.versions,
          ),
        ),
      ])
  and serialize_TypeMap__DigTypes____shortReference:
    _TypeMap__DigTypes__shortReference => Json.t =
    value =>
      (
        ((arg0, arg1, arg2)) =>
          Json.Array([
            serialize_Analyze__TopTypes____moduleName(arg0),
            (list => Json.Array(Belt.List.map(list, s => Json.String(s))))(
              arg1,
            ),
            (s => Json.String(s))(arg2),
          ])
      )(
        value,
      )
  and serialize_TypeMap__DigTypes____typeMap:
    'arg0.
    ('arg0 => Json.t, _TypeMap__DigTypes__typeMap('arg0)) => Json.t
   =
    (referenceTransformer, value) =>
      (
        serialize_Stdlib__hashtbl____t(
          serialize_TypeMap__DigTypes____shortReference, ((arg0, arg1)) =>
          Json.Array([
            serialize_Parsetree____attributes(arg0),
            (
              serialize_SharedTypes__SimpleType__declaration(
                serialize_TypeMap__DigTypes____typeSource(
                  referenceTransformer,
                ),
              )
            )(
              arg1,
            ),
          ])
        )
      )(
        value,
      )
  and serialize_TypeMap__DigTypes____typeSource:
    'arg0.
    ('arg0 => Json.t, _TypeMap__DigTypes__typeSource('arg0)) => Json.t
   =
    (referenceTransformer, constructor) =>
      switch (constructor) {
      | Builtin(arg0) =>
        Json.Array([Json.String("Builtin"), (s => Json.String(s))(arg0)])
      | Public(arg0) =>
        Json.Array([Json.String("Public"), referenceTransformer(arg0)])
      | NotFound => Json.Array([Json.String("NotFound")])
      };
  let rec migrate_Analyze__TopTypes____moduleName:
    Version1._Analyze__TopTypes__moduleName => _Analyze__TopTypes__moduleName =
    _input_data => _input_data
  and migrate_Asttypes____loc:
    'a 'a_migrated.
    ('a => 'a_migrated, Version1._Asttypes__loc('a)) =>
    _Asttypes__loc('a_migrated)
   =
    (type a, type a_migrated) => (
      (_migrator_a, _input_data) => {
        let _converted_txt = _migrator_a(_input_data.txt);
        let _converted_loc = migrate_Location____t(_input_data.loc);
        {loc: _converted_loc, txt: _converted_txt};
      }:
        (a => a_migrated, Version1._Asttypes__loc(a)) =>
        _Asttypes__loc(a_migrated)
    )
  and migrate_Location____t: Version1._Location__t => _Location__t =
    _input_data => _input_data
  and migrate_Parsetree____attribute:
    Version1._Parsetree__attribute => _Parsetree__attribute =
    _input_data => _input_data
  and migrate_Parsetree____attributes:
    Version1._Parsetree__attributes => _Parsetree__attributes =
    _input_data => _input_data
  and migrate_Parsetree____core_type:
    Version1._Parsetree__core_type => _Parsetree__core_type =
    _input_data => _input_data
  and migrate_Parsetree____expression:
    Version1._Parsetree__expression => _Parsetree__expression =
    _input_data => _input_data
  and migrate_Parsetree____pattern:
    Version1._Parsetree__pattern => _Parsetree__pattern =
    _input_data => _input_data
  and migrate_Parsetree____payload:
    Version1._Parsetree__payload => _Parsetree__payload =
    _input_data => _input_data
  and migrate_Parsetree____signature:
    Version1._Parsetree__signature => _Parsetree__signature =
    _input_data => _input_data
  and migrate_Parsetree____structure:
    Version1._Parsetree__structure => _Parsetree__structure =
    _input_data => _input_data
  and migrate_SharedTypes__SimpleType__body:
    'source 'source_migrated.
    (
      'source => 'source_migrated,
      Version1._SharedTypes__SimpleType__body('source)
    ) =>
    _SharedTypes__SimpleType__body('source_migrated)
   =
    (type source, type source_migrated) => (
      (_migrator_source, _input_data) =>
        switch (_input_data) {
        | Open => Open
        | Abstract => Abstract
        | Expr(arg0) =>
          [@implicit_arity]
          Expr(
            migrate_SharedTypes__SimpleType__expr(
              arg => _migrator_source(arg),
              arg0,
            ),
          )
        | Record(arg0) =>
          [@implicit_arity]
          Record(
            (Belt.List.map(arg0))(_item => {
              let (arg0, arg1) = _item;
              (
                arg0,
                migrate_SharedTypes__SimpleType__expr(
                  arg => _migrator_source(arg),
                  arg1,
                ),
              );
            }),
          )
        | Variant(arg0) =>
          [@implicit_arity]
          Variant(
            (Belt.List.map(arg0))(_item => {
              let (arg0, arg1, arg2) = _item;
              (
                arg0,
                (Belt.List.map(arg1))(_item =>
                  migrate_SharedTypes__SimpleType__expr(
                    arg => _migrator_source(arg),
                    _item,
                  )
                ),
                switch (arg2) {
                | None => None
                | Some(_item) =>
                  Some(
                    migrate_SharedTypes__SimpleType__expr(
                      arg => _migrator_source(arg),
                      _item,
                    ),
                  )
                },
              );
            }),
          )
        }:
        (
          source => source_migrated,
          Version1._SharedTypes__SimpleType__body(source)
        ) =>
        _SharedTypes__SimpleType__body(source_migrated)
    )
  and migrate_SharedTypes__SimpleType__declaration:
    'source 'source_migrated.
    (
      'source => 'source_migrated,
      Version1._SharedTypes__SimpleType__declaration('source)
    ) =>
    _SharedTypes__SimpleType__declaration('source_migrated)
   =
    (type source, type source_migrated) => (
      (_migrator_source, _input_data) => {
        let _converted_name = _input_data.name;
        let _converted_variables =
          (Belt.List.map(_input_data.variables))(_item =>
            migrate_SharedTypes__SimpleType__expr(
              arg => _migrator_source(arg),
              _item,
            )
          );
        let _converted_body =
          migrate_SharedTypes__SimpleType__body(
            arg => _migrator_source(arg),
            _input_data.body,
          );
        {
          body: _converted_body,
          variables: _converted_variables,
          name: _converted_name,
        };
      }:
        (
          source => source_migrated,
          Version1._SharedTypes__SimpleType__declaration(source)
        ) =>
        _SharedTypes__SimpleType__declaration(source_migrated)
    )
  and migrate_SharedTypes__SimpleType__expr:
    'source 'source_migrated.
    (
      'source => 'source_migrated,
      Version1._SharedTypes__SimpleType__expr('source)
    ) =>
    _SharedTypes__SimpleType__expr('source_migrated)
   =
    (type source, type source_migrated) => (
      (_migrator_source, _input_data) =>
        switch (_input_data) {
        | Variable(arg0) => [@implicit_arity] Variable(arg0)
        | AnonVariable => AnonVariable
        | [@implicit_arity] RowVariant(arg0, arg1) =>
          [@implicit_arity]
          RowVariant(
            (Belt.List.map(arg0))(_item => {
              let (arg0, arg1) = _item;
              (
                arg0,
                switch (arg1) {
                | None => None
                | Some(_item) =>
                  Some(
                    migrate_SharedTypes__SimpleType__expr(
                      arg => _migrator_source(arg),
                      _item,
                    ),
                  )
                },
              );
            }),
            arg1,
          )
        | [@implicit_arity] Reference(arg0, arg1) =>
          [@implicit_arity]
          Reference(
            _migrator_source(arg0),
            (Belt.List.map(arg1))(_item =>
              migrate_SharedTypes__SimpleType__expr(
                arg => _migrator_source(arg),
                _item,
              )
            ),
          )
        | Tuple(arg0) =>
          [@implicit_arity]
          Tuple(
            (Belt.List.map(arg0))(_item =>
              migrate_SharedTypes__SimpleType__expr(
                arg => _migrator_source(arg),
                _item,
              )
            ),
          )
        | [@implicit_arity] Fn(arg0, arg1) =>
          [@implicit_arity]
          Fn(
            (Belt.List.map(arg0))(_item => {
              let (arg0, arg1) = _item;
              (
                switch (arg0) {
                | None => None
                | Some(_item) => Some(_item)
                },
                migrate_SharedTypes__SimpleType__expr(
                  arg => _migrator_source(arg),
                  arg1,
                ),
              );
            }),
            migrate_SharedTypes__SimpleType__expr(
              arg => _migrator_source(arg),
              arg1,
            ),
          )
        | Other => Other
        }:
        (
          source => source_migrated,
          Version1._SharedTypes__SimpleType__expr(source)
        ) =>
        _SharedTypes__SimpleType__expr(source_migrated)
    )
  and migrate_Stdlib__hashtbl____t:
    'a 'a_migrated 'b 'b_migrated.
    (
      'b => 'b_migrated,
      'a => 'a_migrated,
      Version1._Stdlib__hashtbl__t('a, 'b)
    ) =>
    _Stdlib__hashtbl__t('a_migrated, 'b_migrated)
   =
    (type b, type b_migrated, type a, type a_migrated) => (
      (_migrator_b, _migrator_a, _input_data) =>
        TransformHelpers.migrate_Stdlib__hashtbl__t(
          _migrator_a,
          _migrator_b,
          _input_data,
        ):
        (
          b => b_migrated,
          a => a_migrated,
          Version1._Stdlib__hashtbl__t(a, b)
        ) =>
        _Stdlib__hashtbl__t(a_migrated, b_migrated)
    )
  and migrate_Stdlib__lexing____position:
    Version1._Stdlib__lexing__position => _Stdlib__lexing__position =
    _input_data => _input_data
  and migrate_TypeMapSerde__Config____custom:
    Version1._TypeMapSerde__Config__custom => _TypeMapSerde__Config__custom =
    _input_data => {
      let _converted_module_ = _input_data.module_;
      let _converted_path = (Belt.List.map(_input_data.path))(_item => _item);
      let _converted_name = _input_data.name;
      let _converted_args =
        (
          custom => custom.args == 0 ? None : Some(custom.args):
            Version1._TypeMapSerde__Config__custom => option(int)
        )(
          _input_data,
        );
      {
        args: _converted_args,
        name: _converted_name,
        path: _converted_path,
        module_: _converted_module_,
      };
    }
  and migrate_TypeMapSerde__Config____engine:
    Version1._TypeMapSerde__Config__engine => _TypeMapSerde__Config__engine =
    _input_data => _input_data
  and migrate_TypeMapSerde__Config____entry:
    Version1._TypeMapSerde__Config__entry => _TypeMapSerde__Config__entry =
    _input_data => {
      let _converted_file = _input_data.file;
      let _converted_type_ = _input_data.type_;
      let _converted_engines =
        (
          entry => None:
            Version1._TypeMapSerde__Config__entry =>
            option(list(_TypeMapSerde__Config__engine))
        )(
          _input_data,
        );
      let _converted_publicName =
        switch (_input_data.publicName) {
        | None => None
        | Some(_item) => Some(_item)
        };
      let _converted_history =
        (entry => None: Version1._TypeMapSerde__Config__entry => option(bool))(
          _input_data,
        );
      let _converted_minVersion =
        (entry => None: Version1._TypeMapSerde__Config__entry => option(int))(
          _input_data,
        );
      {
        minVersion: _converted_minVersion,
        history: _converted_history,
        publicName: _converted_publicName,
        engines: _converted_engines,
        type_: _converted_type_,
        file: _converted_file,
      };
    }
  and migrate_TypeMapSerde__Config____serializableLockfile:
    Version1._TypeMapSerde__Config__serializableLockfile =>
    _TypeMapSerde__Config__serializableLockfile =
    _input_data =>
      migrate_TypeMapSerde__Config__Locked__lockfile(
        arg => migrate_TypeMap__DigTypes____shortReference(arg),
        _input_data,
      )
  and migrate_TypeMapSerde__Config____simpleDeclaration:
    Version1._TypeMapSerde__Config__simpleDeclaration =>
    _TypeMapSerde__Config__simpleDeclaration =
    _input_data => _input_data
  and migrate_TypeMapSerde__Config____simpleExpr:
    Version1._TypeMapSerde__Config__simpleExpr =>
    _TypeMapSerde__Config__simpleExpr =
    _input_data => _input_data
  and migrate_TypeMapSerde__Config____t:
    Version1._TypeMapSerde__Config__t => _TypeMapSerde__Config__t =
    _input_data => {
      let _converted_version = _input_data.version;
      let _converted_minVersion =
        (t => None: Version1._TypeMapSerde__Config__t => option(int))(
          _input_data,
        );
      let _converted_outputTypes =
        (t => None: Version1._TypeMapSerde__Config__t => option(string))(
          _input_data,
        );
      let _converted_engines =
        (
          ({engine, output}) =>
            switch (engine) {
            | Rex_json => {
                bs_json: None,
                rex_json: Some({output, helpers: None}),
              }
            | Bs_json => {
                rex_json: None,
                bs_json: Some({output, helpers: None}),
              }
            }:
            Version1._TypeMapSerde__Config__t => _TypeMapSerde__Config__engines
        )(
          _input_data,
        );
      let _converted_globalEngines =
        (
          t => None:
            Version1._TypeMapSerde__Config__t =>
            option(list(_TypeMapSerde__Config__engine))
        )(
          _input_data,
        );
      let _converted_entries =
        (Belt.List.map(_input_data.entries))(_item =>
          migrate_TypeMapSerde__Config____entry(_item)
        );
      let _converted_custom =
        (Belt.List.map(_input_data.custom))(_item =>
          migrate_TypeMapSerde__Config____custom(_item)
        );
      {
        custom: _converted_custom,
        entries: _converted_entries,
        globalEngines: _converted_globalEngines,
        engines: _converted_engines,
        outputTypes: _converted_outputTypes,
        minVersion: _converted_minVersion,
        version: _converted_version,
      };
    }
  and migrate_TypeMapSerde__Config__Locked__lockedConfig:
    'reference 'reference_migrated.
    (
      'reference => 'reference_migrated,
      Version1._TypeMapSerde__Config__Locked__lockedConfig('reference)
    ) =>
    _TypeMapSerde__Config__Locked__lockedConfig('reference_migrated)
   =
    (type reference, type reference_migrated) => (
      (_migrator_reference, _input_data) => {
        let _converted_entries =
          (Belt.List.map(_input_data.entries))(_item =>
            migrate_TypeMapSerde__Config__Locked__lockedEntry(_item)
          );
        let _converted_typeMap =
          migrate_TypeMap__DigTypes____typeMap(
            arg => _migrator_reference(arg),
            _input_data.typeMap,
          );
        {typeMap: _converted_typeMap, entries: _converted_entries};
      }:
        (
          reference => reference_migrated,
          Version1._TypeMapSerde__Config__Locked__lockedConfig(reference)
        ) =>
        _TypeMapSerde__Config__Locked__lockedConfig(reference_migrated)
    )
  and migrate_TypeMapSerde__Config__Locked__lockedEntry:
    Version1._TypeMapSerde__Config__Locked__lockedEntry =>
    _TypeMapSerde__Config__Locked__lockedEntry =
    _input_data => {
      let _converted_moduleName = _input_data.moduleName;
      let _converted_modulePath =
        (Belt.List.map(_input_data.modulePath))(_item => _item);
      let _converted_engines =
        (
          l => []:
            Version1._TypeMapSerde__Config__Locked__lockedEntry =>
            list((_TypeMapSerde__Config__engine, int))
        )(
          _input_data,
        );
      let _converted_name = _input_data.name;
      {
        name: _converted_name,
        engines: _converted_engines,
        modulePath: _converted_modulePath,
        moduleName: _converted_moduleName,
      };
    }
  and migrate_TypeMapSerde__Config__Locked__lockfile:
    'reference 'reference_migrated.
    (
      'reference => 'reference_migrated,
      Version1._TypeMapSerde__Config__Locked__lockfile('reference)
    ) =>
    _TypeMapSerde__Config__Locked__lockfile('reference_migrated)
   =
    (type reference, type reference_migrated) => (
      (_migrator_reference, _input_data) => {
        let _converted_versions =
          (Belt.Array.map(_input_data.versions))(_item =>
            migrate_TypeMapSerde__Config__Locked__lockedConfig(
              arg => _migrator_reference(arg),
              _item,
            )
          );
        {versions: _converted_versions};
      }:
        (
          reference => reference_migrated,
          Version1._TypeMapSerde__Config__Locked__lockfile(reference)
        ) =>
        _TypeMapSerde__Config__Locked__lockfile(reference_migrated)
    )
  and migrate_TypeMap__DigTypes____shortReference:
    Version1._TypeMap__DigTypes__shortReference =>
    _TypeMap__DigTypes__shortReference =
    _input_data => _input_data
  and migrate_TypeMap__DigTypes____typeMap:
    'reference 'reference_migrated.
    (
      'reference => 'reference_migrated,
      Version1._TypeMap__DigTypes__typeMap('reference)
    ) =>
    _TypeMap__DigTypes__typeMap('reference_migrated)
   =
    (type reference, type reference_migrated) => (
      (_migrator_reference, _input_data) =>
        migrate_Stdlib__hashtbl____t(
          arg => {
            let (arg0, arg1) = arg;
            (
              migrate_Parsetree____attributes(arg0),
              migrate_SharedTypes__SimpleType__declaration(
                arg =>
                  migrate_TypeMap__DigTypes____typeSource(
                    arg => _migrator_reference(arg),
                    arg,
                  ),
                arg1,
              ),
            );
          },
          arg => migrate_TypeMap__DigTypes____shortReference(arg),
          _input_data,
        ):
        (
          reference => reference_migrated,
          Version1._TypeMap__DigTypes__typeMap(reference)
        ) =>
        _TypeMap__DigTypes__typeMap(reference_migrated)
    )
  and migrate_TypeMap__DigTypes____typeSource:
    'reference 'reference_migrated.
    (
      'reference => 'reference_migrated,
      Version1._TypeMap__DigTypes__typeSource('reference)
    ) =>
    _TypeMap__DigTypes__typeSource('reference_migrated)
   =
    (type reference, type reference_migrated) => (
      (_migrator_reference, _input_data) =>
        switch (_input_data) {
        | Builtin(arg0) => [@implicit_arity] Builtin(arg0)
        | Public(arg0) =>
          [@implicit_arity] Public(_migrator_reference(arg0))
        | NotFound => NotFound
        }:
        (
          reference => reference_migrated,
          Version1._TypeMap__DigTypes__typeSource(reference)
        ) =>
        _TypeMap__DigTypes__typeSource(reference_migrated)
    );
};
let currentVersion = 2;
module Current = Version2;
let parseVersion = json =>
  switch (json) {
  | Json.Object(items) =>
    switch ((Belt.List.getAssoc(items))("$schemaVersion", (==))) {
    | Some(Json.Number(schemaVersion)) =>
      [@implicit_arity] Belt.Result.Ok(int_of_float(schemaVersion), json)
    | Some(_) => Belt.Result.Error("Invalid schema version - expected number")
    | None => Belt.Result.Error("No $schemaVersion")
    }
  | Json.Array([Json.Number(version), payload]) =>
    [@implicit_arity] Belt.Result.Ok(int_of_float(version), payload)
  | _ => Belt.Result.Error("Not wrapped in a version")
  };
let wrapWithVersion = (version, payload) =>
  switch (payload) {
  | Json.Object(items) =>
    Json.Object([
      ("$schemaVersion", Json.Number(float_of_int(version))),
      ...items,
    ])
  | _ => Json.Array([Json.Number(float_of_int(version)), payload])
  };
let serializeSerializableLockfile = data =>
  wrapWithVersion(
    currentVersion,
    Version2.serialize_TypeMapSerde__Config____serializableLockfile(data),
  )
and deserializeSerializableLockfile = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 2 =>
      switch (
        Version2.deserialize_TypeMapSerde__Config____serializableLockfile(
          data,
        )
      ) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | 1 =>
      switch (
        Version1.deserialize_TypeMapSerde__Config____serializableLockfile(
          data,
        )
      ) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data =
          Version2.migrate_TypeMapSerde__Config____serializableLockfile(data);
        Belt.Result.Ok(data);
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
let serializeT = data =>
  wrapWithVersion(
    currentVersion,
    Version2.serialize_TypeMapSerde__Config____t(data),
  )
and deserializeT = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 2 =>
      switch (Version2.deserialize_TypeMapSerde__Config____t(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | 1 =>
      switch (Version1.deserialize_TypeMapSerde__Config____t(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version2.migrate_TypeMapSerde__Config____t(data);
        Belt.Result.Ok(data);
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
let serializeSimpleDeclaration = data =>
  wrapWithVersion(
    currentVersion,
    Version2.serialize_TypeMapSerde__Config____simpleDeclaration(data),
  )
and deserializeSimpleDeclaration = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 2 =>
      switch (
        Version2.deserialize_TypeMapSerde__Config____simpleDeclaration(data)
      ) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | 1 =>
      switch (
        Version1.deserialize_TypeMapSerde__Config____simpleDeclaration(data)
      ) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data =
          Version2.migrate_TypeMapSerde__Config____simpleDeclaration(data);
        Belt.Result.Ok(data);
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
let serializeSimpleExpr = data =>
  wrapWithVersion(
    currentVersion,
    Version2.serialize_TypeMapSerde__Config____simpleExpr(data),
  )
and deserializeSimpleExpr = data =>
  switch (parseVersion(data)) {
  | Belt.Result.Error(err) => Belt.Result.Error([err])
  | [@implicit_arity] Ok(version, data) =>
    switch (version) {
    | 2 =>
      switch (Version2.deserialize_TypeMapSerde__Config____simpleExpr(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) => Belt.Result.Ok(data)
      }
    | 1 =>
      switch (Version1.deserialize_TypeMapSerde__Config____simpleExpr(data)) {
      | Belt.Result.Error(error) => Belt.Result.Error(error)
      | Ok(data) =>
        let data = Version2.migrate_TypeMapSerde__Config____simpleExpr(data);
        Belt.Result.Ok(data);
      }
    | _ =>
      Belt.Result.Error(["Unexpected version " ++ string_of_int(version)])
    }
  };
