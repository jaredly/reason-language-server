module V1_Locked =
  struct
    type _Analyze__TopTypes__moduleName = string
    and 'arg0 _Belt__Belt_HashMapInt__t = 'arg0 Belt__Belt_HashMapInt.t
    and 'source _SharedTypes__SimpleType__body =
      'source SharedTypes.SimpleType.body =
      | Open 
      | Abstract 
      | Expr of 'source _SharedTypes__SimpleType__expr 
      | Record of (string * 'source _SharedTypes__SimpleType__expr) list 
      | Variant of (string * 'source _SharedTypes__SimpleType__expr list *
      'source _SharedTypes__SimpleType__expr option) list 
    and 'source _SharedTypes__SimpleType__declaration =
      'source SharedTypes.SimpleType.declaration =
      {
      name: string ;
      variables: 'source _SharedTypes__SimpleType__expr list ;
      body: 'source _SharedTypes__SimpleType__body }
    and 'source _SharedTypes__SimpleType__expr =
      'source SharedTypes.SimpleType.expr =
      | Variable of string 
      | AnonVariable 
      | Reference of 'source * 'source _SharedTypes__SimpleType__expr list 
      | Tuple of 'source _SharedTypes__SimpleType__expr list 
      | Fn of (string option * 'source _SharedTypes__SimpleType__expr) list *
      'source _SharedTypes__SimpleType__expr 
      | Other 
    and ('a, 'b) _Stdlib__hashtbl__t = ('a, 'b) Stdlib__hashtbl.t
    and _TypeMapSerde__Config__custom = TypeMapSerde__Config.custom =
      {
      module_: string ;
      path: string list ;
      name: string ;
      args: int }
    and _TypeMapSerde__Config__engine = TypeMapSerde__Config.engine =
      | Rex_json 
      | Bs_json 
    and _TypeMapSerde__Config__entry = TypeMapSerde__Config.entry =
      {
      file: string ;
      type_: string }
    and _TypeMapSerde__Config__t = TypeMapSerde__Config.t =
      {
      version: int ;
      output: string ;
      engine: _TypeMapSerde__Config__engine ;
      entries: _TypeMapSerde__Config__entry list ;
      custom: _TypeMapSerde__Config__custom list }
    and 'reference _TypeMap__DigTypes__lockfile =
      'reference TypeMap__DigTypes.lockfile =
      {
      version: int ;
      pastVersions:
        (int, 'reference _TypeMap__DigTypes__typeMap) _Stdlib__hashtbl__t ;
      current: 'reference _TypeMap__DigTypes__typeMap }
    and _TypeMap__DigTypes__serializableLockfile =
      _TypeMap__DigTypes__shortReference _TypeMap__DigTypes__lockfile
    and _TypeMap__DigTypes__shortReference =
      (_Analyze__TopTypes__moduleName * string list * string)
    and 'reference _TypeMap__DigTypes__typeMap =
      (_TypeMap__DigTypes__shortReference,
        'reference _TypeMap__DigTypes__typeSource
          _SharedTypes__SimpleType__declaration)
        _Stdlib__hashtbl__t
    and 'reference _TypeMap__DigTypes__typeSource =
      'reference TypeMap__DigTypes.typeSource =
      | Builtin of string 
      | Public of 'reference 
      | NotFound 
  end
module DeserializeRaw =
  struct
    let rec (deserialize_Analyze__TopTypes____moduleName :
      Json.t -> (Analyze__TopTypes.moduleName, string) Belt.Result.t) =
      fun value ->
        (fun string ->
           match string with
           | ((Json.String (string))[@explicit_arity ]) ->
               ((Belt.Result.Ok (string))[@explicit_arity ])
           | _ -> ((Error ("epected a string"))[@explicit_arity ])) value
    and deserialize_Belt__Belt_HashMapInt____t :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t -> ('arg0 Belt__Belt_HashMapInt.t, string) Belt.Result.t
      =
      fun arg0Transformer ->
        TransformHelpers.deserialize_Belt__Belt_HashMapInt____t
          arg0Transformer
    and deserialize_SharedTypes__SimpleType__body :
      type arg0 .
        (Json.t -> (arg0, string) Belt.Result.t) ->
          Json.t -> (arg0 SharedTypes.SimpleType.body, string) Belt.Result.t
      =
      fun sourceTransformer ->
        fun constructor ->
          match constructor with
          | Json.Array (tag::[]) when (Json.String "Open") = tag ->
              Belt.Result.Ok (Open : arg0 SharedTypes.SimpleType.body)
          | Json.Array (tag::[]) when (Json.String "Abstract") = tag ->
              Belt.Result.Ok (Abstract : arg0 SharedTypes.SimpleType.body)
          | Json.Array (tag::arg0::[]) when (Json.String "Expr") = tag ->
              (match (deserialize_SharedTypes__SimpleType__expr
                        sourceTransformer) arg0
               with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Expr (arg0) : arg0 SharedTypes.SimpleType.body)
               | Error error -> Error error)
          | Json.Array (tag::arg0::[]) when (Json.String "Record") = tag ->
              (match (fun list ->
                        match list with
                        | ((Json.Array (items))[@explicit_arity ]) ->
                            let transformer json =
                              match json with
                              | ((Json.Array
                                  (arg0::arg1::[]))[@explicit_arity ]) ->
                                  (match (deserialize_SharedTypes__SimpleType__expr
                                            sourceTransformer) arg1
                                   with
                                   | Belt.Result.Ok arg1 ->
                                       (match (fun string ->
                                                 match string with
                                                 | ((Json.String
                                                     (string))[@explicit_arity
                                                                ])
                                                     ->
                                                     ((Belt.Result.Ok
                                                         (string))
                                                     [@explicit_arity ])
                                                 | _ ->
                                                     ((Error
                                                         ("epected a string"))
                                                     [@explicit_arity ]))
                                                arg0
                                        with
                                        | Belt.Result.Ok arg0 ->
                                            Belt.Result.Ok (arg0, arg1)
                                        | Error error -> Error error)
                                   | Error error -> Error error)
                              | _ -> ((Belt.Result.Error ("Expected array"))
                                  [@explicit_arity ]) in
                            let rec loop items =
                              match items with
                              | [] -> ((Belt.Result.Ok ([]))
                                  [@explicit_arity ])
                              | one::rest ->
                                  (match transformer one with
                                   | ((Belt.Result.Error
                                       (error))[@explicit_arity ]) ->
                                       ((Belt.Result.Error (error))
                                       [@explicit_arity ])
                                   | ((Belt.Result.Ok
                                       (value))[@explicit_arity ]) ->
                                       (match loop rest with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (rest))[@explicit_arity ]) ->
                                            ((Belt.Result.Ok
                                                ((value :: rest)))
                                            [@explicit_arity ]))) in
                            loop items
                        | _ -> ((Belt.Result.Error ("expected an array"))
                            [@explicit_arity ])) arg0
               with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Record (arg0) : arg0 SharedTypes.SimpleType.body)
               | Error error -> Error error)
          | Json.Array (tag::arg0::[]) when (Json.String "Variant") = tag ->
              (match (fun list ->
                        match list with
                        | ((Json.Array (items))[@explicit_arity ]) ->
                            let transformer json =
                              match json with
                              | ((Json.Array
                                  (arg0::arg1::arg2::[]))[@explicit_arity ])
                                  ->
                                  (match ((fun transformer ->
                                             fun option ->
                                               match option with
                                               | Json.Null ->
                                                   ((Belt.Result.Ok (None))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   (match transformer option
                                                    with
                                                    | ((Belt.Result.Error
                                                        (error))[@explicit_arity
                                                                  ])
                                                        ->
                                                        ((Belt.Result.Error
                                                            (error))
                                                        [@explicit_arity ])
                                                    | ((Belt.Result.Ok
                                                        (value))[@explicit_arity
                                                                  ])
                                                        ->
                                                        ((Belt.Result.Ok
                                                            (((Some (value))
                                                              [@explicit_arity
                                                                ])))
                                                        [@explicit_arity ])))
                                            (deserialize_SharedTypes__SimpleType__expr
                                               sourceTransformer)) arg2
                                   with
                                   | Belt.Result.Ok arg2 ->
                                       (match (fun list ->
                                                 match list with
                                                 | ((Json.Array
                                                     (items))[@explicit_arity
                                                               ])
                                                     ->
                                                     let transformer =
                                                       deserialize_SharedTypes__SimpleType__expr
                                                         sourceTransformer in
                                                     let rec loop items =
                                                       match items with
                                                       | [] ->
                                                           ((Belt.Result.Ok
                                                               ([]))
                                                           [@explicit_arity ])
                                                       | one::rest ->
                                                           (match transformer
                                                                    one
                                                            with
                                                            | ((Belt.Result.Error
                                                                (error))
                                                                [@explicit_arity
                                                                  ])
                                                                ->
                                                                ((Belt.Result.Error
                                                                    (error))
                                                                [@explicit_arity
                                                                  ])
                                                            | ((Belt.Result.Ok
                                                                (value))
                                                                [@explicit_arity
                                                                  ])
                                                                ->
                                                                (match 
                                                                   loop rest
                                                                 with
                                                                 | ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((
                                                                    Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ])
                                                                 | ((Belt.Result.Ok
                                                                    (rest))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((
                                                                    Belt.Result.Ok
                                                                    ((value
                                                                    :: rest)))
                                                                    [@explicit_arity
                                                                    ]))) in
                                                     loop items
                                                 | _ ->
                                                     ((Belt.Result.Error
                                                         ("expected an array"))
                                                     [@explicit_arity ]))
                                                arg1
                                        with
                                        | Belt.Result.Ok arg1 ->
                                            (match (fun string ->
                                                      match string with
                                                      | ((Json.String
                                                          (string))[@explicit_arity
                                                                    ])
                                                          ->
                                                          ((Belt.Result.Ok
                                                              (string))
                                                          [@explicit_arity ])
                                                      | _ ->
                                                          ((Error
                                                              ("epected a string"))
                                                          [@explicit_arity ]))
                                                     arg0
                                             with
                                             | Belt.Result.Ok arg0 ->
                                                 Belt.Result.Ok
                                                   (arg0, arg1, arg2)
                                             | Error error -> Error error)
                                        | Error error -> Error error)
                                   | Error error -> Error error)
                              | _ -> ((Belt.Result.Error ("Expected array"))
                                  [@explicit_arity ]) in
                            let rec loop items =
                              match items with
                              | [] -> ((Belt.Result.Ok ([]))
                                  [@explicit_arity ])
                              | one::rest ->
                                  (match transformer one with
                                   | ((Belt.Result.Error
                                       (error))[@explicit_arity ]) ->
                                       ((Belt.Result.Error (error))
                                       [@explicit_arity ])
                                   | ((Belt.Result.Ok
                                       (value))[@explicit_arity ]) ->
                                       (match loop rest with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (rest))[@explicit_arity ]) ->
                                            ((Belt.Result.Ok
                                                ((value :: rest)))
                                            [@explicit_arity ]))) in
                            loop items
                        | _ -> ((Belt.Result.Error ("expected an array"))
                            [@explicit_arity ])) arg0
               with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Variant (arg0) : arg0 SharedTypes.SimpleType.body)
               | Error error -> Error error)
          | _ -> Error "Expected an array"
    and deserialize_SharedTypes__SimpleType__declaration :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t ->
            ('arg0 SharedTypes.SimpleType.declaration, string) Belt.Result.t
      =
      fun sourceTransformer ->
        fun record ->
          match record with
          | ((Json.Object (items))[@explicit_arity ]) ->
              (match Belt.List.getAssoc items "body" (=) with
               | None -> ((Belt.Result.Error (("No attribute " ^ "body")))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match (deserialize_SharedTypes__SimpleType__body
                             sourceTransformer) json
                    with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (error))[@explicit_arity ])
                    | ((Belt.Result.Ok (attr_body))[@explicit_arity ]) ->
                        (match Belt.List.getAssoc items "variables" (=) with
                         | None ->
                             ((Belt.Result.Error
                                 (("No attribute " ^ "variables")))
                             [@explicit_arity ])
                         | ((Some (json))[@explicit_arity ]) ->
                             (match (fun list ->
                                       match list with
                                       | ((Json.Array
                                           (items))[@explicit_arity ]) ->
                                           let transformer =
                                             deserialize_SharedTypes__SimpleType__expr
                                               sourceTransformer in
                                           let rec loop items =
                                             match items with
                                             | [] -> ((Belt.Result.Ok ([]))
                                                 [@explicit_arity ])
                                             | one::rest ->
                                                 (match transformer one with
                                                  | ((Belt.Result.Error
                                                      (error))[@explicit_arity
                                                                ])
                                                      ->
                                                      ((Belt.Result.Error
                                                          (error))
                                                      [@explicit_arity ])
                                                  | ((Belt.Result.Ok
                                                      (value))[@explicit_arity
                                                                ])
                                                      ->
                                                      (match loop rest with
                                                       | ((Belt.Result.Error
                                                           (error))[@explicit_arity
                                                                    ])
                                                           ->
                                                           ((Belt.Result.Error
                                                               (error))
                                                           [@explicit_arity ])
                                                       | ((Belt.Result.Ok
                                                           (rest))[@explicit_arity
                                                                    ])
                                                           ->
                                                           ((Belt.Result.Ok
                                                               ((value ::
                                                                 rest)))
                                                           [@explicit_arity ]))) in
                                           loop items
                                       | _ ->
                                           ((Belt.Result.Error
                                               ("expected an array"))
                                           [@explicit_arity ])) json
                              with
                              | ((Belt.Result.Error
                                  (error))[@explicit_arity ]) ->
                                  ((Belt.Result.Error (error))
                                  [@explicit_arity ])
                              | ((Belt.Result.Ok
                                  (attr_variables))[@explicit_arity ]) ->
                                  (match Belt.List.getAssoc items "name" (=)
                                   with
                                   | None ->
                                       ((Belt.Result.Error
                                           (("No attribute " ^ "name")))
                                       [@explicit_arity ])
                                   | ((Some (json))[@explicit_arity ]) ->
                                       (match (fun string ->
                                                 match string with
                                                 | ((Json.String
                                                     (string))[@explicit_arity
                                                                ])
                                                     ->
                                                     ((Belt.Result.Ok
                                                         (string))
                                                     [@explicit_arity ])
                                                 | _ ->
                                                     ((Error
                                                         ("epected a string"))
                                                     [@explicit_arity ]))
                                                json
                                        with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (attr_name))[@explicit_arity ])
                                            ->
                                            Belt.Result.Ok
                                              {
                                                name = attr_name;
                                                variables = attr_variables;
                                                body = attr_body
                                              }))))))
          | _ -> ((Belt.Result.Error ("Expected an object"))
              [@explicit_arity ])
    and deserialize_SharedTypes__SimpleType__expr :
      type arg0 .
        (Json.t -> (arg0, string) Belt.Result.t) ->
          Json.t -> (arg0 SharedTypes.SimpleType.expr, string) Belt.Result.t
      =
      fun sourceTransformer ->
        fun constructor ->
          match constructor with
          | Json.Array (tag::arg0::[]) when (Json.String "Variable") = tag ->
              (match (fun string ->
                        match string with
                        | ((Json.String (string))[@explicit_arity ]) ->
                            ((Belt.Result.Ok (string))[@explicit_arity ])
                        | _ -> ((Error ("epected a string"))
                            [@explicit_arity ])) arg0
               with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Variable (arg0) : arg0 SharedTypes.SimpleType.expr)
               | Error error -> Error error)
          | Json.Array (tag::[]) when (Json.String "AnonVariable") = tag ->
              Belt.Result.Ok
                (AnonVariable : arg0 SharedTypes.SimpleType.expr)
          | Json.Array (tag::arg0::arg1::[]) when
              (Json.String "Reference") = tag ->
              (match (fun list ->
                        match list with
                        | ((Json.Array (items))[@explicit_arity ]) ->
                            let transformer =
                              deserialize_SharedTypes__SimpleType__expr
                                sourceTransformer in
                            let rec loop items =
                              match items with
                              | [] -> ((Belt.Result.Ok ([]))
                                  [@explicit_arity ])
                              | one::rest ->
                                  (match transformer one with
                                   | ((Belt.Result.Error
                                       (error))[@explicit_arity ]) ->
                                       ((Belt.Result.Error (error))
                                       [@explicit_arity ])
                                   | ((Belt.Result.Ok
                                       (value))[@explicit_arity ]) ->
                                       (match loop rest with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (rest))[@explicit_arity ]) ->
                                            ((Belt.Result.Ok
                                                ((value :: rest)))
                                            [@explicit_arity ]))) in
                            loop items
                        | _ -> ((Belt.Result.Error ("expected an array"))
                            [@explicit_arity ])) arg1
               with
               | Belt.Result.Ok arg1 ->
                   (match sourceTransformer arg0 with
                    | Belt.Result.Ok arg0 ->
                        Belt.Result.Ok
                          (Reference (arg0, arg1) : arg0
                                                      SharedTypes.SimpleType.expr)
                    | Error error -> Error error)
               | Error error -> Error error)
          | Json.Array (tag::arg0::[]) when (Json.String "Tuple") = tag ->
              (match (fun list ->
                        match list with
                        | ((Json.Array (items))[@explicit_arity ]) ->
                            let transformer =
                              deserialize_SharedTypes__SimpleType__expr
                                sourceTransformer in
                            let rec loop items =
                              match items with
                              | [] -> ((Belt.Result.Ok ([]))
                                  [@explicit_arity ])
                              | one::rest ->
                                  (match transformer one with
                                   | ((Belt.Result.Error
                                       (error))[@explicit_arity ]) ->
                                       ((Belt.Result.Error (error))
                                       [@explicit_arity ])
                                   | ((Belt.Result.Ok
                                       (value))[@explicit_arity ]) ->
                                       (match loop rest with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (rest))[@explicit_arity ]) ->
                                            ((Belt.Result.Ok
                                                ((value :: rest)))
                                            [@explicit_arity ]))) in
                            loop items
                        | _ -> ((Belt.Result.Error ("expected an array"))
                            [@explicit_arity ])) arg0
               with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Tuple (arg0) : arg0 SharedTypes.SimpleType.expr)
               | Error error -> Error error)
          | Json.Array (tag::arg0::arg1::[]) when (Json.String "Fn") = tag ->
              (match (deserialize_SharedTypes__SimpleType__expr
                        sourceTransformer) arg1
               with
               | Belt.Result.Ok arg1 ->
                   (match (fun list ->
                             match list with
                             | ((Json.Array (items))[@explicit_arity ]) ->
                                 let transformer json =
                                   match json with
                                   | ((Json.Array
                                       (arg0::arg1::[]))[@explicit_arity ])
                                       ->
                                       (match (deserialize_SharedTypes__SimpleType__expr
                                                 sourceTransformer) arg1
                                        with
                                        | Belt.Result.Ok arg1 ->
                                            (match ((fun transformer ->
                                                       fun option ->
                                                         match option with
                                                         | Json.Null ->
                                                             ((Belt.Result.Ok
                                                                 (None))
                                                             [@explicit_arity
                                                               ])
                                                         | _ ->
                                                             (match transformer
                                                                    option
                                                              with
                                                              | ((Belt.Result.Error
                                                                  (error))
                                                                  [@explicit_arity
                                                                    ])
                                                                  ->
                                                                  ((Belt.Result.Error
                                                                    (error))
                                                                  [@explicit_arity
                                                                    ])
                                                              | ((Belt.Result.Ok
                                                                  (value))
                                                                  [@explicit_arity
                                                                    ])
                                                                  ->
                                                                  ((Belt.Result.Ok
                                                                    (((
                                                                    Some
                                                                    (value))
                                                                    [@explicit_arity
                                                                    ])))
                                                                  [@explicit_arity
                                                                    ])))
                                                      (fun string ->
                                                         match string with
                                                         | ((Json.String
                                                             (string))
                                                             [@explicit_arity
                                                               ])
                                                             ->
                                                             ((Belt.Result.Ok
                                                                 (string))
                                                             [@explicit_arity
                                                               ])
                                                         | _ ->
                                                             ((Error
                                                                 ("epected a string"))
                                                             [@explicit_arity
                                                               ]))) arg0
                                             with
                                             | Belt.Result.Ok arg0 ->
                                                 Belt.Result.Ok (arg0, arg1)
                                             | Error error -> Error error)
                                        | Error error -> Error error)
                                   | _ ->
                                       ((Belt.Result.Error ("Expected array"))
                                       [@explicit_arity ]) in
                                 let rec loop items =
                                   match items with
                                   | [] -> ((Belt.Result.Ok ([]))
                                       [@explicit_arity ])
                                   | one::rest ->
                                       (match transformer one with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (value))[@explicit_arity ]) ->
                                            (match loop rest with
                                             | ((Belt.Result.Error
                                                 (error))[@explicit_arity ])
                                                 ->
                                                 ((Belt.Result.Error (error))
                                                 [@explicit_arity ])
                                             | ((Belt.Result.Ok
                                                 (rest))[@explicit_arity ])
                                                 ->
                                                 ((Belt.Result.Ok
                                                     ((value :: rest)))
                                                 [@explicit_arity ]))) in
                                 loop items
                             | _ ->
                                 ((Belt.Result.Error ("expected an array"))
                                 [@explicit_arity ])) arg0
                    with
                    | Belt.Result.Ok arg0 ->
                        Belt.Result.Ok
                          (Fn (arg0, arg1) : arg0
                                               SharedTypes.SimpleType.expr)
                    | Error error -> Error error)
               | Error error -> Error error)
          | Json.Array (tag::[]) when (Json.String "Other") = tag ->
              Belt.Result.Ok (Other : arg0 SharedTypes.SimpleType.expr)
          | _ -> Error "Expected an array"
    and deserialize_Stdlib__hashtbl____t :
      'arg0 'arg1 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          (Json.t -> ('arg1, string) Belt.Result.t) ->
            Json.t ->
              (('arg0, 'arg1) Stdlib__hashtbl.t, string) Belt.Result.t
      =
      fun aTransformer ->
        fun bTransformer ->
          TransformHelpers.deserialize_Stdlib__hashtbl____t aTransformer
            bTransformer
    and (deserialize_TypeMapSerde__Config____custom :
      Json.t -> (TypeMapSerde__Config.custom, string) Belt.Result.t) =
      fun record ->
        match record with
        | ((Json.Object (items))[@explicit_arity ]) ->
            (match Belt.List.getAssoc items "args" (=) with
             | None -> ((Belt.Result.Error (("No attribute " ^ "args")))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun number ->
                           match number with
                           | ((Json.Number (number))[@explicit_arity ]) ->
                               ((Belt.Result.Ok ((int_of_float number)))
                               [@explicit_arity ])
                           | _ -> ((Error ("Expected a float"))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Belt.Result.Ok (attr_args))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "name" (=) with
                       | None ->
                           ((Belt.Result.Error (("No attribute " ^ "name")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun string ->
                                     match string with
                                     | ((Json.String
                                         (string))[@explicit_arity ]) ->
                                         ((Belt.Result.Ok (string))
                                         [@explicit_arity ])
                                     | _ -> ((Error ("epected a string"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Belt.Result.Ok
                                (attr_name))[@explicit_arity ]) ->
                                (match Belt.List.getAssoc items "path" (=)
                                 with
                                 | None ->
                                     ((Belt.Result.Error
                                         (("No attribute " ^ "path")))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun list ->
                                               match list with
                                               | ((Json.Array
                                                   (items))[@explicit_arity ])
                                                   ->
                                                   let transformer string =
                                                     match string with
                                                     | ((Json.String
                                                         (string))[@explicit_arity
                                                                    ])
                                                         ->
                                                         ((Belt.Result.Ok
                                                             (string))
                                                         [@explicit_arity ])
                                                     | _ ->
                                                         ((Error
                                                             ("epected a string"))
                                                         [@explicit_arity ]) in
                                                   let rec loop items =
                                                     match items with
                                                     | [] ->
                                                         ((Belt.Result.Ok
                                                             ([]))
                                                         [@explicit_arity ])
                                                     | one::rest ->
                                                         (match transformer
                                                                  one
                                                          with
                                                          | ((Belt.Result.Error
                                                              (error))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              ((Belt.Result.Error
                                                                  (error))
                                                              [@explicit_arity
                                                                ])
                                                          | ((Belt.Result.Ok
                                                              (value))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              (match 
                                                                 loop rest
                                                               with
                                                               | ((Belt.Result.Error
                                                                   (error))
                                                                   [@explicit_arity
                                                                    ])
                                                                   ->
                                                                   ((
                                                                   Belt.Result.Error
                                                                    (error))
                                                                   [@explicit_arity
                                                                    ])
                                                               | ((Belt.Result.Ok
                                                                   (rest))
                                                                   [@explicit_arity
                                                                    ])
                                                                   ->
                                                                   ((
                                                                   Belt.Result.Ok
                                                                    ((value
                                                                    :: rest)))
                                                                   [@explicit_arity
                                                                    ]))) in
                                                   loop items
                                               | _ ->
                                                   ((Belt.Result.Error
                                                       ("expected an array"))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (attr_path))[@explicit_arity ]) ->
                                          (match Belt.List.getAssoc items
                                                   "module_" (=)
                                           with
                                           | None ->
                                               ((Belt.Result.Error
                                                   (("No attribute " ^
                                                       "module_")))
                                               [@explicit_arity ])
                                           | ((Some
                                               (json))[@explicit_arity ]) ->
                                               (match (fun string ->
                                                         match string with
                                                         | ((Json.String
                                                             (string))
                                                             [@explicit_arity
                                                               ])
                                                             ->
                                                             ((Belt.Result.Ok
                                                                 (string))
                                                             [@explicit_arity
                                                               ])
                                                         | _ ->
                                                             ((Error
                                                                 ("epected a string"))
                                                             [@explicit_arity
                                                               ])) json
                                                with
                                                | ((Belt.Result.Error
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (error))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (attr_module_))[@explicit_arity
                                                                    ])
                                                    ->
                                                    Belt.Result.Ok
                                                      {
                                                        module_ =
                                                          attr_module_;
                                                        path = attr_path;
                                                        name = attr_name;
                                                        args = attr_args
                                                      }))))))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and (deserialize_TypeMapSerde__Config____engine :
      Json.t -> (TypeMapSerde__Config.engine, string) Belt.Result.t) =
      fun constructor ->
        match constructor with
        | Json.Array (tag::[]) when (Json.String "Rex_json") = tag ->
            Belt.Result.Ok (Rex_json : TypeMapSerde__Config.engine)
        | Json.Array (tag::[]) when (Json.String "Bs_json") = tag ->
            Belt.Result.Ok (Bs_json : TypeMapSerde__Config.engine)
        | _ -> Error "Expected an array"
    and (deserialize_TypeMapSerde__Config____entry :
      Json.t -> (TypeMapSerde__Config.entry, string) Belt.Result.t) =
      fun record ->
        match record with
        | ((Json.Object (items))[@explicit_arity ]) ->
            (match Belt.List.getAssoc items "type_" (=) with
             | None -> ((Belt.Result.Error (("No attribute " ^ "type_")))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun string ->
                           match string with
                           | ((Json.String (string))[@explicit_arity ]) ->
                               ((Belt.Result.Ok (string))[@explicit_arity ])
                           | _ -> ((Error ("epected a string"))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Belt.Result.Ok (attr_type_))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "file" (=) with
                       | None ->
                           ((Belt.Result.Error (("No attribute " ^ "file")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun string ->
                                     match string with
                                     | ((Json.String
                                         (string))[@explicit_arity ]) ->
                                         ((Belt.Result.Ok (string))
                                         [@explicit_arity ])
                                     | _ -> ((Error ("epected a string"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Belt.Result.Ok
                                (attr_file))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { file = attr_file; type_ = attr_type_ }))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and (deserialize_TypeMapSerde__Config____t :
      Json.t -> (TypeMapSerde__Config.t, string) Belt.Result.t) =
      fun record ->
        match record with
        | ((Json.Object (items))[@explicit_arity ]) ->
            (match Belt.List.getAssoc items "custom" (=) with
             | None -> ((Belt.Result.Error (("No attribute " ^ "custom")))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun list ->
                           match list with
                           | ((Json.Array (items))[@explicit_arity ]) ->
                               let transformer =
                                 deserialize_TypeMapSerde__Config____custom in
                               let rec loop items =
                                 match items with
                                 | [] -> ((Belt.Result.Ok ([]))
                                     [@explicit_arity ])
                                 | one::rest ->
                                     (match transformer one with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (value))[@explicit_arity ]) ->
                                          (match loop rest with
                                           | ((Belt.Result.Error
                                               (error))[@explicit_arity ]) ->
                                               ((Belt.Result.Error (error))
                                               [@explicit_arity ])
                                           | ((Belt.Result.Ok
                                               (rest))[@explicit_arity ]) ->
                                               ((Belt.Result.Ok
                                                   ((value :: rest)))
                                               [@explicit_arity ]))) in
                               loop items
                           | _ -> ((Belt.Result.Error ("expected an array"))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Belt.Result.Ok (attr_custom))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "entries" (=) with
                       | None ->
                           ((Belt.Result.Error
                               (("No attribute " ^ "entries")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match list with
                                     | ((Json.Array
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_TypeMapSerde__Config____entry in
                                         let rec loop items =
                                           match items with
                                           | [] -> ((Belt.Result.Ok ([]))
                                               [@explicit_arity ])
                                           | one::rest ->
                                               (match transformer one with
                                                | ((Belt.Result.Error
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (error))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop rest with
                                                     | ((Belt.Result.Error
                                                         (error))[@explicit_arity
                                                                   ])
                                                         ->
                                                         ((Belt.Result.Error
                                                             (error))
                                                         [@explicit_arity ])
                                                     | ((Belt.Result.Ok
                                                         (rest))[@explicit_arity
                                                                  ])
                                                         ->
                                                         ((Belt.Result.Ok
                                                             ((value ::
                                                               rest)))
                                                         [@explicit_arity ]))) in
                                         loop items
                                     | _ ->
                                         ((Belt.Result.Error
                                             ("expected an array"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Belt.Result.Ok
                                (attr_entries))[@explicit_arity ]) ->
                                (match Belt.List.getAssoc items "engine" (=)
                                 with
                                 | None ->
                                     ((Belt.Result.Error
                                         (("No attribute " ^ "engine")))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match deserialize_TypeMapSerde__Config____engine
                                              json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (attr_engine))[@explicit_arity ])
                                          ->
                                          (match Belt.List.getAssoc items
                                                   "output" (=)
                                           with
                                           | None ->
                                               ((Belt.Result.Error
                                                   (("No attribute " ^
                                                       "output")))
                                               [@explicit_arity ])
                                           | ((Some
                                               (json))[@explicit_arity ]) ->
                                               (match (fun string ->
                                                         match string with
                                                         | ((Json.String
                                                             (string))
                                                             [@explicit_arity
                                                               ])
                                                             ->
                                                             ((Belt.Result.Ok
                                                                 (string))
                                                             [@explicit_arity
                                                               ])
                                                         | _ ->
                                                             ((Error
                                                                 ("epected a string"))
                                                             [@explicit_arity
                                                               ])) json
                                                with
                                                | ((Belt.Result.Error
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (error))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (attr_output))[@explicit_arity
                                                                    ])
                                                    ->
                                                    (match Belt.List.getAssoc
                                                             items "version"
                                                             (=)
                                                     with
                                                     | None ->
                                                         ((Belt.Result.Error
                                                             (("No attribute "
                                                                 ^ "version")))
                                                         [@explicit_arity ])
                                                     | ((Some
                                                         (json))[@explicit_arity
                                                                  ])
                                                         ->
                                                         (match (fun number
                                                                   ->
                                                                   match number
                                                                   with
                                                                   | 
                                                                   ((Json.Number
                                                                    (number))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((
                                                                    Belt.Result.Ok
                                                                    ((int_of_float
                                                                    number)))
                                                                    [@explicit_arity
                                                                    ])
                                                                   | 
                                                                   _ ->
                                                                    ((
                                                                    Error
                                                                    ("Expected a float"))
                                                                    [@explicit_arity
                                                                    ])) json
                                                          with
                                                          | ((Belt.Result.Error
                                                              (error))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              ((Belt.Result.Error
                                                                  (error))
                                                              [@explicit_arity
                                                                ])
                                                          | ((Belt.Result.Ok
                                                              (attr_version))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              Belt.Result.Ok
                                                                {
                                                                  version =
                                                                    attr_version;
                                                                  output =
                                                                    attr_output;
                                                                  engine =
                                                                    attr_engine;
                                                                  entries =
                                                                    attr_entries;
                                                                  custom =
                                                                    attr_custom
                                                                }))))))))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and deserialize_TypeMap__DigTypes____lockfile :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t -> ('arg0 TypeMap__DigTypes.lockfile, string) Belt.Result.t
      =
      fun referenceTransformer ->
        fun record ->
          match record with
          | ((Json.Object (items))[@explicit_arity ]) ->
              (match Belt.List.getAssoc items "current" (=) with
               | None -> ((Belt.Result.Error (("No attribute " ^ "current")))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match (deserialize_TypeMap__DigTypes____typeMap
                             referenceTransformer) json
                    with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (error))[@explicit_arity ])
                    | ((Belt.Result.Ok (attr_current))[@explicit_arity ]) ->
                        (match Belt.List.getAssoc items "pastVersions" (=)
                         with
                         | None ->
                             ((Belt.Result.Error
                                 (("No attribute " ^ "pastVersions")))
                             [@explicit_arity ])
                         | ((Some (json))[@explicit_arity ]) ->
                             (match (deserialize_Stdlib__hashtbl____t
                                       (fun number ->
                                          match number with
                                          | ((Json.Number
                                              (number))[@explicit_arity ]) ->
                                              ((Belt.Result.Ok
                                                  ((int_of_float number)))
                                              [@explicit_arity ])
                                          | _ ->
                                              ((Error ("Expected a float"))
                                              [@explicit_arity ]))
                                       (deserialize_TypeMap__DigTypes____typeMap
                                          referenceTransformer)) json
                              with
                              | ((Belt.Result.Error
                                  (error))[@explicit_arity ]) ->
                                  ((Belt.Result.Error (error))
                                  [@explicit_arity ])
                              | ((Belt.Result.Ok
                                  (attr_pastVersions))[@explicit_arity ]) ->
                                  (match Belt.List.getAssoc items "version"
                                           (=)
                                   with
                                   | None ->
                                       ((Belt.Result.Error
                                           (("No attribute " ^ "version")))
                                       [@explicit_arity ])
                                   | ((Some (json))[@explicit_arity ]) ->
                                       (match (fun number ->
                                                 match number with
                                                 | ((Json.Number
                                                     (number))[@explicit_arity
                                                                ])
                                                     ->
                                                     ((Belt.Result.Ok
                                                         ((int_of_float
                                                             number)))
                                                     [@explicit_arity ])
                                                 | _ ->
                                                     ((Error
                                                         ("Expected a float"))
                                                     [@explicit_arity ]))
                                                json
                                        with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (attr_version))[@explicit_arity ])
                                            ->
                                            Belt.Result.Ok
                                              {
                                                version = attr_version;
                                                pastVersions =
                                                  attr_pastVersions;
                                                current = attr_current
                                              }))))))
          | _ -> ((Belt.Result.Error ("Expected an object"))
              [@explicit_arity ])
    and (deserialize_TypeMap__DigTypes____serializableLockfile :
      Json.t ->
        (TypeMap__DigTypes.serializableLockfile, string) Belt.Result.t)
      =
      fun value ->
        (deserialize_TypeMap__DigTypes____lockfile
           deserialize_TypeMap__DigTypes____shortReference) value
    and (deserialize_TypeMap__DigTypes____shortReference :
      Json.t -> (TypeMap__DigTypes.shortReference, string) Belt.Result.t) =
      fun value ->
        (fun json ->
           match json with
           | ((Json.Array (arg0::arg1::arg2::[]))[@explicit_arity ]) ->
               (match (fun string ->
                         match string with
                         | ((Json.String (string))[@explicit_arity ]) ->
                             ((Belt.Result.Ok (string))[@explicit_arity ])
                         | _ -> ((Error ("epected a string"))
                             [@explicit_arity ])) arg2
                with
                | Belt.Result.Ok arg2 ->
                    (match (fun list ->
                              match list with
                              | ((Json.Array (items))[@explicit_arity ]) ->
                                  let transformer string =
                                    match string with
                                    | ((Json.String
                                        (string))[@explicit_arity ]) ->
                                        ((Belt.Result.Ok (string))
                                        [@explicit_arity ])
                                    | _ -> ((Error ("epected a string"))
                                        [@explicit_arity ]) in
                                  let rec loop items =
                                    match items with
                                    | [] -> ((Belt.Result.Ok ([]))
                                        [@explicit_arity ])
                                    | one::rest ->
                                        (match transformer one with
                                         | ((Belt.Result.Error
                                             (error))[@explicit_arity ]) ->
                                             ((Belt.Result.Error (error))
                                             [@explicit_arity ])
                                         | ((Belt.Result.Ok
                                             (value))[@explicit_arity ]) ->
                                             (match loop rest with
                                              | ((Belt.Result.Error
                                                  (error))[@explicit_arity ])
                                                  ->
                                                  ((Belt.Result.Error (error))
                                                  [@explicit_arity ])
                                              | ((Belt.Result.Ok
                                                  (rest))[@explicit_arity ])
                                                  ->
                                                  ((Belt.Result.Ok
                                                      ((value :: rest)))
                                                  [@explicit_arity ]))) in
                                  loop items
                              | _ ->
                                  ((Belt.Result.Error ("expected an array"))
                                  [@explicit_arity ])) arg1
                     with
                     | Belt.Result.Ok arg1 ->
                         (match deserialize_Analyze__TopTypes____moduleName
                                  arg0
                          with
                          | Belt.Result.Ok arg0 ->
                              Belt.Result.Ok (arg0, arg1, arg2)
                          | Error error -> Error error)
                     | Error error -> Error error)
                | Error error -> Error error)
           | _ -> ((Belt.Result.Error ("Expected array"))[@explicit_arity ]))
          value
    and deserialize_TypeMap__DigTypes____typeMap :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t -> ('arg0 TypeMap__DigTypes.typeMap, string) Belt.Result.t
      =
      fun referenceTransformer ->
        fun value ->
          (deserialize_Stdlib__hashtbl____t
             deserialize_TypeMap__DigTypes____shortReference
             (deserialize_SharedTypes__SimpleType__declaration
                (deserialize_TypeMap__DigTypes____typeSource
                   referenceTransformer))) value
    and deserialize_TypeMap__DigTypes____typeSource :
      type arg0 .
        (Json.t -> (arg0, string) Belt.Result.t) ->
          Json.t ->
            (arg0 TypeMap__DigTypes.typeSource, string) Belt.Result.t
      =
      fun referenceTransformer ->
        fun constructor ->
          match constructor with
          | Json.Array (tag::arg0::[]) when (Json.String "Builtin") = tag ->
              (match (fun string ->
                        match string with
                        | ((Json.String (string))[@explicit_arity ]) ->
                            ((Belt.Result.Ok (string))[@explicit_arity ])
                        | _ -> ((Error ("epected a string"))
                            [@explicit_arity ])) arg0
               with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Builtin (arg0) : arg0 TypeMap__DigTypes.typeSource)
               | Error error -> Error error)
          | Json.Array (tag::arg0::[]) when (Json.String "Public") = tag ->
              (match referenceTransformer arg0 with
               | Belt.Result.Ok arg0 ->
                   Belt.Result.Ok
                     (Public (arg0) : arg0 TypeMap__DigTypes.typeSource)
               | Error error -> Error error)
          | Json.Array (tag::[]) when (Json.String "NotFound") = tag ->
              Belt.Result.Ok (NotFound : arg0 TypeMap__DigTypes.typeSource)
          | _ -> Error "Expected an array"
  end
module SerializeRaw =
  struct
    let rec (serialize_Analyze__TopTypes____moduleName :
      Analyze__TopTypes.moduleName -> Json.t) =
      fun value -> (fun s -> ((Json.String (s))[@explicit_arity ])) value
    and serialize_Belt__Belt_HashMapInt____t :
      'arg0 . ('arg0 -> Json.t) -> 'arg0 Belt__Belt_HashMapInt.t -> Json.t =
      fun arg0Transformer ->
        TransformHelpers.serialize_Belt__Belt_HashMapInt____t arg0Transformer
    and serialize_SharedTypes__SimpleType__body :
      'arg0 .
        ('arg0 -> Json.t) -> 'arg0 SharedTypes.SimpleType.body -> Json.t
      =
      fun sourceTransformer ->
        fun constructor ->
          match constructor with
          | Open -> Json.Array [Json.String "Open"]
          | Abstract -> Json.Array [Json.String "Abstract"]
          | Expr arg0 ->
              Json.Array
                [Json.String "Expr";
                (serialize_SharedTypes__SimpleType__expr sourceTransformer)
                  arg0]
          | Record arg0 ->
              Json.Array
                [Json.String "Record";
                ((fun list ->
                    Json.Array
                      (Belt.List.map list
                         (fun (arg0, arg1) ->
                            Json.Array
                              [((fun s -> ((Json.String (s))
                                   [@explicit_arity ]))) arg0;
                              (serialize_SharedTypes__SimpleType__expr
                                 sourceTransformer) arg1])))) arg0]
          | Variant arg0 ->
              Json.Array
                [Json.String "Variant";
                ((fun list ->
                    Json.Array
                      (Belt.List.map list
                         (fun (arg0, arg1, arg2) ->
                            Json.Array
                              [((fun s -> ((Json.String (s))
                                   [@explicit_arity ]))) arg0;
                              ((fun list ->
                                  Json.Array
                                    (Belt.List.map list
                                       (serialize_SharedTypes__SimpleType__expr
                                          sourceTransformer)))) arg1;
                              (((fun transformer ->
                                   function
                                   | None -> Json.Null
                                   | ((Some (v))[@explicit_arity ]) ->
                                       transformer v))
                                 (serialize_SharedTypes__SimpleType__expr
                                    sourceTransformer)) arg2])))) arg0]
    and serialize_SharedTypes__SimpleType__declaration :
      'arg0 .
        ('arg0 -> Json.t) ->
          'arg0 SharedTypes.SimpleType.declaration -> Json.t
      =
      fun sourceTransformer ->
        fun record ->
          Json.Object
            [("name",
               (((fun s -> ((Json.String (s))[@explicit_arity ])))
                  record.name));
            ("variables",
              (((fun list ->
                   Json.Array
                     (Belt.List.map list
                        (serialize_SharedTypes__SimpleType__expr
                           sourceTransformer)))) record.variables));
            ("body",
              ((serialize_SharedTypes__SimpleType__body sourceTransformer)
                 record.body))]
    and serialize_SharedTypes__SimpleType__expr :
      'arg0 .
        ('arg0 -> Json.t) -> 'arg0 SharedTypes.SimpleType.expr -> Json.t
      =
      fun sourceTransformer ->
        fun constructor ->
          match constructor with
          | Variable arg0 ->
              Json.Array
                [Json.String "Variable";
                ((fun s -> ((Json.String (s))[@explicit_arity ]))) arg0]
          | AnonVariable -> Json.Array [Json.String "AnonVariable"]
          | Reference (arg0, arg1) ->
              Json.Array
                [Json.String "Reference";
                sourceTransformer arg0;
                ((fun list ->
                    Json.Array
                      (Belt.List.map list
                         (serialize_SharedTypes__SimpleType__expr
                            sourceTransformer)))) arg1]
          | Tuple arg0 ->
              Json.Array
                [Json.String "Tuple";
                ((fun list ->
                    Json.Array
                      (Belt.List.map list
                         (serialize_SharedTypes__SimpleType__expr
                            sourceTransformer)))) arg0]
          | Fn (arg0, arg1) ->
              Json.Array
                [Json.String "Fn";
                ((fun list ->
                    Json.Array
                      (Belt.List.map list
                         (fun (arg0, arg1) ->
                            Json.Array
                              [(((fun transformer ->
                                    function
                                    | None -> Json.Null
                                    | ((Some (v))[@explicit_arity ]) ->
                                        transformer v))
                                  (fun s -> ((Json.String (s))
                                     [@explicit_arity ]))) arg0;
                              (serialize_SharedTypes__SimpleType__expr
                                 sourceTransformer) arg1])))) arg0;
                (serialize_SharedTypes__SimpleType__expr sourceTransformer)
                  arg1]
          | Other -> Json.Array [Json.String "Other"]
    and serialize_Stdlib__hashtbl____t :
      'arg0 'arg1 .
        ('arg0 -> Json.t) ->
          ('arg1 -> Json.t) -> ('arg0, 'arg1) Stdlib__hashtbl.t -> Json.t
      =
      fun aTransformer ->
        fun bTransformer ->
          TransformHelpers.serialize_Stdlib__hashtbl____t aTransformer
            bTransformer
    and (serialize_TypeMapSerde__Config____custom :
      TypeMapSerde__Config.custom -> Json.t) =
      fun record ->
        Json.Object
          [("module_",
             (((fun s -> ((Json.String (s))[@explicit_arity ])))
                record.module_));
          ("path",
            (((fun list ->
                 Json.Array
                   (Belt.List.map list
                      (fun s -> ((Json.String (s))[@explicit_arity ])))))
               record.path));
          ("name",
            (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.name));
          ("args",
            (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
               record.args))]
    and (serialize_TypeMapSerde__Config____engine :
      TypeMapSerde__Config.engine -> Json.t) =
      fun constructor ->
        match constructor with
        | Rex_json -> Json.Array [Json.String "Rex_json"]
        | Bs_json -> Json.Array [Json.String "Bs_json"]
    and (serialize_TypeMapSerde__Config____entry :
      TypeMapSerde__Config.entry -> Json.t) =
      fun record ->
        Json.Object
          [("file",
             (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.file));
          ("type_",
            (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.type_))]
    and (serialize_TypeMapSerde__Config____t :
      TypeMapSerde__Config.t -> Json.t) =
      fun record ->
        Json.Object
          [("version",
             (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
                record.version));
          ("output",
            (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.output));
          ("engine",
            (serialize_TypeMapSerde__Config____engine record.engine));
          ("entries",
            (((fun list ->
                 Json.Array
                   (Belt.List.map list
                      serialize_TypeMapSerde__Config____entry)))
               record.entries));
          ("custom",
            (((fun list ->
                 Json.Array
                   (Belt.List.map list
                      serialize_TypeMapSerde__Config____custom)))
               record.custom))]
    and serialize_TypeMap__DigTypes____lockfile :
      'arg0 . ('arg0 -> Json.t) -> 'arg0 TypeMap__DigTypes.lockfile -> Json.t
      =
      fun referenceTransformer ->
        fun record ->
          Json.Object
            [("version",
               (((fun i -> ((Json.Number ((float_of_int i)))
                    [@explicit_arity ]))) record.version));
            ("pastVersions",
              ((serialize_Stdlib__hashtbl____t
                  (fun i -> ((Json.Number ((float_of_int i)))
                     [@explicit_arity ]))
                  (serialize_TypeMap__DigTypes____typeMap
                     referenceTransformer)) record.pastVersions));
            ("current",
              ((serialize_TypeMap__DigTypes____typeMap referenceTransformer)
                 record.current))]
    and (serialize_TypeMap__DigTypes____serializableLockfile :
      TypeMap__DigTypes.serializableLockfile -> Json.t) =
      fun value ->
        (serialize_TypeMap__DigTypes____lockfile
           serialize_TypeMap__DigTypes____shortReference) value
    and (serialize_TypeMap__DigTypes____shortReference :
      TypeMap__DigTypes.shortReference -> Json.t) =
      fun value ->
        (fun (arg0, arg1, arg2) ->
           Json.Array
             [serialize_Analyze__TopTypes____moduleName arg0;
             ((fun list ->
                 Json.Array
                   (Belt.List.map list
                      (fun s -> ((Json.String (s))[@explicit_arity ])))))
               arg1;
             ((fun s -> ((Json.String (s))[@explicit_arity ]))) arg2]) value
    and serialize_TypeMap__DigTypes____typeMap :
      'arg0 . ('arg0 -> Json.t) -> 'arg0 TypeMap__DigTypes.typeMap -> Json.t
      =
      fun referenceTransformer ->
        fun value ->
          (serialize_Stdlib__hashtbl____t
             serialize_TypeMap__DigTypes____shortReference
             (serialize_SharedTypes__SimpleType__declaration
                (serialize_TypeMap__DigTypes____typeSource
                   referenceTransformer))) value
    and serialize_TypeMap__DigTypes____typeSource :
      'arg0 .
        ('arg0 -> Json.t) -> 'arg0 TypeMap__DigTypes.typeSource -> Json.t
      =
      fun referenceTransformer ->
        fun constructor ->
          match constructor with
          | Builtin arg0 ->
              Json.Array
                [Json.String "Builtin";
                ((fun s -> ((Json.String (s))[@explicit_arity ]))) arg0]
          | Public arg0 ->
              Json.Array [Json.String "Public"; referenceTransformer arg0]
          | NotFound -> Json.Array [Json.String "NotFound"]
  end
include SerializeRaw
include DeserializeRaw