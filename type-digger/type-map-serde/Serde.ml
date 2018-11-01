let rec (deserialize_TypeMap__DigTypes____shortReference :
  Json.t -> (TypeMap__DigTypes.shortReference, string) Belt.Result.t) =
  fun value ->
    (fun json ->
       match json with
       | ((Json.Array (arg0::arg1::arg2::[]))[@explicit_arity ]) ->
           (match (fun string ->
                     match string with
                     | ((Json.String (string))[@explicit_arity ]) ->
                         ((Belt.Result.Ok (string))[@explicit_arity ])
                     | _ ->
                         ((Error
                             ((("epected a string")
                               [@reason.raw_literal "epected a string"])))
                         [@explicit_arity ])) arg2
            with
            | Belt.Result.Ok arg2 ->
                (match (fun list ->
                          match list with
                          | ((Json.Array (items))[@explicit_arity ]) ->
                              let transformer string =
                                match string with
                                | ((Json.String (string))[@explicit_arity ])
                                    -> ((Belt.Result.Ok (string))
                                    [@explicit_arity ])
                                | _ ->
                                    ((Error
                                        ((("epected a string")
                                          [@reason.raw_literal
                                            "epected a string"])))
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
                          | _ ->
                              ((Belt.Result.Error
                                  ((("expected an array")
                                    [@reason.raw_literal "expected an array"])))
                              [@explicit_arity ])) arg1
                 with
                 | Belt.Result.Ok arg1 ->
                     (match deserialize_Analyze__TopTypes____moduleName arg0
                      with
                      | Belt.Result.Ok arg0 ->
                          Belt.Result.Ok (arg0, arg1, arg2)
                      | Error error -> Error error)
                 | Error error -> Error error)
            | Error error -> Error error)
       | _ ->
           ((Belt.Result.Error
               ((("Expected array")[@reason.raw_literal "Expected array"])))
           [@explicit_arity ])) value
and (deserialize_TypeMap__DigTypes____serializableLockfile :
  Json.t -> (TypeMap__DigTypes.serializableLockfile, string) Belt.Result.t) =
  fun value ->
    (deserialize_TypeMap__DigTypes____lockfile
       deserialize_TypeMap__DigTypes____shortReference) value
and deserialize_Belt__Belt_HashMapInt____t :
  type arg0 .
    (Json.t -> (arg0, string) Belt.Result.t) ->
      Json.t -> (arg0 Belt__Belt_HashMapInt.t, string) Belt.Result.t
  =
  fun arg0Transformer ->
    TransformHelpers.deserialize_Belt__Belt_HashMapInt____t arg0Transformer
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
                    | _ ->
                        ((Error
                            ((("epected a string")
                              [@reason.raw_literal "epected a string"])))
                        [@explicit_arity ])) arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Variable (arg0) : arg0 SharedTypes.SimpleType.expr)
           | Error error -> Error error)
      | Json.Array (tag::[]) when (Json.String "AnonVariable") = tag ->
          Belt.Result.Ok (AnonVariable : arg0 SharedTypes.SimpleType.expr)
      | Json.Array (tag::arg0::arg1::[]) when (Json.String "Reference") = tag
          ->
          (match (fun list ->
                    match list with
                    | ((Json.Array (items))[@explicit_arity ]) ->
                        let transformer =
                          deserialize_SharedTypes__SimpleType__expr
                            sourceTransformer in
                        let rec loop items =
                          match items with
                          | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                          | one::rest ->
                              (match transformer one with
                               | ((Belt.Result.Error
                                   (error))[@explicit_arity ]) ->
                                   ((Belt.Result.Error (error))
                                   [@explicit_arity ])
                               | ((Belt.Result.Ok (value))[@explicit_arity ])
                                   ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Belt.Result.Ok
                                        (rest))[@explicit_arity ]) ->
                                        ((Belt.Result.Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop items
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
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
                          | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                          | one::rest ->
                              (match transformer one with
                               | ((Belt.Result.Error
                                   (error))[@explicit_arity ]) ->
                                   ((Belt.Result.Error (error))
                                   [@explicit_arity ])
                               | ((Belt.Result.Ok (value))[@explicit_arity ])
                                   ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Belt.Result.Ok
                                        (rest))[@explicit_arity ]) ->
                                        ((Belt.Result.Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop items
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
                        [@explicit_arity ])) arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Tuple (arg0) : arg0 SharedTypes.SimpleType.expr)
           | Error error -> Error error)
      | Json.Array (tag::arg0::arg1::[]) when (Json.String "Fn") = tag ->
          (match (deserialize_SharedTypes__SimpleType__expr sourceTransformer)
                   arg1
           with
           | Belt.Result.Ok arg1 ->
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
                                        (match ((fun transformer ->
                                                   fun option ->
                                                     match option with
                                                     | Json.Null ->
                                                         ((Belt.Result.Ok
                                                             (None))
                                                         [@explicit_arity ])
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
                                                         (string))[@explicit_arity
                                                                    ])
                                                         ->
                                                         ((Belt.Result.Ok
                                                             (string))
                                                         [@explicit_arity ])
                                                     | _ ->
                                                         ((Error
                                                             ((("epected a string")
                                                               [@reason.raw_literal
                                                                 "epected a string"])))
                                                         [@explicit_arity ])))
                                                 arg0
                                         with
                                         | Belt.Result.Ok arg0 ->
                                             Belt.Result.Ok (arg0, arg1)
                                         | Error error -> Error error)
                                    | Error error -> Error error)
                               | _ ->
                                   ((Belt.Result.Error
                                       ((("Expected array")
                                         [@reason.raw_literal
                                           "Expected array"])))
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
                         | _ ->
                             ((Belt.Result.Error
                                 ((("expected an array")
                                   [@reason.raw_literal "expected an array"])))
                             [@explicit_arity ])) arg0
                with
                | Belt.Result.Ok arg0 ->
                    Belt.Result.Ok
                      (Fn (arg0, arg1) : arg0 SharedTypes.SimpleType.expr)
                | Error error -> Error error)
           | Error error -> Error error)
      | Json.Array (tag::[]) when (Json.String "Other") = tag ->
          Belt.Result.Ok (Other : arg0 SharedTypes.SimpleType.expr)
      | _ -> Error "Expected an array"
and deserialize_TypeMap__DigTypes____typeSource :
  type arg0 .
    (Json.t -> (arg0, string) Belt.Result.t) ->
      Json.t -> (arg0 TypeMap__DigTypes.typeSource, string) Belt.Result.t
  =
  fun referenceTransformer ->
    fun constructor ->
      match constructor with
      | Json.Array (tag::arg0::[]) when (Json.String "Builtin") = tag ->
          (match (fun string ->
                    match string with
                    | ((Json.String (string))[@explicit_arity ]) ->
                        ((Belt.Result.Ok (string))[@explicit_arity ])
                    | _ ->
                        ((Error
                            ((("epected a string")
                              [@reason.raw_literal "epected a string"])))
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
          (match (deserialize_SharedTypes__SimpleType__expr sourceTransformer)
                   arg0
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
                          | ((Json.Array (arg0::arg1::[]))[@explicit_arity ])
                              ->
                              (match (deserialize_SharedTypes__SimpleType__expr
                                        sourceTransformer) arg1
                               with
                               | Belt.Result.Ok arg1 ->
                                   (match (fun string ->
                                             match string with
                                             | ((Json.String
                                                 (string))[@explicit_arity ])
                                                 ->
                                                 ((Belt.Result.Ok (string))
                                                 [@explicit_arity ])
                                             | _ ->
                                                 ((Error
                                                     ((("epected a string")
                                                       [@reason.raw_literal
                                                         "epected a string"])))
                                                 [@explicit_arity ])) arg0
                                    with
                                    | Belt.Result.Ok arg0 ->
                                        Belt.Result.Ok (arg0, arg1)
                                    | Error error -> Error error)
                               | Error error -> Error error)
                          | _ ->
                              ((Belt.Result.Error
                                  ((("Expected array")
                                    [@reason.raw_literal "Expected array"])))
                              [@explicit_arity ]) in
                        let rec loop items =
                          match items with
                          | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                          | one::rest ->
                              (match transformer one with
                               | ((Belt.Result.Error
                                   (error))[@explicit_arity ]) ->
                                   ((Belt.Result.Error (error))
                                   [@explicit_arity ])
                               | ((Belt.Result.Ok (value))[@explicit_arity ])
                                   ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Belt.Result.Ok
                                        (rest))[@explicit_arity ]) ->
                                        ((Belt.Result.Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop items
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
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
                              (arg0::arg1::arg2::[]))[@explicit_arity ]) ->
                              (match ((fun transformer ->
                                         fun option ->
                                           match option with
                                           | Json.Null ->
                                               ((Belt.Result.Ok (None))
                                               [@explicit_arity ])
                                           | _ ->
                                               (match transformer option with
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
                                                          [@explicit_arity ])))
                                                    [@explicit_arity ])))
                                        (deserialize_SharedTypes__SimpleType__expr
                                           sourceTransformer)) arg2
                               with
                               | Belt.Result.Ok arg2 ->
                                   (match (fun list ->
                                             match list with
                                             | ((Json.Array
                                                 (items))[@explicit_arity ])
                                                 ->
                                                 let transformer =
                                                   deserialize_SharedTypes__SimpleType__expr
                                                     sourceTransformer in
                                                 let rec loop items =
                                                   match items with
                                                   | [] ->
                                                       ((Belt.Result.Ok ([]))
                                                       [@explicit_arity ])
                                                   | one::rest ->
                                                       (match transformer one
                                                        with
                                                        | ((Belt.Result.Error
                                                            (error))[@explicit_arity
                                                                    ])
                                                            ->
                                                            ((Belt.Result.Error
                                                                (error))
                                                            [@explicit_arity
                                                              ])
                                                        | ((Belt.Result.Ok
                                                            (value))[@explicit_arity
                                                                    ])
                                                            ->
                                                            (match loop rest
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
                                                                 (rest))
                                                                 [@explicit_arity
                                                                   ])
                                                                 ->
                                                                 ((Belt.Result.Ok
                                                                    ((value
                                                                    :: rest)))
                                                                 [@explicit_arity
                                                                   ]))) in
                                                 loop items
                                             | _ ->
                                                 ((Belt.Result.Error
                                                     ((("expected an array")
                                                       [@reason.raw_literal
                                                         "expected an array"])))
                                                 [@explicit_arity ])) arg1
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
                                                          ((("epected a string")
                                                            [@reason.raw_literal
                                                              "epected a string"])))
                                                      [@explicit_arity ]))
                                                 arg0
                                         with
                                         | Belt.Result.Ok arg0 ->
                                             Belt.Result.Ok
                                               (arg0, arg1, arg2)
                                         | Error error -> Error error)
                                    | Error error -> Error error)
                               | Error error -> Error error)
                          | _ ->
                              ((Belt.Result.Error
                                  ((("Expected array")
                                    [@reason.raw_literal "Expected array"])))
                              [@explicit_arity ]) in
                        let rec loop items =
                          match items with
                          | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                          | one::rest ->
                              (match transformer one with
                               | ((Belt.Result.Error
                                   (error))[@explicit_arity ]) ->
                                   ((Belt.Result.Error (error))
                                   [@explicit_arity ])
                               | ((Belt.Result.Ok (value))[@explicit_arity ])
                                   ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Belt.Result.Ok
                                        (rest))[@explicit_arity ]) ->
                                        ((Belt.Result.Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop items
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
                        [@explicit_arity ])) arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Variant (arg0) : arg0 SharedTypes.SimpleType.body)
           | Error error -> Error error)
      | _ -> Error "Expected an array"
and deserialize_TypeMap__DigTypes____lockfile :
  type arg0 .
    (Json.t -> (arg0, string) Belt.Result.t) ->
      Json.t -> (arg0 TypeMap__DigTypes.lockfile, string) Belt.Result.t
  =
  fun referenceTransformer ->
    fun record ->
      match record with
      | ((Json.Object (items))[@explicit_arity ]) ->
          (match Belt.List.getAssoc items "current" (=) with
           | None ->
               ((Belt.Result.Error
                   (((("No attribute ")[@reason.raw_literal "No attribute "])
                       ^ "current")))
               [@explicit_arity ])
           | ((Some (json))[@explicit_arity ]) ->
               (match (fun list ->
                         match list with
                         | ((Json.Array (items))[@explicit_arity ]) ->
                             let transformer json =
                               match json with
                               | ((Json.Array
                                   (arg0::arg1::[]))[@explicit_arity ]) ->
                                   (match (deserialize_SharedTypes__SimpleType__declaration
                                             (deserialize_TypeMap__DigTypes____typeSource
                                                referenceTransformer)) arg1
                                    with
                                    | Belt.Result.Ok arg1 ->
                                        (match deserialize_TypeMap__DigTypes____shortReference
                                                 arg0
                                         with
                                         | Belt.Result.Ok arg0 ->
                                             Belt.Result.Ok (arg0, arg1)
                                         | Error error -> Error error)
                                    | Error error -> Error error)
                               | _ ->
                                   ((Belt.Result.Error
                                       ((("Expected array")
                                         [@reason.raw_literal
                                           "Expected array"])))
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
                         | _ ->
                             ((Belt.Result.Error
                                 ((("expected an array")
                                   [@reason.raw_literal "expected an array"])))
                             [@explicit_arity ])) json
                with
                | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                    ((Belt.Result.Error (error))[@explicit_arity ])
                | ((Belt.Result.Ok (attr_current))[@explicit_arity ]) ->
                    (match Belt.List.getAssoc items "pastVersions" (=) with
                     | None ->
                         ((Belt.Result.Error
                             (((("No attribute ")
                                 [@reason.raw_literal "No attribute "]) ^
                                 "pastVersions")))
                         [@explicit_arity ])
                     | ((Some (json))[@explicit_arity ]) ->
                         (match (deserialize_Belt__Belt_HashMapInt____t
                                   (fun list ->
                                      match list with
                                      | ((Json.Array
                                          (items))[@explicit_arity ]) ->
                                          let transformer json =
                                            match json with
                                            | ((Json.Array
                                                (arg0::arg1::[]))[@explicit_arity
                                                                   ])
                                                ->
                                                (match (deserialize_SharedTypes__SimpleType__declaration
                                                          (deserialize_TypeMap__DigTypes____typeSource
                                                             referenceTransformer))
                                                         arg1
                                                 with
                                                 | Belt.Result.Ok arg1 ->
                                                     (match deserialize_TypeMap__DigTypes____shortReference
                                                              arg0
                                                      with
                                                      | Belt.Result.Ok arg0
                                                          ->
                                                          Belt.Result.Ok
                                                            (arg0, arg1)
                                                      | Error error ->
                                                          Error error)
                                                 | Error error -> Error error)
                                            | _ ->
                                                ((Belt.Result.Error
                                                    ((("Expected array")
                                                      [@reason.raw_literal
                                                        "Expected array"])))
                                                [@explicit_arity ]) in
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
                                              ((("expected an array")
                                                [@reason.raw_literal
                                                  "expected an array"])))
                                          [@explicit_arity ]))) json
                          with
                          | ((Belt.Result.Error (error))[@explicit_arity ])
                              -> ((Belt.Result.Error (error))
                              [@explicit_arity ])
                          | ((Belt.Result.Ok
                              (attr_pastVersions))[@explicit_arity ]) ->
                              (match Belt.List.getAssoc items "version" (=)
                               with
                               | None ->
                                   ((Belt.Result.Error
                                       (((("No attribute ")
                                           [@reason.raw_literal
                                             "No attribute "])
                                           ^ "version")))
                                   [@explicit_arity ])
                               | ((Some (json))[@explicit_arity ]) ->
                                   (match (fun number ->
                                             match number with
                                             | ((Json.Number
                                                 (number))[@explicit_arity ])
                                                 ->
                                                 ((Belt.Result.Ok
                                                     ((int_of_float number)))
                                                 [@explicit_arity ])
                                             | _ ->
                                                 ((Error
                                                     ((("Expected a float")
                                                       [@reason.raw_literal
                                                         "Expected a float"])))
                                                 [@explicit_arity ])) json
                                    with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Belt.Result.Ok
                                        (attr_version))[@explicit_arity ]) ->
                                        Belt.Result.Ok
                                          {
                                            version = attr_version;
                                            pastVersions = attr_pastVersions;
                                            current = attr_current
                                          }))))))
      | _ ->
          ((Belt.Result.Error
              ((("Expected an object")
                [@reason.raw_literal "Expected an object"])))
          [@explicit_arity ])
and deserialize_SharedTypes__SimpleType__declaration :
  type arg0 .
    (Json.t -> (arg0, string) Belt.Result.t) ->
      Json.t ->
        (arg0 SharedTypes.SimpleType.declaration, string) Belt.Result.t
  =
  fun sourceTransformer ->
    fun record ->
      match record with
      | ((Json.Object (items))[@explicit_arity ]) ->
          (match Belt.List.getAssoc items "body" (=) with
           | None ->
               ((Belt.Result.Error
                   (((("No attribute ")[@reason.raw_literal "No attribute "])
                       ^ "body")))
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
                             (((("No attribute ")
                                 [@reason.raw_literal "No attribute "]) ^
                                 "variables")))
                         [@explicit_arity ])
                     | ((Some (json))[@explicit_arity ]) ->
                         (match (fun list ->
                                   match list with
                                   | ((Json.Array (items))[@explicit_arity ])
                                       ->
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
                                                  (error))[@explicit_arity ])
                                                  ->
                                                  ((Belt.Result.Error (error))
                                                  [@explicit_arity ])
                                              | ((Belt.Result.Ok
                                                  (value))[@explicit_arity ])
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
                                                           ((value :: rest)))
                                                       [@explicit_arity ]))) in
                                       loop items
                                   | _ ->
                                       ((Belt.Result.Error
                                           ((("expected an array")
                                             [@reason.raw_literal
                                               "expected an array"])))
                                       [@explicit_arity ])) json
                          with
                          | ((Belt.Result.Error (error))[@explicit_arity ])
                              -> ((Belt.Result.Error (error))
                              [@explicit_arity ])
                          | ((Belt.Result.Ok
                              (attr_variables))[@explicit_arity ]) ->
                              (match Belt.List.getAssoc items "name" (=) with
                               | None ->
                                   ((Belt.Result.Error
                                       (((("No attribute ")
                                           [@reason.raw_literal
                                             "No attribute "])
                                           ^ "name")))
                                   [@explicit_arity ])
                               | ((Some (json))[@explicit_arity ]) ->
                                   (match (fun string ->
                                             match string with
                                             | ((Json.String
                                                 (string))[@explicit_arity ])
                                                 ->
                                                 ((Belt.Result.Ok (string))
                                                 [@explicit_arity ])
                                             | _ ->
                                                 ((Error
                                                     ((("epected a string")
                                                       [@reason.raw_literal
                                                         "epected a string"])))
                                                 [@explicit_arity ])) json
                                    with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Belt.Result.Ok
                                        (attr_name))[@explicit_arity ]) ->
                                        Belt.Result.Ok
                                          {
                                            name = attr_name;
                                            variables = attr_variables;
                                            body = attr_body
                                          }))))))
      | _ ->
          ((Belt.Result.Error
              ((("Expected an object")
                [@reason.raw_literal "Expected an object"])))
          [@explicit_arity ])
and (deserialize_Analyze__TopTypes____moduleName :
  Json.t -> (Analyze__TopTypes.moduleName, string) Belt.Result.t) =
  fun value ->
    (fun string ->
       match string with
       | ((Json.String (string))[@explicit_arity ]) ->
           ((Belt.Result.Ok (string))[@explicit_arity ])
       | _ ->
           ((Error
               ((("epected a string")
                 [@reason.raw_literal "epected a string"])))
           [@explicit_arity ])) value
let rec (serialize_TypeMap__DigTypes____shortReference :
  TypeMap__DigTypes.shortReference -> Json.t) =
  fun value ->
    (fun (arg0, arg1, arg2) ->
       Json.Array
         [serialize_Analyze__TopTypes____moduleName arg0;
         ((fun list ->
             Json.Array
               (Belt.List.map list
                  (fun s -> ((Json.String (s))[@explicit_arity ]))))) arg1;
         ((fun s -> ((Json.String (s))[@explicit_arity ]))) arg2]) value
and (serialize_TypeMap__DigTypes____serializableLockfile :
  TypeMap__DigTypes.serializableLockfile -> Json.t) =
  fun value ->
    (serialize_TypeMap__DigTypes____lockfile
       serialize_TypeMap__DigTypes____shortReference) value
and serialize_Belt__Belt_HashMapInt____t :
  type arg0 . (arg0 -> Json.t) -> arg0 Belt__Belt_HashMapInt.t -> Json.t =
  fun arg0Transformer ->
    TransformHelpers.serialize_Belt__Belt_HashMapInt____t arg0Transformer
and serialize_SharedTypes__SimpleType__expr :
  type arg0 . (arg0 -> Json.t) -> arg0 SharedTypes.SimpleType.expr -> Json.t =
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
                              (fun s -> ((Json.String (s))[@explicit_arity ])))
                             arg0;
                          (serialize_SharedTypes__SimpleType__expr
                             sourceTransformer) arg1])))) arg0;
            (serialize_SharedTypes__SimpleType__expr sourceTransformer) arg1]
      | Other -> Json.Array [Json.String "Other"]
and serialize_TypeMap__DigTypes____typeSource :
  type arg0 . (arg0 -> Json.t) -> arg0 TypeMap__DigTypes.typeSource -> Json.t =
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
and serialize_SharedTypes__SimpleType__body :
  type arg0 . (arg0 -> Json.t) -> arg0 SharedTypes.SimpleType.body -> Json.t =
  fun sourceTransformer ->
    fun constructor ->
      match constructor with
      | Open -> Json.Array [Json.String "Open"]
      | Abstract -> Json.Array [Json.String "Abstract"]
      | Expr arg0 ->
          Json.Array
            [Json.String "Expr";
            (serialize_SharedTypes__SimpleType__expr sourceTransformer) arg0]
      | Record arg0 ->
          Json.Array
            [Json.String "Record";
            ((fun list ->
                Json.Array
                  (Belt.List.map list
                     (fun (arg0, arg1) ->
                        Json.Array
                          [((fun s -> ((Json.String (s))[@explicit_arity ])))
                             arg0;
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
                          [((fun s -> ((Json.String (s))[@explicit_arity ])))
                             arg0;
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
and serialize_TypeMap__DigTypes____lockfile :
  type arg0 . (arg0 -> Json.t) -> arg0 TypeMap__DigTypes.lockfile -> Json.t =
  fun referenceTransformer ->
    fun record ->
      Json.Object
        [("version",
           (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
              record.version));
        ("pastVersions",
          ((serialize_Belt__Belt_HashMapInt____t
              (fun list ->
                 Json.Array
                   (Belt.List.map list
                      (fun (arg0, arg1) ->
                         Json.Array
                           [serialize_TypeMap__DigTypes____shortReference
                              arg0;
                           (serialize_SharedTypes__SimpleType__declaration
                              (serialize_TypeMap__DigTypes____typeSource
                                 referenceTransformer)) arg1]))))
             record.pastVersions));
        ("current",
          (((fun list ->
               Json.Array
                 (Belt.List.map list
                    (fun (arg0, arg1) ->
                       Json.Array
                         [serialize_TypeMap__DigTypes____shortReference arg0;
                         (serialize_SharedTypes__SimpleType__declaration
                            (serialize_TypeMap__DigTypes____typeSource
                               referenceTransformer)) arg1]))))
             record.current))]
and serialize_SharedTypes__SimpleType__declaration :
  type arg0 .
    (arg0 -> Json.t) -> arg0 SharedTypes.SimpleType.declaration -> Json.t
  =
  fun sourceTransformer ->
    fun record ->
      Json.Object
        [("name",
           (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.name));
        ("variables",
          (((fun list ->
               Json.Array
                 (Belt.List.map list
                    (serialize_SharedTypes__SimpleType__expr
                       sourceTransformer)))) record.variables));
        ("body",
          ((serialize_SharedTypes__SimpleType__body sourceTransformer)
             record.body))]
and (serialize_Analyze__TopTypes____moduleName :
  Analyze__TopTypes.moduleName -> Json.t) =
  fun value -> (fun s -> ((Json.String (s))[@explicit_arity ])) value