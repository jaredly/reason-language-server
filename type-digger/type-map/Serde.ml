let rec (deserialize_SharedTypes__Module__contents :
  Json.t -> (SharedTypes.Module.contents, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "topLevel"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "topLevel")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (fun list ->
                       match Js.Json.classify list with
                       | ((JSONArray (items))[@explicit_arity ]) ->
                           let transformer =
                             deserialize_SharedTypes____declared
                               deserialize_SharedTypes__Module__item in
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
                                  | ((Ok (value))[@explicit_arity ]) ->
                                      (match loop rest with
                                       | ((Belt.Result.Error
                                           (error))[@explicit_arity ]) ->
                                           ((Belt.Result.Error (error))
                                           [@explicit_arity ])
                                       | ((Ok (rest))[@explicit_arity ]) ->
                                           ((Ok ((value :: rest)))
                                           [@explicit_arity ]))) in
                           loop (Belt.List.fromArray items)
                       | _ ->
                           ((Belt.Result.Error
                               ((("expected an array")
                                 [@reason.raw_literal "expected an array"])))
                           [@explicit_arity ])) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_topLevel))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "exported"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "exported")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_SharedTypes__Module__exported json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_exported))[@explicit_arity ]) ->
                            Belt.Result.Ok
                              {
                                exported = attr_exported;
                                topLevel = attr_topLevel
                              }))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_SharedTypes__Module__item :
  Json.t -> (SharedTypes.Module.item, string) Belt.Result.t) =
  fun constructor ->
    match constructor with
    | Json.Array [|tag;arg0|] when (Json.String "Value") = tag ->
        (match deserialize_SharedTypes__Value__t arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Value (arg0) : SharedTypes.Module.item)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Type") = tag ->
        (match deserialize_SharedTypes__Type__t arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Type (arg0) : SharedTypes.Module.item)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Module") = tag ->
        (match deserialize_SharedTypes__Module__kind arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Module (arg0) : SharedTypes.Module.item)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "ModuleType") = tag ->
        (match deserialize_SharedTypes__Module__kind arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (ModuleType (arg0) : SharedTypes.Module.item)
         | Error error -> Error error)
    | _ -> Error "Expected an array"
and (deserialize_TypeMap__DigTypes____shortReference :
  Json.t -> (TypeMap__DigTypes.shortReference, string) Belt.Result.t) =
  fun value ->
    (fun json ->
       match Js.Json.classify json with
       | ((JSONArray ([|arg0;arg1;arg2|]))[@explicit_arity ]) ->
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
                          match Js.Json.classify list with
                          | ((JSONArray (items))[@explicit_arity ]) ->
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
                                     | ((Ok (value))[@explicit_arity ]) ->
                                         (match loop rest with
                                          | ((Belt.Result.Error
                                              (error))[@explicit_arity ]) ->
                                              ((Belt.Result.Error (error))
                                              [@explicit_arity ])
                                          | ((Ok (rest))[@explicit_arity ])
                                              -> ((Ok ((value :: rest)))
                                              [@explicit_arity ]))) in
                              loop (Belt.List.fromArray items)
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
and (deserialize_SharedTypes____flexibleDeclaration :
  Json.t -> (SharedTypes.flexibleDeclaration, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "asSimpleDeclaration"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "asSimpleDeclaration")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (failwith "not impl expr") json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_asSimpleDeclaration))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "declarationKind"), (=))
                   with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "declarationKind")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_SharedTypes____kinds json with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_declarationKind))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "declToString"),
                                     (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "declToString")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (failwith "not impl expr") json with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok
                                      (attr_declToString))[@explicit_arity ])
                                      ->
                                      Belt.Result.Ok
                                        {
                                          declToString = attr_declToString;
                                          declarationKind =
                                            attr_declarationKind;
                                          asSimpleDeclaration =
                                            attr_asSimpleDeclaration
                                        }))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and deserialize_SharedTypes____stampMap :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 SharedTypes.stampMap, string) Belt.Result.t
  =
  fun tTransformer ->
    fun value ->
      (deserialize_Hashtbl____t
         (fun number ->
            match number with
            | ((Json.Number (number))[@explicit_arity ]) ->
                ((Belt.Result.Ok ((int_of_float number)))[@explicit_arity ])
            | _ ->
                ((Error
                    ((("Expected a float")
                      [@reason.raw_literal "Expected a float"])))
                [@explicit_arity ])) tTransformer) value
and (deserialize_SharedTypes____visibilityPath :
  Json.t -> (SharedTypes.visibilityPath, string) Belt.Result.t) =
  fun constructor ->
    match constructor with
    | Json.Array [|tag;arg0;arg1|] when (Json.String "File") = tag ->
        (match (fun string ->
                  match string with
                  | ((Json.String (string))[@explicit_arity ]) ->
                      ((Belt.Result.Ok (string))[@explicit_arity ])
                  | _ ->
                      ((Error
                          ((("epected a string")
                            [@reason.raw_literal "epected a string"])))
                      [@explicit_arity ])) arg1
         with
         | Belt.Result.Ok arg1 ->
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
                    (File (arg0, arg1) : SharedTypes.visibilityPath)
              | Error error -> Error error)
         | Error error -> Error error)
    | Json.Array [|tag|] when (Json.String "NotVisible") = tag ->
        Belt.Result.Ok (NotVisible : SharedTypes.visibilityPath)
    | Json.Array [|tag;arg0;arg1|] when (Json.String "IncludedModule") = tag
        ->
        (match deserialize_SharedTypes____visibilityPath arg1 with
         | Belt.Result.Ok arg1 ->
             (match deserialize_Path____t arg0 with
              | Belt.Result.Ok arg0 ->
                  Belt.Result.Ok
                    (IncludedModule (arg0, arg1) : SharedTypes.visibilityPath)
              | Error error -> Error error)
         | Error error -> Error error)
    | Json.Array [|tag;arg0;arg1|] when (Json.String "ExportedModule") = tag
        ->
        (match deserialize_SharedTypes____visibilityPath arg1 with
         | Belt.Result.Ok arg1 ->
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
                    (ExportedModule (arg0, arg1) : SharedTypes.visibilityPath)
              | Error error -> Error error)
         | Error error -> Error error)
    | Json.Array [|tag;arg0;arg1|] when (Json.String "HiddenModule") = tag ->
        (match deserialize_SharedTypes____visibilityPath arg1 with
         | Belt.Result.Ok arg1 ->
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
                    (HiddenModule (arg0, arg1) : SharedTypes.visibilityPath)
              | Error error -> Error error)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Expression") = tag ->
        (match deserialize_SharedTypes____visibilityPath arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Expression (arg0) : SharedTypes.visibilityPath)
         | Error error -> Error error)
    | _ -> Error "Expected an array"
and (deserialize_SharedTypes____kinds :
  Json.t -> (SharedTypes.kinds, string) Belt.Result.t) =
  fun value -> (failwith "not impl expr") value
and deserialize_SharedTypes__SimpleType__expr :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 SharedTypes.SimpleType.expr, string) Belt.Result.t
  =
  fun sourceTransformer ->
    fun constructor ->
      match constructor with
      | Json.Array [|tag;arg0|] when (Json.String "Variable") = tag ->
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
                 (Variable (arg0) : 'arg0 SharedTypes.SimpleType.expr)
           | Error error -> Error error)
      | Json.Array [|tag|] when (Json.String "AnonVariable") = tag ->
          Belt.Result.Ok (AnonVariable : 'arg0 SharedTypes.SimpleType.expr)
      | Json.Array [|tag;arg0;arg1|] when (Json.String "Reference") = tag ->
          (match (fun list ->
                    match Js.Json.classify list with
                    | ((JSONArray (items))[@explicit_arity ]) ->
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
                               | ((Ok (value))[@explicit_arity ]) ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Ok (rest))[@explicit_arity ]) ->
                                        ((Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop (Belt.List.fromArray items)
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
                      (Reference (arg0, arg1) : 'arg0
                                                  SharedTypes.SimpleType.expr)
                | Error error -> Error error)
           | Error error -> Error error)
      | Json.Array [|tag;arg0|] when (Json.String "Tuple") = tag ->
          (match (fun list ->
                    match Js.Json.classify list with
                    | ((JSONArray (items))[@explicit_arity ]) ->
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
                               | ((Ok (value))[@explicit_arity ]) ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Ok (rest))[@explicit_arity ]) ->
                                        ((Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop (Belt.List.fromArray items)
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
                        [@explicit_arity ])) arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Tuple (arg0) : 'arg0 SharedTypes.SimpleType.expr)
           | Error error -> Error error)
      | Json.Array [|tag;arg0;arg1|] when (Json.String "Fn") = tag ->
          (match (deserialize_SharedTypes__SimpleType__expr sourceTransformer)
                   arg1
           with
           | Belt.Result.Ok arg1 ->
               (match (fun list ->
                         match Js.Json.classify list with
                         | ((JSONArray (items))[@explicit_arity ]) ->
                             let transformer json =
                               match Js.Json.classify json with
                               | ((JSONArray
                                   ([|arg0;arg1|]))[@explicit_arity ]) ->
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
                                                          | ((Ok
                                                              (value))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              ((Ok
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
                                    | ((Ok (value))[@explicit_arity ]) ->
                                        (match loop rest with
                                         | ((Belt.Result.Error
                                             (error))[@explicit_arity ]) ->
                                             ((Belt.Result.Error (error))
                                             [@explicit_arity ])
                                         | ((Ok (rest))[@explicit_arity ]) ->
                                             ((Ok ((value :: rest)))
                                             [@explicit_arity ]))) in
                             loop (Belt.List.fromArray items)
                         | _ ->
                             ((Belt.Result.Error
                                 ((("expected an array")
                                   [@reason.raw_literal "expected an array"])))
                             [@explicit_arity ])) arg0
                with
                | Belt.Result.Ok arg0 ->
                    Belt.Result.Ok
                      (Fn (arg0, arg1) : 'arg0 SharedTypes.SimpleType.expr)
                | Error error -> Error error)
           | Error error -> Error error)
      | Json.Array [|tag|] when (Json.String "Other") = tag ->
          Belt.Result.Ok (Other : 'arg0 SharedTypes.SimpleType.expr)
      | _ -> Error "Expected an array"
and (deserialize_SharedTypes__Type__kind :
  Json.t -> (SharedTypes.Type.kind, string) Belt.Result.t) =
  fun constructor ->
    match constructor with
    | Json.Array [|tag;arg0|] when (Json.String "Abstract") = tag ->
        (match ((fun transformer ->
                   fun option ->
                     match option with
                     | Json.Null -> ((Belt.Result.Ok (None))
                         [@explicit_arity ])
                     | _ ->
                         (match transformer option with
                          | ((Belt.Result.Error (error))[@explicit_arity ])
                              -> ((Belt.Result.Error (error))
                              [@explicit_arity ])
                          | ((Ok (value))[@explicit_arity ]) ->
                              ((Ok (((Some (value))[@explicit_arity ])))
                              [@explicit_arity ])))
                  (fun json ->
                     match Js.Json.classify json with
                     | ((JSONArray ([|arg0;arg1|]))[@explicit_arity ]) ->
                         (match (fun list ->
                                   match Js.Json.classify list with
                                   | ((JSONArray (items))[@explicit_arity ])
                                       ->
                                       let transformer =
                                         deserialize_SharedTypes____flexibleType in
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
                                              | ((Ok
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
                                                   | ((Ok
                                                       (rest))[@explicit_arity
                                                                ])
                                                       ->
                                                       ((Ok ((value :: rest)))
                                                       [@explicit_arity ]))) in
                                       loop (Belt.List.fromArray items)
                                   | _ ->
                                       ((Belt.Result.Error
                                           ((("expected an array")
                                             [@reason.raw_literal
                                               "expected an array"])))
                                       [@explicit_arity ])) arg1
                          with
                          | Belt.Result.Ok arg1 ->
                              (match deserialize_Path____t arg0 with
                               | Belt.Result.Ok arg0 ->
                                   Belt.Result.Ok (arg0, arg1)
                               | Error error -> Error error)
                          | Error error -> Error error)
                     | _ ->
                         ((Belt.Result.Error
                             ((("Expected array")
                               [@reason.raw_literal "Expected array"])))
                         [@explicit_arity ]))) arg0
         with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Abstract (arg0) : SharedTypes.Type.kind)
         | Error error -> Error error)
    | Json.Array [|tag|] when (Json.String "Open") = tag ->
        Belt.Result.Ok (Open : SharedTypes.Type.kind)
    | Json.Array [|tag;arg0|] when (Json.String "Tuple") = tag ->
        (match (fun list ->
                  match Js.Json.classify list with
                  | ((JSONArray (items))[@explicit_arity ]) ->
                      let transformer =
                        deserialize_SharedTypes____flexibleType in
                      let rec loop items =
                        match items with
                        | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                        | one::rest ->
                            (match transformer one with
                             | ((Belt.Result.Error
                                 (error))[@explicit_arity ]) ->
                                 ((Belt.Result.Error (error))
                                 [@explicit_arity ])
                             | ((Ok (value))[@explicit_arity ]) ->
                                 (match loop rest with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (rest))[@explicit_arity ]) ->
                                      ((Ok ((value :: rest)))
                                      [@explicit_arity ]))) in
                      loop (Belt.List.fromArray items)
                  | _ ->
                      ((Belt.Result.Error
                          ((("expected an array")
                            [@reason.raw_literal "expected an array"])))
                      [@explicit_arity ])) arg0
         with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Tuple (arg0) : SharedTypes.Type.kind)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Record") = tag ->
        (match (fun list ->
                  match Js.Json.classify list with
                  | ((JSONArray (items))[@explicit_arity ]) ->
                      let transformer =
                        deserialize_SharedTypes__Type__Attribute__t in
                      let rec loop items =
                        match items with
                        | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                        | one::rest ->
                            (match transformer one with
                             | ((Belt.Result.Error
                                 (error))[@explicit_arity ]) ->
                                 ((Belt.Result.Error (error))
                                 [@explicit_arity ])
                             | ((Ok (value))[@explicit_arity ]) ->
                                 (match loop rest with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (rest))[@explicit_arity ]) ->
                                      ((Ok ((value :: rest)))
                                      [@explicit_arity ]))) in
                      loop (Belt.List.fromArray items)
                  | _ ->
                      ((Belt.Result.Error
                          ((("expected an array")
                            [@reason.raw_literal "expected an array"])))
                      [@explicit_arity ])) arg0
         with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Record (arg0) : SharedTypes.Type.kind)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Variant") = tag ->
        (match (fun list ->
                  match Js.Json.classify list with
                  | ((JSONArray (items))[@explicit_arity ]) ->
                      let transformer =
                        deserialize_SharedTypes__Type__Constructor__t in
                      let rec loop items =
                        match items with
                        | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                        | one::rest ->
                            (match transformer one with
                             | ((Belt.Result.Error
                                 (error))[@explicit_arity ]) ->
                                 ((Belt.Result.Error (error))
                                 [@explicit_arity ])
                             | ((Ok (value))[@explicit_arity ]) ->
                                 (match loop rest with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (rest))[@explicit_arity ]) ->
                                      ((Ok ((value :: rest)))
                                      [@explicit_arity ]))) in
                      loop (Belt.List.fromArray items)
                  | _ ->
                      ((Belt.Result.Error
                          ((("expected an array")
                            [@reason.raw_literal "expected an array"])))
                      [@explicit_arity ])) arg0
         with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Variant (arg0) : SharedTypes.Type.kind)
         | Error error -> Error error)
    | _ -> Error "Expected an array"
and (deserialize_Belt__Belt_HashMapInt____key :
  Json.t -> (Belt__Belt_HashMapInt.key, string) Belt.Result.t) =
  fun value ->
    (fun number ->
       match number with
       | ((Json.Number (number))[@explicit_arity ]) ->
           ((Belt.Result.Ok ((int_of_float number)))[@explicit_arity ])
       | _ ->
           ((Error
               ((("Expected a float")
                 [@reason.raw_literal "Expected a float"])))
           [@explicit_arity ])) value
and deserialize_Belt__Belt_internalBuckets____bucket :
  'arg0 'arg1 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      (Json.t -> ('arg1, string) Belt.Result.t) ->
        Json.t ->
          (('arg0, 'arg1) Belt__Belt_internalBuckets.bucket, string)
            Belt.Result.t
  =
  fun aTransformer ->
    fun bTransformer ->
      TransformHelpers.deserialize_Belt__Belt_internalBuckets____bucket
        aTransformer bTransformer
and (deserialize_TypeMap__DigTypes____typeSource :
  Json.t -> (TypeMap__DigTypes.typeSource, string) Belt.Result.t) =
  fun constructor ->
    match constructor with
    | Json.Array [|tag;arg0|] when (Json.String "Builtin") = tag ->
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
             Belt.Result.Ok (Builtin (arg0) : TypeMap__DigTypes.typeSource)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Public") = tag ->
        (match deserialize_TypeMap__DigTypes____reference arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Public (arg0) : TypeMap__DigTypes.typeSource)
         | Error error -> Error error)
    | Json.Array [|tag|] when (Json.String "NotFound") = tag ->
        Belt.Result.Ok (NotFound : TypeMap__DigTypes.typeSource)
    | _ -> Error "Expected an array"
and deserialize_Belt__Belt_internalBuckets____t :
  'arg0 'arg1 'arg2 'arg3 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      (Json.t -> ('arg1, string) Belt.Result.t) ->
        (Json.t -> ('arg2, string) Belt.Result.t) ->
          (Json.t -> ('arg3, string) Belt.Result.t) ->
            Json.t ->
              (('arg0, 'arg1, 'arg2, 'arg3) Belt__Belt_internalBuckets.t,
                string) Belt.Result.t
  =
  fun hashTransformer ->
    fun eqTransformer ->
      fun aTransformer ->
        fun bTransformer ->
          fun value ->
            (deserialize_Belt__Belt_internalBucketsType____container
               hashTransformer eqTransformer
               (deserialize_Belt__Belt_internalBuckets____bucket aTransformer
                  bTransformer)) value
and (deserialize_SharedTypes__Value__t :
  Json.t -> (SharedTypes.Value.t, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "recursive"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "recursive")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (fun bool ->
                       match bool with
                       | Json.True -> ((Belt.Result.Ok (true))
                           [@explicit_arity ])
                       | Json.False -> ((Belt.Result.Ok (false))
                           [@explicit_arity ])
                       | _ ->
                           ((Belt.Result.Error
                               ((("Expected a bool")
                                 [@reason.raw_literal "Expected a bool"])))
                           [@explicit_arity ])) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_recursive))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "typ"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^ "typ")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_SharedTypes____flexibleType json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_typ))[@explicit_arity ]) ->
                            Belt.Result.Ok
                              { typ = attr_typ; recursive = attr_recursive }))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_Lexing____position :
  Json.t -> (Lexing.position, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "pos_cnum"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "pos_cnum")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (fun number ->
                       match number with
                       | ((Json.Number (number))[@explicit_arity ]) ->
                           ((Belt.Result.Ok ((int_of_float number)))
                           [@explicit_arity ])
                       | _ ->
                           ((Error
                               ((("Expected a float")
                                 [@reason.raw_literal "Expected a float"])))
                           [@explicit_arity ])) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_pos_cnum))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "pos_bol"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "pos_bol")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (fun number ->
                                 match number with
                                 | ((Json.Number (number))[@explicit_arity ])
                                     ->
                                     ((Belt.Result.Ok ((int_of_float number)))
                                     [@explicit_arity ])
                                 | _ ->
                                     ((Error
                                         ((("Expected a float")
                                           [@reason.raw_literal
                                             "Expected a float"])))
                                     [@explicit_arity ])) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_pos_bol))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "pos_lnum"),
                                     (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "pos_lnum")))
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
                                  | ((Ok (attr_pos_lnum))[@explicit_arity ])
                                      ->
                                      (match ((Belt.List.getAssoc items
                                                 "pos_fname"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "pos_fname")))
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
                                                             ((("epected a string")
                                                               [@reason.raw_literal
                                                                 "epected a string"])))
                                                         [@explicit_arity ]))
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_pos_fname))[@explicit_arity
                                                                   ])
                                                ->
                                                Belt.Result.Ok
                                                  {
                                                    pos_fname =
                                                      attr_pos_fname;
                                                    pos_lnum = attr_pos_lnum;
                                                    pos_bol = attr_pos_bol;
                                                    pos_cnum = attr_pos_cnum
                                                  }))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_SharedTypes__Type__Constructor__t :
  Json.t -> (SharedTypes.Type.Constructor.t, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "res"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "res")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match ((fun transformer ->
                        fun option ->
                          match option with
                          | Json.Null -> ((Belt.Result.Ok (None))
                              [@explicit_arity ])
                          | _ ->
                              (match transformer option with
                               | ((Belt.Result.Error
                                   (error))[@explicit_arity ]) ->
                                   ((Belt.Result.Error (error))
                                   [@explicit_arity ])
                               | ((Ok (value))[@explicit_arity ]) ->
                                   ((Ok (((Some (value))[@explicit_arity ])))
                                   [@explicit_arity ])))
                       deserialize_SharedTypes____flexibleType) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_res))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "args"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "args")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (fun list ->
                                 match Js.Json.classify list with
                                 | ((JSONArray (items))[@explicit_arity ]) ->
                                     let transformer json =
                                       match Js.Json.classify json with
                                       | ((JSONArray
                                           ([|arg0;arg1|]))[@explicit_arity ])
                                           ->
                                           (match deserialize_Location____t
                                                    arg1
                                            with
                                            | Belt.Result.Ok arg1 ->
                                                (match deserialize_SharedTypes____flexibleType
                                                         arg0
                                                 with
                                                 | Belt.Result.Ok arg0 ->
                                                     Belt.Result.Ok
                                                       (arg0, arg1)
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
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
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
                                                 | ((Ok
                                                     (rest))[@explicit_arity
                                                              ])
                                                     ->
                                                     ((Ok ((value :: rest)))
                                                     [@explicit_arity ]))) in
                                     loop (Belt.List.fromArray items)
                                 | _ ->
                                     ((Belt.Result.Error
                                         ((("expected an array")
                                           [@reason.raw_literal
                                             "expected an array"])))
                                     [@explicit_arity ])) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_args))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "name"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "name")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (deserialize_Location____loc
                                           (fun string ->
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
                                                  [@explicit_arity ]))) json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_name))[@explicit_arity ]) ->
                                      (match ((Belt.List.getAssoc items
                                                 "stamp"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "stamp")))
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
                                                             ((("Expected a float")
                                                               [@reason.raw_literal
                                                                 "Expected a float"])))
                                                         [@explicit_arity ]))
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_stamp))[@explicit_arity
                                                               ])
                                                ->
                                                Belt.Result.Ok
                                                  {
                                                    stamp = attr_stamp;
                                                    name = attr_name;
                                                    args = attr_args;
                                                    res = attr_res
                                                  }))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_SharedTypes____stamps :
  Json.t -> (SharedTypes.stamps, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "constructors"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "constructors")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (deserialize_SharedTypes____stampMap
                       (deserialize_SharedTypes____declared
                          deserialize_SharedTypes__Type__Constructor__t))
                      json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_constructors))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "moduleTypes"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "moduleTypes")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (deserialize_SharedTypes____stampMap
                                 (deserialize_SharedTypes____declared
                                    deserialize_SharedTypes__Module__kind))
                                json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_moduleTypes))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "modules"),
                                     (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "modules")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (deserialize_SharedTypes____stampMap
                                           (deserialize_SharedTypes____declared
                                              deserialize_SharedTypes__Module__kind))
                                          json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_modules))[@explicit_arity ])
                                      ->
                                      (match ((Belt.List.getAssoc items
                                                 "values"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "values")))
                                           [@explicit_arity ])
                                       | ((Some (json))[@explicit_arity ]) ->
                                           (match (deserialize_SharedTypes____stampMap
                                                     (deserialize_SharedTypes____declared
                                                        deserialize_SharedTypes__Value__t))
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_values))[@explicit_arity
                                                                ])
                                                ->
                                                (match ((Belt.List.getAssoc
                                                           items "types"),
                                                         (=))
                                                 with
                                                 | None ->
                                                     ((Belt.Result.Error
                                                         (((("No attribute ")
                                                             [@reason.raw_literal
                                                               "No attribute "])
                                                             ^ "types")))
                                                     [@explicit_arity ])
                                                 | ((Some
                                                     (json))[@explicit_arity
                                                              ])
                                                     ->
                                                     (match (deserialize_SharedTypes____stampMap
                                                               (deserialize_SharedTypes____declared
                                                                  deserialize_SharedTypes__Type__t))
                                                              json
                                                      with
                                                      | ((Belt.Result.Error
                                                          (error))[@explicit_arity
                                                                    ])
                                                          ->
                                                          ((Belt.Result.Error
                                                              (error))
                                                          [@explicit_arity ])
                                                      | ((Ok
                                                          (attr_types))
                                                          [@explicit_arity ])
                                                          ->
                                                          Belt.Result.Ok
                                                            {
                                                              types =
                                                                attr_types;
                                                              values =
                                                                attr_values;
                                                              modules =
                                                                attr_modules;
                                                              moduleTypes =
                                                                attr_moduleTypes;
                                                              constructors =
                                                                attr_constructors
                                                            }))))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_SharedTypes__Module__kind :
  Json.t -> (SharedTypes.Module.kind, string) Belt.Result.t) =
  fun constructor ->
    match constructor with
    | Json.Array [|tag;arg0|] when (Json.String "Ident") = tag ->
        (match deserialize_Path____t arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Ident (arg0) : SharedTypes.Module.kind)
         | Error error -> Error error)
    | Json.Array [|tag;arg0|] when (Json.String "Structure") = tag ->
        (match deserialize_SharedTypes__Module__contents arg0 with
         | Belt.Result.Ok arg0 ->
             Belt.Result.Ok (Structure (arg0) : SharedTypes.Module.kind)
         | Error error -> Error error)
    | _ -> Error "Expected an array"
and (deserialize_Query____queryEnv :
  Json.t -> (Query.queryEnv, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "exported"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "exported")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match deserialize_SharedTypes__Module__exported json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_exported))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "file"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "file")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_SharedTypes____file json with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_file))[@explicit_arity ]) ->
                            Belt.Result.Ok
                              { file = attr_file; exported = attr_exported }))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and deserialize_SharedTypes____declared :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 SharedTypes.declared, string) Belt.Result.t
  =
  fun tTransformer ->
    fun record ->
      match record with
      | ((Json.Object (items))[@explicit_arity ]) ->
          (match ((Belt.List.getAssoc items "contents"), (=)) with
           | None ->
               ((Belt.Result.Error
                   (((("No attribute ")[@reason.raw_literal "No attribute "])
                       ^ "contents")))
               [@explicit_arity ])
           | ((Some (json))[@explicit_arity ]) ->
               (match tTransformer json with
                | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                    ((Belt.Result.Error (error))[@explicit_arity ])
                | ((Ok (attr_contents))[@explicit_arity ]) ->
                    (match ((Belt.List.getAssoc items "docstring"), (=)) with
                     | None ->
                         ((Belt.Result.Error
                             (((("No attribute ")
                                 [@reason.raw_literal "No attribute "]) ^
                                 "docstring")))
                         [@explicit_arity ])
                     | ((Some (json))[@explicit_arity ]) ->
                         (match ((fun transformer ->
                                    fun option ->
                                      match option with
                                      | Json.Null -> ((Belt.Result.Ok (None))
                                          [@explicit_arity ])
                                      | _ ->
                                          (match transformer option with
                                           | ((Belt.Result.Error
                                               (error))[@explicit_arity ]) ->
                                               ((Belt.Result.Error (error))
                                               [@explicit_arity ])
                                           | ((Ok (value))[@explicit_arity ])
                                               ->
                                               ((Ok
                                                   (((Some (value))
                                                     [@explicit_arity ])))
                                               [@explicit_arity ])))
                                   (fun string ->
                                      match string with
                                      | ((Json.String
                                          (string))[@explicit_arity ]) ->
                                          ((Belt.Result.Ok (string))
                                          [@explicit_arity ])
                                      | _ ->
                                          ((Error
                                              ((("epected a string")
                                                [@reason.raw_literal
                                                  "epected a string"])))
                                          [@explicit_arity ]))) json
                          with
                          | ((Belt.Result.Error (error))[@explicit_arity ])
                              -> ((Belt.Result.Error (error))
                              [@explicit_arity ])
                          | ((Ok (attr_docstring))[@explicit_arity ]) ->
                              (match ((Belt.List.getAssoc items "exported"),
                                       (=))
                               with
                               | None ->
                                   ((Belt.Result.Error
                                       (((("No attribute ")
                                           [@reason.raw_literal
                                             "No attribute "])
                                           ^ "exported")))
                                   [@explicit_arity ])
                               | ((Some (json))[@explicit_arity ]) ->
                                   (match (fun bool ->
                                             match bool with
                                             | Json.True ->
                                                 ((Belt.Result.Ok (true))
                                                 [@explicit_arity ])
                                             | Json.False ->
                                                 ((Belt.Result.Ok (false))
                                                 [@explicit_arity ])
                                             | _ ->
                                                 ((Belt.Result.Error
                                                     ((("Expected a bool")
                                                       [@reason.raw_literal
                                                         "Expected a bool"])))
                                                 [@explicit_arity ])) json
                                    with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Ok
                                        (attr_exported))[@explicit_arity ])
                                        ->
                                        (match ((Belt.List.getAssoc items
                                                   "modulePath"), (=))
                                         with
                                         | None ->
                                             ((Belt.Result.Error
                                                 (((("No attribute ")
                                                     [@reason.raw_literal
                                                       "No attribute "])
                                                     ^ "modulePath")))
                                             [@explicit_arity ])
                                         | ((Some (json))[@explicit_arity ])
                                             ->
                                             (match deserialize_SharedTypes____visibilityPath
                                                      json
                                              with
                                              | ((Belt.Result.Error
                                                  (error))[@explicit_arity ])
                                                  ->
                                                  ((Belt.Result.Error (error))
                                                  [@explicit_arity ])
                                              | ((Ok
                                                  (attr_modulePath))[@explicit_arity
                                                                    ])
                                                  ->
                                                  (match ((Belt.List.getAssoc
                                                             items
                                                             "deprecated"),
                                                           (=))
                                                   with
                                                   | None ->
                                                       ((Belt.Result.Error
                                                           (((("No attribute ")
                                                               [@reason.raw_literal
                                                                 "No attribute "])
                                                               ^ "deprecated")))
                                                       [@explicit_arity ])
                                                   | ((Some
                                                       (json))[@explicit_arity
                                                                ])
                                                       ->
                                                       (match ((fun
                                                                  transformer
                                                                  ->
                                                                  fun option
                                                                    ->
                                                                    match option
                                                                    with
                                                                    | 
                                                                    Json.Null
                                                                    ->
                                                                    ((Belt.Result.Ok
                                                                    (None))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    _ ->
                                                                    (match 
                                                                    transformer
                                                                    option
                                                                    with
                                                                    | 
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Ok
                                                                    (value))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Ok
                                                                    (((
                                                                    Some
                                                                    (value))
                                                                    [@explicit_arity
                                                                    ])))
                                                                    [@explicit_arity
                                                                    ])))
                                                                 (fun string
                                                                    ->
                                                                    match string
                                                                    with
                                                                    | 
                                                                    ((Json.String
                                                                    (string))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Ok
                                                                    (string))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    _ ->
                                                                    ((Error
                                                                    ((("epected a string")
                                                                    [@reason.raw_literal
                                                                    "epected a string"])))
                                                                    [@explicit_arity
                                                                    ]))) json
                                                        with
                                                        | ((Belt.Result.Error
                                                            (error))[@explicit_arity
                                                                    ])
                                                            ->
                                                            ((Belt.Result.Error
                                                                (error))
                                                            [@explicit_arity
                                                              ])
                                                        | ((Ok
                                                            (attr_deprecated))
                                                            [@explicit_arity
                                                              ])
                                                            ->
                                                            (match ((Belt.List.getAssoc
                                                                    items
                                                                    "stamp"),
                                                                    (=))
                                                             with
                                                             | None ->
                                                                 ((Belt.Result.Error
                                                                    (((("No attribute ")
                                                                    [@reason.raw_literal
                                                                    "No attribute "])
                                                                    ^ "stamp")))
                                                                 [@explicit_arity
                                                                   ])
                                                             | ((Some
                                                                 (json))
                                                                 [@explicit_arity
                                                                   ])
                                                                 ->
                                                                 (match 
                                                                    (fun
                                                                    number ->
                                                                    match number
                                                                    with
                                                                    | 
                                                                    ((Json.Number
                                                                    (number))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Ok
                                                                    ((int_of_float
                                                                    number)))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    _ ->
                                                                    ((Error
                                                                    ((("Expected a float")
                                                                    [@reason.raw_literal
                                                                    "Expected a float"])))
                                                                    [@explicit_arity
                                                                    ])) json
                                                                  with
                                                                  | ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ])
                                                                  | ((Ok
                                                                    (attr_stamp))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    ((Belt.List.getAssoc
                                                                    items
                                                                    "scopeLoc"),
                                                                    (=))
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    ((Belt.Result.Error
                                                                    (((("No attribute ")
                                                                    [@reason.raw_literal
                                                                    "No attribute "])
                                                                    ^
                                                                    "scopeLoc")))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Some
                                                                    (json))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    deserialize_Location____t
                                                                    json
                                                                    with
                                                                    | 
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Ok
                                                                    (attr_scopeLoc))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    ((Belt.List.getAssoc
                                                                    items
                                                                    "extentLoc"),
                                                                    (=))
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    ((Belt.Result.Error
                                                                    (((("No attribute ")
                                                                    [@reason.raw_literal
                                                                    "No attribute "])
                                                                    ^
                                                                    "extentLoc")))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Some
                                                                    (json))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    deserialize_Location____t
                                                                    json
                                                                    with
                                                                    | 
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Ok
                                                                    (attr_extentLoc))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    ((Belt.List.getAssoc
                                                                    items
                                                                    "name"),
                                                                    (=))
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    ((Belt.Result.Error
                                                                    (((("No attribute ")
                                                                    [@reason.raw_literal
                                                                    "No attribute "])
                                                                    ^ "name")))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Some
                                                                    (json))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    (deserialize_Location____loc
                                                                    (fun
                                                                    string ->
                                                                    match string
                                                                    with
                                                                    | 
                                                                    ((Json.String
                                                                    (string))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Ok
                                                                    (string))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    _ ->
                                                                    ((Error
                                                                    ((("epected a string")
                                                                    [@reason.raw_literal
                                                                    "epected a string"])))
                                                                    [@explicit_arity
                                                                    ]))) json
                                                                    with
                                                                    | 
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Ok
                                                                    (attr_name))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    Belt.Result.Ok
                                                                    {
                                                                    name =
                                                                    attr_name;
                                                                    extentLoc
                                                                    =
                                                                    attr_extentLoc;
                                                                    scopeLoc
                                                                    =
                                                                    attr_scopeLoc;
                                                                    stamp =
                                                                    attr_stamp;
                                                                    deprecated
                                                                    =
                                                                    attr_deprecated;
                                                                    modulePath
                                                                    =
                                                                    attr_modulePath;
                                                                    exported
                                                                    =
                                                                    attr_exported;
                                                                    docstring
                                                                    =
                                                                    attr_docstring;
                                                                    contents
                                                                    =
                                                                    attr_contents
                                                                    }))))))))))))))))))
      | _ ->
          ((Belt.Result.Error
              ((("Expected an object")
                [@reason.raw_literal "Expected an object"])))
          [@explicit_arity ])
and (deserialize_SharedTypes____file :
  Json.t -> (SharedTypes.file, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "contents"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "contents")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match deserialize_SharedTypes__Module__contents json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_contents))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "moduleName"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "moduleName")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (fun string ->
                                 match string with
                                 | ((Json.String (string))[@explicit_arity ])
                                     -> ((Belt.Result.Ok (string))
                                     [@explicit_arity ])
                                 | _ ->
                                     ((Error
                                         ((("epected a string")
                                           [@reason.raw_literal
                                             "epected a string"])))
                                     [@explicit_arity ])) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_moduleName))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "stamps"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "stamps")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match deserialize_SharedTypes____stamps
                                          json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_stamps))[@explicit_arity ]) ->
                                      (match ((Belt.List.getAssoc items
                                                 "docstring"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "docstring")))
                                           [@explicit_arity ])
                                       | ((Some (json))[@explicit_arity ]) ->
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
                                                             | ((Ok
                                                                 (value))
                                                                 [@explicit_arity
                                                                   ])
                                                                 ->
                                                                 ((Ok
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
                                                                ((("epected a string")
                                                                  [@reason.raw_literal
                                                                    "epected a string"])))
                                                            [@explicit_arity
                                                              ]))) json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_docstring))[@explicit_arity
                                                                   ])
                                                ->
                                                (match ((Belt.List.getAssoc
                                                           items "uri"), (=))
                                                 with
                                                 | None ->
                                                     ((Belt.Result.Error
                                                         (((("No attribute ")
                                                             [@reason.raw_literal
                                                               "No attribute "])
                                                             ^ "uri")))
                                                     [@explicit_arity ])
                                                 | ((Some
                                                     (json))[@explicit_arity
                                                              ])
                                                     ->
                                                     (match (fun string ->
                                                               match string
                                                               with
                                                               | ((Json.String
                                                                   (string))
                                                                   [@explicit_arity
                                                                    ])
                                                                   ->
                                                                   ((
                                                                   Belt.Result.Ok
                                                                    (string))
                                                                   [@explicit_arity
                                                                    ])
                                                               | _ ->
                                                                   ((
                                                                   Error
                                                                    ((("epected a string")
                                                                    [@reason.raw_literal
                                                                    "epected a string"])))
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
                                                      | ((Ok
                                                          (attr_uri))
                                                          [@explicit_arity ])
                                                          ->
                                                          Belt.Result.Ok
                                                            {
                                                              uri = attr_uri;
                                                              docstring =
                                                                attr_docstring;
                                                              stamps =
                                                                attr_stamps;
                                                              moduleName =
                                                                attr_moduleName;
                                                              contents =
                                                                attr_contents
                                                            }))))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_SharedTypes__Module__exported :
  Json.t -> (SharedTypes.Module.exported, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "moduleTypes"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "moduleTypes")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match deserialize_SharedTypes____namedStampMap json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_moduleTypes))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "modules"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "modules")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_SharedTypes____namedStampMap json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_modules))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "values"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "values")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match deserialize_SharedTypes____namedStampMap
                                          json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_values))[@explicit_arity ]) ->
                                      (match ((Belt.List.getAssoc items
                                                 "types"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "types")))
                                           [@explicit_arity ])
                                       | ((Some (json))[@explicit_arity ]) ->
                                           (match deserialize_SharedTypes____namedStampMap
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_types))[@explicit_arity
                                                               ])
                                                ->
                                                Belt.Result.Ok
                                                  {
                                                    types = attr_types;
                                                    values = attr_values;
                                                    modules = attr_modules;
                                                    moduleTypes =
                                                      attr_moduleTypes
                                                  }))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_Path____t : Json.t -> (Path.t, string) Belt.Result.t) =
  fun constructor ->
    match constructor with
    | Json.Array [|tag;arg0|] when (Json.String "Pident") = tag ->
        (match deserialize_Ident____t arg0 with
         | Belt.Result.Ok arg0 -> Belt.Result.Ok (Pident (arg0) : Path.t)
         | Error error -> Error error)
    | Json.Array [|tag;arg0;arg1;arg2|] when (Json.String "Pdot") = tag ->
        (match (fun number ->
                  match number with
                  | ((Json.Number (number))[@explicit_arity ]) ->
                      ((Belt.Result.Ok ((int_of_float number)))
                      [@explicit_arity ])
                  | _ ->
                      ((Error
                          ((("Expected a float")
                            [@reason.raw_literal "Expected a float"])))
                      [@explicit_arity ])) arg2
         with
         | Belt.Result.Ok arg2 ->
             (match (fun string ->
                       match string with
                       | ((Json.String (string))[@explicit_arity ]) ->
                           ((Belt.Result.Ok (string))[@explicit_arity ])
                       | _ ->
                           ((Error
                               ((("epected a string")
                                 [@reason.raw_literal "epected a string"])))
                           [@explicit_arity ])) arg1
              with
              | Belt.Result.Ok arg1 ->
                  (match deserialize_Path____t arg0 with
                   | Belt.Result.Ok arg0 ->
                       Belt.Result.Ok (Pdot (arg0, arg1, arg2) : Path.t)
                   | Error error -> Error error)
              | Error error -> Error error)
         | Error error -> Error error)
    | Json.Array [|tag;arg0;arg1|] when (Json.String "Papply") = tag ->
        (match deserialize_Path____t arg1 with
         | Belt.Result.Ok arg1 ->
             (match deserialize_Path____t arg0 with
              | Belt.Result.Ok arg0 ->
                  Belt.Result.Ok (Papply (arg0, arg1) : Path.t)
              | Error error -> Error error)
         | Error error -> Error error)
    | _ -> Error "Expected an array"
and (deserialize_SharedTypes__Type__t :
  Json.t -> (SharedTypes.Type.t, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "typ"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "typ")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match deserialize_SharedTypes____flexibleDeclaration json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_typ))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "params"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "params")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (fun list ->
                                 match Js.Json.classify list with
                                 | ((JSONArray (items))[@explicit_arity ]) ->
                                     let transformer json =
                                       match Js.Json.classify json with
                                       | ((JSONArray
                                           ([|arg0;arg1|]))[@explicit_arity ])
                                           ->
                                           (match deserialize_Location____t
                                                    arg1
                                            with
                                            | Belt.Result.Ok arg1 ->
                                                (match deserialize_SharedTypes____flexibleType
                                                         arg0
                                                 with
                                                 | Belt.Result.Ok arg0 ->
                                                     Belt.Result.Ok
                                                       (arg0, arg1)
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
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
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
                                                 | ((Ok
                                                     (rest))[@explicit_arity
                                                              ])
                                                     ->
                                                     ((Ok ((value :: rest)))
                                                     [@explicit_arity ]))) in
                                     loop (Belt.List.fromArray items)
                                 | _ ->
                                     ((Belt.Result.Error
                                         ((("expected an array")
                                           [@reason.raw_literal
                                             "expected an array"])))
                                     [@explicit_arity ])) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_params))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "kind"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "kind")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match deserialize_SharedTypes__Type__kind
                                          json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_kind))[@explicit_arity ]) ->
                                      Belt.Result.Ok
                                        {
                                          kind = attr_kind;
                                          params = attr_params;
                                          typ = attr_typ
                                        }))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_SharedTypes____flexibleType :
  Json.t -> (SharedTypes.flexibleType, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "asSimpleType"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "asSimpleType")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (failwith "not impl expr") json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_asSimpleType))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "getArguments"), (=))
                   with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "getArguments")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (failwith "not impl expr") json with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_getArguments))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items
                                       "getConstructorPath"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "getConstructorPath")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (failwith "not impl expr") json with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok
                                      (attr_getConstructorPath))[@explicit_arity
                                                                  ])
                                      ->
                                      (match ((Belt.List.getAssoc items
                                                 "variableKind"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "variableKind")))
                                           [@explicit_arity ])
                                       | ((Some (json))[@explicit_arity ]) ->
                                           (match deserialize_SharedTypes____kinds
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_variableKind))[@explicit_arity
                                                                    ])
                                                ->
                                                (match ((Belt.List.getAssoc
                                                           items "toString"),
                                                         (=))
                                                 with
                                                 | None ->
                                                     ((Belt.Result.Error
                                                         (((("No attribute ")
                                                             [@reason.raw_literal
                                                               "No attribute "])
                                                             ^ "toString")))
                                                     [@explicit_arity ])
                                                 | ((Some
                                                     (json))[@explicit_arity
                                                              ])
                                                     ->
                                                     (match (failwith
                                                               "not impl expr")
                                                              json
                                                      with
                                                      | ((Belt.Result.Error
                                                          (error))[@explicit_arity
                                                                    ])
                                                          ->
                                                          ((Belt.Result.Error
                                                              (error))
                                                          [@explicit_arity ])
                                                      | ((Ok
                                                          (attr_toString))
                                                          [@explicit_arity ])
                                                          ->
                                                          Belt.Result.Ok
                                                            {
                                                              toString =
                                                                attr_toString;
                                                              variableKind =
                                                                attr_variableKind;
                                                              getConstructorPath
                                                                =
                                                                attr_getConstructorPath;
                                                              getArguments =
                                                                attr_getArguments;
                                                              asSimpleType =
                                                                attr_asSimpleType
                                                            }))))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and (deserialize_Location____t :
  Json.t -> (Location.t, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "loc_ghost"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "loc_ghost")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (fun bool ->
                       match bool with
                       | Json.True -> ((Belt.Result.Ok (true))
                           [@explicit_arity ])
                       | Json.False -> ((Belt.Result.Ok (false))
                           [@explicit_arity ])
                       | _ ->
                           ((Belt.Result.Error
                               ((("Expected a bool")
                                 [@reason.raw_literal "Expected a bool"])))
                           [@explicit_arity ])) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_loc_ghost))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "loc_end"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "loc_end")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_Lexing____position json with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_loc_end))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "loc_start"),
                                     (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "loc_start")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match deserialize_Lexing____position json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_loc_start))[@explicit_arity ])
                                      ->
                                      Belt.Result.Ok
                                        {
                                          loc_start = attr_loc_start;
                                          loc_end = attr_loc_end;
                                          loc_ghost = attr_loc_ghost
                                        }))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and deserialize_Hashtbl____t :
  'arg0 'arg1 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      (Json.t -> ('arg1, string) Belt.Result.t) ->
        Json.t -> (('arg0, 'arg1) Hashtbl.t, string) Belt.Result.t
  =
  fun aTransformer ->
    fun bTransformer ->
      TransformHelpers.deserialize_Hashtbl____t aTransformer bTransformer
and (deserialize_TypeMap__DigTypes____reference :
  Json.t -> (TypeMap__DigTypes.reference, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "env"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "env")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match deserialize_Query____queryEnv json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_env))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "name"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "name")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (fun string ->
                                 match string with
                                 | ((Json.String (string))[@explicit_arity ])
                                     -> ((Belt.Result.Ok (string))
                                     [@explicit_arity ])
                                 | _ ->
                                     ((Error
                                         ((("epected a string")
                                           [@reason.raw_literal
                                             "epected a string"])))
                                     [@explicit_arity ])) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_name))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "modulePath"),
                                     (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "modulePath")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (fun list ->
                                           match Js.Json.classify list with
                                           | ((JSONArray
                                               (items))[@explicit_arity ]) ->
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
                                                         ((("epected a string")
                                                           [@reason.raw_literal
                                                             "epected a string"])))
                                                     [@explicit_arity ]) in
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
                                                          [@explicit_arity ])
                                                      | ((Ok
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
                                                           | ((Ok
                                                               (rest))
                                                               [@explicit_arity
                                                                 ])
                                                               ->
                                                               ((Ok
                                                                   ((value ::
                                                                    rest)))
                                                               [@explicit_arity
                                                                 ]))) in
                                               loop
                                                 (Belt.List.fromArray items)
                                           | _ ->
                                               ((Belt.Result.Error
                                                   ((("expected an array")
                                                     [@reason.raw_literal
                                                       "expected an array"])))
                                               [@explicit_arity ])) json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok
                                      (attr_modulePath))[@explicit_arity ])
                                      ->
                                      (match ((Belt.List.getAssoc items
                                                 "declared"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "declared")))
                                           [@explicit_arity ])
                                       | ((Some (json))[@explicit_arity ]) ->
                                           (match (deserialize_SharedTypes____declared
                                                     deserialize_SharedTypes__Type__t)
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_declared))[@explicit_arity
                                                                  ])
                                                ->
                                                (match ((Belt.List.getAssoc
                                                           items "moduleName"),
                                                         (=))
                                                 with
                                                 | None ->
                                                     ((Belt.Result.Error
                                                         (((("No attribute ")
                                                             [@reason.raw_literal
                                                               "No attribute "])
                                                             ^ "moduleName")))
                                                     [@explicit_arity ])
                                                 | ((Some
                                                     (json))[@explicit_arity
                                                              ])
                                                     ->
                                                     (match (fun string ->
                                                               match string
                                                               with
                                                               | ((Json.String
                                                                   (string))
                                                                   [@explicit_arity
                                                                    ])
                                                                   ->
                                                                   ((
                                                                   Belt.Result.Ok
                                                                    (string))
                                                                   [@explicit_arity
                                                                    ])
                                                               | _ ->
                                                                   ((
                                                                   Error
                                                                    ((("epected a string")
                                                                    [@reason.raw_literal
                                                                    "epected a string"])))
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
                                                      | ((Ok
                                                          (attr_moduleName))
                                                          [@explicit_arity ])
                                                          ->
                                                          (match ((Belt.List.getAssoc
                                                                    items
                                                                    "uri"),
                                                                   (=))
                                                           with
                                                           | None ->
                                                               ((Belt.Result.Error
                                                                   (((("No attribute ")
                                                                    [@reason.raw_literal
                                                                    "No attribute "])
                                                                    ^ "uri")))
                                                               [@explicit_arity
                                                                 ])
                                                           | ((Some
                                                               (json))
                                                               [@explicit_arity
                                                                 ])
                                                               ->
                                                               (match 
                                                                  (fun string
                                                                    ->
                                                                    match string
                                                                    with
                                                                    | 
                                                                    ((Json.String
                                                                    (string))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Ok
                                                                    (string))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    _ ->
                                                                    ((Error
                                                                    ((("epected a string")
                                                                    [@reason.raw_literal
                                                                    "epected a string"])))
                                                                    [@explicit_arity
                                                                    ])) json
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
                                                                | ((Ok
                                                                    (attr_uri))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    Belt.Result.Ok
                                                                    {
                                                                    uri =
                                                                    attr_uri;
                                                                    moduleName
                                                                    =
                                                                    attr_moduleName;
                                                                    declared
                                                                    =
                                                                    attr_declared;
                                                                    modulePath
                                                                    =
                                                                    attr_modulePath;
                                                                    name =
                                                                    attr_name;
                                                                    env =
                                                                    attr_env
                                                                    }))))))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and deserialize_Belt__Belt_internalBucketsType____container :
  'arg0 'arg1 'arg2 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      (Json.t -> ('arg1, string) Belt.Result.t) ->
        (Json.t -> ('arg2, string) Belt.Result.t) ->
          Json.t ->
            (('arg0, 'arg1, 'arg2) Belt__Belt_internalBucketsType.container,
              string) Belt.Result.t
  =
  fun hashTransformer ->
    fun eqTransformer ->
      fun cTransformer ->
        TransformHelpers.deserialize_Belt__Belt_internalBucketsType____container
          hashTransformer eqTransformer cTransformer
and deserialize_Belt__Belt_HashMapInt____t :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 Belt__Belt_HashMapInt.t, string) Belt.Result.t
  =
  fun bTransformer ->
    fun value ->
      (deserialize_Belt__Belt_internalBuckets____t
         (fun _ -> failwith "Not found") (fun _ -> failwith "Not found")
         deserialize_Belt__Belt_HashMapInt____key bTransformer) value
and (deserialize_SharedTypes__Type__Attribute__t :
  Json.t -> (SharedTypes.Type.Attribute.t, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "typLoc"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "typLoc")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match deserialize_Location____t json with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_typLoc))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "typ"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^ "typ")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match deserialize_SharedTypes____flexibleType json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_typ))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "name"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "name")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (deserialize_Location____loc
                                           (fun string ->
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
                                                  [@explicit_arity ]))) json
                                  with
                                  | ((Belt.Result.Error
                                      (error))[@explicit_arity ]) ->
                                      ((Belt.Result.Error (error))
                                      [@explicit_arity ])
                                  | ((Ok (attr_name))[@explicit_arity ]) ->
                                      (match ((Belt.List.getAssoc items
                                                 "stamp"), (=))
                                       with
                                       | None ->
                                           ((Belt.Result.Error
                                               (((("No attribute ")
                                                   [@reason.raw_literal
                                                     "No attribute "])
                                                   ^ "stamp")))
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
                                                             ((("Expected a float")
                                                               [@reason.raw_literal
                                                                 "Expected a float"])))
                                                         [@explicit_arity ]))
                                                    json
                                            with
                                            | ((Belt.Result.Error
                                                (error))[@explicit_arity ])
                                                ->
                                                ((Belt.Result.Error (error))
                                                [@explicit_arity ])
                                            | ((Ok
                                                (attr_stamp))[@explicit_arity
                                                               ])
                                                ->
                                                Belt.Result.Ok
                                                  {
                                                    stamp = attr_stamp;
                                                    name = attr_name;
                                                    typ = attr_typ;
                                                    typLoc = attr_typLoc
                                                  }))))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
and deserialize_SharedTypes__SimpleType__body :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 SharedTypes.SimpleType.body, string) Belt.Result.t
  =
  fun sourceTransformer ->
    fun constructor ->
      match constructor with
      | Json.Array [|tag|] when (Json.String "Open") = tag ->
          Belt.Result.Ok (Open : 'arg0 SharedTypes.SimpleType.body)
      | Json.Array [|tag|] when (Json.String "Abstract") = tag ->
          Belt.Result.Ok (Abstract : 'arg0 SharedTypes.SimpleType.body)
      | Json.Array [|tag;arg0|] when (Json.String "Expr") = tag ->
          (match (deserialize_SharedTypes__SimpleType__expr sourceTransformer)
                   arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Expr (arg0) : 'arg0 SharedTypes.SimpleType.body)
           | Error error -> Error error)
      | Json.Array [|tag;arg0|] when (Json.String "Record") = tag ->
          (match (fun list ->
                    match Js.Json.classify list with
                    | ((JSONArray (items))[@explicit_arity ]) ->
                        let transformer json =
                          match Js.Json.classify json with
                          | ((JSONArray ([|arg0;arg1|]))[@explicit_arity ])
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
                               | ((Ok (value))[@explicit_arity ]) ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Ok (rest))[@explicit_arity ]) ->
                                        ((Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop (Belt.List.fromArray items)
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
                        [@explicit_arity ])) arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Record (arg0) : 'arg0 SharedTypes.SimpleType.body)
           | Error error -> Error error)
      | Json.Array [|tag;arg0|] when (Json.String "Variant") = tag ->
          (match (fun list ->
                    match Js.Json.classify list with
                    | ((JSONArray (items))[@explicit_arity ]) ->
                        let transformer json =
                          match Js.Json.classify json with
                          | ((JSONArray
                              ([|arg0;arg1;arg2|]))[@explicit_arity ]) ->
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
                                                | ((Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Ok
                                                        (((Some (value))
                                                          [@explicit_arity ])))
                                                    [@explicit_arity ])))
                                        (deserialize_SharedTypes__SimpleType__expr
                                           sourceTransformer)) arg2
                               with
                               | Belt.Result.Ok arg2 ->
                                   (match (fun list ->
                                             match Js.Json.classify list with
                                             | ((JSONArray
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
                                                        | ((Ok
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
                                                             | ((Ok
                                                                 (rest))
                                                                 [@explicit_arity
                                                                   ])
                                                                 ->
                                                                 ((Ok
                                                                    ((value
                                                                    :: rest)))
                                                                 [@explicit_arity
                                                                   ]))) in
                                                 loop
                                                   (Belt.List.fromArray items)
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
                               | ((Ok (value))[@explicit_arity ]) ->
                                   (match loop rest with
                                    | ((Belt.Result.Error
                                        (error))[@explicit_arity ]) ->
                                        ((Belt.Result.Error (error))
                                        [@explicit_arity ])
                                    | ((Ok (rest))[@explicit_arity ]) ->
                                        ((Ok ((value :: rest)))
                                        [@explicit_arity ]))) in
                        loop (Belt.List.fromArray items)
                    | _ ->
                        ((Belt.Result.Error
                            ((("expected an array")
                              [@reason.raw_literal "expected an array"])))
                        [@explicit_arity ])) arg0
           with
           | Belt.Result.Ok arg0 ->
               Belt.Result.Ok
                 (Variant (arg0) : 'arg0 SharedTypes.SimpleType.body)
           | Error error -> Error error)
      | _ -> Error "Expected an array"
and deserialize_SharedTypes____namedMap :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 SharedTypes.namedMap, string) Belt.Result.t
  =
  fun tTransformer ->
    fun value ->
      (deserialize_Hashtbl____t
         (fun string ->
            match string with
            | ((Json.String (string))[@explicit_arity ]) ->
                ((Belt.Result.Ok (string))[@explicit_arity ])
            | _ ->
                ((Error
                    ((("epected a string")
                      [@reason.raw_literal "epected a string"])))
                [@explicit_arity ])) tTransformer) value
and (deserialize_SharedTypes____namedStampMap :
  Json.t -> (SharedTypes.namedStampMap, string) Belt.Result.t) =
  fun value ->
    (deserialize_SharedTypes____namedMap
       (fun number ->
          match number with
          | ((Json.Number (number))[@explicit_arity ]) ->
              ((Belt.Result.Ok ((int_of_float number)))[@explicit_arity ])
          | _ ->
              ((Error
                  ((("Expected a float")
                    [@reason.raw_literal "Expected a float"])))
              [@explicit_arity ]))) value
and (deserialize_TypeMap__DigTypes____lockfile :
  Json.t -> (TypeMap__DigTypes.lockfile, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "current"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "current")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (fun list ->
                       match Js.Json.classify list with
                       | ((JSONArray (items))[@explicit_arity ]) ->
                           let transformer json =
                             match Js.Json.classify json with
                             | ((JSONArray
                                 ([|arg0;arg1|]))[@explicit_arity ]) ->
                                 (match (deserialize_SharedTypes__SimpleType__declaration
                                           deserialize_TypeMap__DigTypes____typeSource)
                                          arg1
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
                                       [@reason.raw_literal "Expected array"])))
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
                                  | ((Ok (value))[@explicit_arity ]) ->
                                      (match loop rest with
                                       | ((Belt.Result.Error
                                           (error))[@explicit_arity ]) ->
                                           ((Belt.Result.Error (error))
                                           [@explicit_arity ])
                                       | ((Ok (rest))[@explicit_arity ]) ->
                                           ((Ok ((value :: rest)))
                                           [@explicit_arity ]))) in
                           loop (Belt.List.fromArray items)
                       | _ ->
                           ((Belt.Result.Error
                               ((("expected an array")
                                 [@reason.raw_literal "expected an array"])))
                           [@explicit_arity ])) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_current))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "pastVersions"), (=))
                   with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "pastVersions")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (deserialize_Belt__Belt_HashMapInt____t
                                 (fun list ->
                                    match Js.Json.classify list with
                                    | ((JSONArray (items))[@explicit_arity ])
                                        ->
                                        let transformer json =
                                          match Js.Json.classify json with
                                          | ((JSONArray
                                              ([|arg0;arg1|]))[@explicit_arity
                                                                ])
                                              ->
                                              (match (deserialize_SharedTypes__SimpleType__declaration
                                                        deserialize_TypeMap__DigTypes____typeSource)
                                                       arg1
                                               with
                                               | Belt.Result.Ok arg1 ->
                                                   (match deserialize_TypeMap__DigTypes____shortReference
                                                            arg0
                                                    with
                                                    | Belt.Result.Ok arg0 ->
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
                                                   (error))[@explicit_arity ])
                                                   ->
                                                   ((Belt.Result.Error
                                                       (error))
                                                   [@explicit_arity ])
                                               | ((Ok
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
                                                    | ((Ok
                                                        (rest))[@explicit_arity
                                                                 ])
                                                        ->
                                                        ((Ok
                                                            ((value :: rest)))
                                                        [@explicit_arity ]))) in
                                        loop (Belt.List.fromArray items)
                                    | _ ->
                                        ((Belt.Result.Error
                                            ((("expected an array")
                                              [@reason.raw_literal
                                                "expected an array"])))
                                        [@explicit_arity ]))) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_pastVersions))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "version"),
                                     (=))
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
                                  | ((Ok (attr_version))[@explicit_arity ])
                                      ->
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
and (deserialize_Ident____t : Json.t -> (Ident.t, string) Belt.Result.t) =
  fun record ->
    match record with
    | ((Json.Object (items))[@explicit_arity ]) ->
        (match ((Belt.List.getAssoc items "flags"), (=)) with
         | None ->
             ((Belt.Result.Error
                 (((("No attribute ")[@reason.raw_literal "No attribute "]) ^
                     "flags")))
             [@explicit_arity ])
         | ((Some (json))[@explicit_arity ]) ->
             (match (fun number ->
                       match number with
                       | ((Json.Number (number))[@explicit_arity ]) ->
                           ((Belt.Result.Ok ((int_of_float number)))
                           [@explicit_arity ])
                       | _ ->
                           ((Error
                               ((("Expected a float")
                                 [@reason.raw_literal "Expected a float"])))
                           [@explicit_arity ])) json
              with
              | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                  ((Belt.Result.Error (error))[@explicit_arity ])
              | ((Ok (attr_flags))[@explicit_arity ]) ->
                  (match ((Belt.List.getAssoc items "name"), (=)) with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "name")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (fun string ->
                                 match string with
                                 | ((Json.String (string))[@explicit_arity ])
                                     -> ((Belt.Result.Ok (string))
                                     [@explicit_arity ])
                                 | _ ->
                                     ((Error
                                         ((("epected a string")
                                           [@reason.raw_literal
                                             "epected a string"])))
                                     [@explicit_arity ])) json
                        with
                        | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                            ((Belt.Result.Error (error))[@explicit_arity ])
                        | ((Ok (attr_name))[@explicit_arity ]) ->
                            (match ((Belt.List.getAssoc items "stamp"), (=))
                             with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "stamp")))
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
                                  | ((Ok (attr_stamp))[@explicit_arity ]) ->
                                      Belt.Result.Ok
                                        {
                                          stamp = attr_stamp;
                                          name = attr_name;
                                          flags = attr_flags
                                        }))))))
    | _ ->
        ((Belt.Result.Error
            ((("Expected an object")
              [@reason.raw_literal "Expected an object"])))
        [@explicit_arity ])
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
          (match ((Belt.List.getAssoc items "body"), (=)) with
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
                | ((Ok (attr_body))[@explicit_arity ]) ->
                    (match ((Belt.List.getAssoc items "variables"), (=)) with
                     | None ->
                         ((Belt.Result.Error
                             (((("No attribute ")
                                 [@reason.raw_literal "No attribute "]) ^
                                 "variables")))
                         [@explicit_arity ])
                     | ((Some (json))[@explicit_arity ]) ->
                         (match (fun list ->
                                   match Js.Json.classify list with
                                   | ((JSONArray (items))[@explicit_arity ])
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
                                              | ((Ok
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
                                                   | ((Ok
                                                       (rest))[@explicit_arity
                                                                ])
                                                       ->
                                                       ((Ok ((value :: rest)))
                                                       [@explicit_arity ]))) in
                                       loop (Belt.List.fromArray items)
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
                          | ((Ok (attr_variables))[@explicit_arity ]) ->
                              (match ((Belt.List.getAssoc items "name"), (=))
                               with
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
                                    | ((Ok (attr_name))[@explicit_arity ]) ->
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
and deserialize_Location____loc :
  'arg0 .
    (Json.t -> ('arg0, string) Belt.Result.t) ->
      Json.t -> ('arg0 Location.loc, string) Belt.Result.t
  =
  fun aTransformer ->
    fun record ->
      match record with
      | ((Json.Object (items))[@explicit_arity ]) ->
          (match ((Belt.List.getAssoc items "loc"), (=)) with
           | None ->
               ((Belt.Result.Error
                   (((("No attribute ")[@reason.raw_literal "No attribute "])
                       ^ "loc")))
               [@explicit_arity ])
           | ((Some (json))[@explicit_arity ]) ->
               (match deserialize_Location____t json with
                | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                    ((Belt.Result.Error (error))[@explicit_arity ])
                | ((Ok (attr_loc))[@explicit_arity ]) ->
                    (match ((Belt.List.getAssoc items "txt"), (=)) with
                     | None ->
                         ((Belt.Result.Error
                             (((("No attribute ")
                                 [@reason.raw_literal "No attribute "]) ^
                                 "txt")))
                         [@explicit_arity ])
                     | ((Some (json))[@explicit_arity ]) ->
                         (match aTransformer json with
                          | ((Belt.Result.Error (error))[@explicit_arity ])
                              -> ((Belt.Result.Error (error))
                              [@explicit_arity ])
                          | ((Ok (attr_txt))[@explicit_arity ]) ->
                              Belt.Result.Ok
                                { txt = attr_txt; loc = attr_loc }))))
      | _ ->
          ((Belt.Result.Error
              ((("Expected an object")
                [@reason.raw_literal "Expected an object"])))
          [@explicit_arity ])
let rec (serialize_SharedTypes__Module__contents :
  SharedTypes.Module.contents -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("exported",
          (serialize_SharedTypes__Module__exported record.exported));
        ("topLevel",
          (((fun list ->
               Json.Array
                 (Belt.List.toArray
                    (Belt.List.map list
                       (serialize_SharedTypes____declared
                          serialize_SharedTypes__Module__item)))))
             record.topLevel))|]
and (serialize_SharedTypes__Module__item :
  SharedTypes.Module.item -> Js.Json.t) =
  fun constructor ->
    match constructor with
    | Value arg0 ->
        Json.Array
          [|(Json.String "Value");(serialize_SharedTypes__Value__t arg0)|]
    | Type arg0 ->
        Json.Array
          [|(Json.String "Type");(serialize_SharedTypes__Type__t arg0)|]
    | Module arg0 ->
        Json.Array
          [|(Json.String "Module");(serialize_SharedTypes__Module__kind arg0)|]
    | ModuleType arg0 ->
        Json.Array
          [|(Json.String "ModuleType");(serialize_SharedTypes__Module__kind
                                          arg0)|]
and (serialize_TypeMap__DigTypes____shortReference :
  TypeMap__DigTypes.shortReference -> Js.Json.t) =
  fun value ->
    (fun (arg0, arg1, arg2) ->
       Json.Array
         [|(serialize_Analyze__TopTypes____moduleName arg0);(((fun list ->
                                                                 Json.Array
                                                                   (Belt.List.toArray
                                                                    (Belt.List.map
                                                                    list
                                                                    (fun s ->
                                                                    ((Json.String
                                                                    (s))
                                                                    [@explicit_arity
                                                                    ]))))))
                                                               arg1);(
           ((fun s -> ((Json.String (s))[@explicit_arity ]))) arg2)|]) value
and (serialize_SharedTypes____flexibleDeclaration :
  SharedTypes.flexibleDeclaration -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("declToString",
          (((fun _ -> failwith "not impl expr")) record.declToString));
        ("declarationKind",
          (serialize_SharedTypes____kinds record.declarationKind));("asSimpleDeclaration",
                                                                    (((fun _
                                                                    ->
                                                                    failwith
                                                                    "not impl expr"))
                                                                    record.asSimpleDeclaration))|]
and serialize_SharedTypes____stampMap :
  'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 SharedTypes.stampMap -> Js.Json.t =
  fun tTransformer ->
    fun value ->
      (serialize_Hashtbl____t
         (fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ]))
         tTransformer) value
and (serialize_SharedTypes____visibilityPath :
  SharedTypes.visibilityPath -> Js.Json.t) =
  fun constructor ->
    match constructor with
    | File (arg0, arg1) ->
        Json.Array
          [|(Json.String "File");(((fun s -> ((Json.String (s))
                                      [@explicit_arity ]))) arg0);(((fun s ->
                                                                    ((Json.String
                                                                    (s))
                                                                    [@explicit_arity
                                                                    ]))) arg1)|]
    | NotVisible -> Json.Array [|(Json.String "NotVisible")|]
    | IncludedModule (arg0, arg1) ->
        Json.Array
          [|(Json.String "IncludedModule");(serialize_Path____t arg0);(
            serialize_SharedTypes____visibilityPath arg1)|]
    | ExportedModule (arg0, arg1) ->
        Json.Array
          [|(Json.String "ExportedModule");(((fun s -> ((Json.String (s))
                                                [@explicit_arity ]))) arg0);(
            serialize_SharedTypes____visibilityPath arg1)|]
    | HiddenModule (arg0, arg1) ->
        Json.Array
          [|(Json.String "HiddenModule");(((fun s -> ((Json.String (s))
                                              [@explicit_arity ]))) arg0);(
            serialize_SharedTypes____visibilityPath arg1)|]
    | Expression arg0 ->
        Json.Array
          [|(Json.String "Expression");(serialize_SharedTypes____visibilityPath
                                          arg0)|]
and (serialize_SharedTypes____kinds : SharedTypes.kinds -> Js.Json.t) =
  fun value -> (fun _ -> failwith "not impl expr") value
and serialize_SharedTypes__SimpleType__expr :
  'arg0 .
    ('arg0 -> Js.Json.t) -> 'arg0 SharedTypes.SimpleType.expr -> Js.Json.t
  =
  fun sourceTransformer ->
    fun constructor ->
      match constructor with
      | Variable arg0 ->
          Json.Array
            [|(Json.String "Variable");(((fun s -> ((Json.String (s))
                                            [@explicit_arity ]))) arg0)|]
      | AnonVariable -> Json.Array [|(Json.String "AnonVariable")|]
      | Reference (arg0, arg1) ->
          Json.Array
            [|(Json.String "Reference");(sourceTransformer arg0);(((fun list
                                                                    ->
                                                                    Json.Array
                                                                    (Belt.List.toArray
                                                                    (Belt.List.map
                                                                    list
                                                                    (serialize_SharedTypes__SimpleType__expr
                                                                    sourceTransformer)))))
                                                                    arg1)|]
      | Tuple arg0 ->
          Json.Array
            [|(Json.String "Tuple");(((fun list ->
                                         Json.Array
                                           (Belt.List.toArray
                                              (Belt.List.map list
                                                 (serialize_SharedTypes__SimpleType__expr
                                                    sourceTransformer)))))
                                       arg0)|]
      | Fn (arg0, arg1) ->
          Json.Array
            [|(Json.String "Fn");(((fun list ->
                                      Json.Array
                                        (Belt.List.toArray
                                           (Belt.List.map list
                                              (fun (arg0, arg1) ->
                                                 Json.Array
                                                   [|((((fun transformer ->
                                                           function
                                                           | None -> Js.Null
                                                           | ((Some
                                                               (v))[@explicit_arity
                                                                    ])
                                                               ->
                                                               transformer v))
                                                         (fun s ->
                                                            ((Json.String (s))
                                                            [@explicit_arity
                                                              ]))) arg0);(
                                                     (serialize_SharedTypes__SimpleType__expr
                                                        sourceTransformer)
                                                       arg1)|]))))) arg0);(
              (serialize_SharedTypes__SimpleType__expr sourceTransformer)
                arg1)|]
      | Other -> Json.Array [|(Json.String "Other")|]
and (serialize_SharedTypes__Type__kind : SharedTypes.Type.kind -> Js.Json.t)
  =
  fun constructor ->
    match constructor with
    | Abstract arg0 ->
        Json.Array
          [|(Json.String "Abstract");((((fun transformer ->
                                           function
                                           | None -> Js.Null
                                           | ((Some (v))[@explicit_arity ])
                                               -> transformer v))
                                         (fun (arg0, arg1) ->
                                            Json.Array
                                              [|(serialize_Path____t arg0);(
                                                ((fun list ->
                                                    Json.Array
                                                      (Belt.List.toArray
                                                         (Belt.List.map list
                                                            serialize_SharedTypes____flexibleType))))
                                                  arg1)|])) arg0)|]
    | Open -> Json.Array [|(Json.String "Open")|]
    | Tuple arg0 ->
        Json.Array
          [|(Json.String "Tuple");(((fun list ->
                                       Json.Array
                                         (Belt.List.toArray
                                            (Belt.List.map list
                                               serialize_SharedTypes____flexibleType))))
                                     arg0)|]
    | Record arg0 ->
        Json.Array
          [|(Json.String "Record");(((fun list ->
                                        Json.Array
                                          (Belt.List.toArray
                                             (Belt.List.map list
                                                serialize_SharedTypes__Type__Attribute__t))))
                                      arg0)|]
    | Variant arg0 ->
        Json.Array
          [|(Json.String "Variant");(((fun list ->
                                         Json.Array
                                           (Belt.List.toArray
                                              (Belt.List.map list
                                                 serialize_SharedTypes__Type__Constructor__t))))
                                       arg0)|]
and (serialize_Belt__Belt_HashMapInt____key :
  Belt__Belt_HashMapInt.key -> Js.Json.t) =
  fun value ->
    (fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])) value
and serialize_Belt__Belt_internalBuckets____bucket :
  'arg0 'arg1 .
    ('arg0 -> Js.Json.t) ->
      ('arg1 -> Js.Json.t) ->
        ('arg0, 'arg1) Belt__Belt_internalBuckets.bucket -> Js.Json.t
  =
  fun aTransformer ->
    fun bTransformer ->
      TransformHelpers.serialize_Belt__Belt_internalBuckets____bucket
        aTransformer bTransformer
and (serialize_TypeMap__DigTypes____typeSource :
  TypeMap__DigTypes.typeSource -> Js.Json.t) =
  fun constructor ->
    match constructor with
    | Builtin arg0 ->
        Json.Array
          [|(Json.String "Builtin");(((fun s -> ((Json.String (s))
                                         [@explicit_arity ]))) arg0)|]
    | Public arg0 ->
        Json.Array
          [|(Json.String "Public");(serialize_TypeMap__DigTypes____reference
                                      arg0)|]
    | NotFound -> Json.Array [|(Json.String "NotFound")|]
and serialize_Belt__Belt_internalBuckets____t :
  'arg0 'arg1 'arg2 'arg3 .
    ('arg0 -> Js.Json.t) ->
      ('arg1 -> Js.Json.t) ->
        ('arg2 -> Js.Json.t) ->
          ('arg3 -> Js.Json.t) ->
            ('arg0, 'arg1, 'arg2, 'arg3) Belt__Belt_internalBuckets.t ->
              Js.Json.t
  =
  fun hashTransformer ->
    fun eqTransformer ->
      fun aTransformer ->
        fun bTransformer ->
          fun value ->
            (serialize_Belt__Belt_internalBucketsType____container
               hashTransformer eqTransformer
               (serialize_Belt__Belt_internalBuckets____bucket aTransformer
                  bTransformer)) value
and (serialize_SharedTypes__Value__t : SharedTypes.Value.t -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("typ", (serialize_SharedTypes____flexibleType record.typ));("recursive",
                                                                    (((fun b
                                                                    ->
                                                                    match b
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Json.True
                                                                    | 
                                                                    false ->
                                                                    Json.False))
                                                                    record.recursive))|]
and (serialize_Lexing____position : Lexing.position -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("pos_fname",
          (((fun s -> ((Json.String (s))[@explicit_arity ])))
             record.pos_fname));("pos_lnum",
                                  (((fun i ->
                                       ((Json.Number ((float_of_int i)))
                                       [@explicit_arity ]))) record.pos_lnum));
        ("pos_bol",
          (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
             record.pos_bol));("pos_cnum",
                                (((fun i -> ((Json.Number ((float_of_int i)))
                                     [@explicit_arity ]))) record.pos_cnum))|]
and (serialize_SharedTypes__Type__Constructor__t :
  SharedTypes.Type.Constructor.t -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("stamp",
          (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
             record.stamp));("name",
                              ((serialize_Location____loc
                                  (fun s -> ((Json.String (s))
                                     [@explicit_arity ]))) record.name));
        ("args",
          (((fun list ->
               Json.Array
                 (Belt.List.toArray
                    (Belt.List.map list
                       (fun (arg0, arg1) ->
                          Json.Array
                            [|(serialize_SharedTypes____flexibleType arg0);(
                              serialize_Location____t arg1)|])))))
             record.args));("res",
                             ((((fun transformer ->
                                   function
                                   | None -> Js.Null
                                   | ((Some (v))[@explicit_arity ]) ->
                                       transformer v))
                                 serialize_SharedTypes____flexibleType)
                                record.res))|]
and (serialize_SharedTypes____stamps : SharedTypes.stamps -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("types",
          ((serialize_SharedTypes____stampMap
              (serialize_SharedTypes____declared
                 serialize_SharedTypes__Type__t)) record.types));("values",
                                                                   ((serialize_SharedTypes____stampMap
                                                                    (serialize_SharedTypes____declared
                                                                    serialize_SharedTypes__Value__t))
                                                                    record.values));
        ("modules",
          ((serialize_SharedTypes____stampMap
              (serialize_SharedTypes____declared
                 serialize_SharedTypes__Module__kind)) record.modules));
        ("moduleTypes",
          ((serialize_SharedTypes____stampMap
              (serialize_SharedTypes____declared
                 serialize_SharedTypes__Module__kind)) record.moduleTypes));
        ("constructors",
          ((serialize_SharedTypes____stampMap
              (serialize_SharedTypes____declared
                 serialize_SharedTypes__Type__Constructor__t))
             record.constructors))|]
and (serialize_SharedTypes__Module__kind :
  SharedTypes.Module.kind -> Js.Json.t) =
  fun constructor ->
    match constructor with
    | Ident arg0 ->
        Json.Array [|(Json.String "Ident");(serialize_Path____t arg0)|]
    | Structure arg0 ->
        Json.Array
          [|(Json.String "Structure");(serialize_SharedTypes__Module__contents
                                         arg0)|]
and (serialize_Query____queryEnv : Query.queryEnv -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("file", (serialize_SharedTypes____file record.file));("exported",
                                                                (serialize_SharedTypes__Module__exported
                                                                   record.exported))|]
and serialize_SharedTypes____declared :
  'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 SharedTypes.declared -> Js.Json.t =
  fun tTransformer ->
    fun record ->
      Json.Object
        [|("name",
            ((serialize_Location____loc
                (fun s -> ((Json.String (s))[@explicit_arity ]))) record.name));
          ("extentLoc", (serialize_Location____t record.extentLoc));("scopeLoc",
                                                                    (serialize_Location____t
                                                                    record.scopeLoc));
          ("stamp",
            (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
               record.stamp));("deprecated",
                                ((((fun transformer ->
                                      function
                                      | None -> Js.Null
                                      | ((Some (v))[@explicit_arity ]) ->
                                          transformer v))
                                    (fun s -> ((Json.String (s))
                                       [@explicit_arity ])))
                                   record.deprecated));("modulePath",
                                                         (serialize_SharedTypes____visibilityPath
                                                            record.modulePath));
          ("exported",
            (((fun b ->
                 match b with | true -> Json.True | false -> Json.False))
               record.exported));("docstring",
                                   ((((fun transformer ->
                                         function
                                         | None -> Js.Null
                                         | ((Some (v))[@explicit_arity ]) ->
                                             transformer v))
                                       (fun s -> ((Json.String (s))
                                          [@explicit_arity ])))
                                      record.docstring));("contents",
                                                           (tTransformer
                                                              record.contents))|]
and (serialize_SharedTypes____file : SharedTypes.file -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("uri",
          (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.uri));
        ("docstring",
          ((((fun transformer ->
                function
                | None -> Js.Null
                | ((Some (v))[@explicit_arity ]) -> transformer v))
              (fun s -> ((Json.String (s))[@explicit_arity ])))
             record.docstring));("stamps",
                                  (serialize_SharedTypes____stamps
                                     record.stamps));("moduleName",
                                                       (((fun s ->
                                                            ((Json.String (s))
                                                            [@explicit_arity
                                                              ])))
                                                          record.moduleName));
        ("contents",
          (serialize_SharedTypes__Module__contents record.contents))|]
and (serialize_SharedTypes__Module__exported :
  SharedTypes.Module.exported -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("types", (serialize_SharedTypes____namedStampMap record.types));
        ("values", (serialize_SharedTypes____namedStampMap record.values));
        ("modules", (serialize_SharedTypes____namedStampMap record.modules));
        ("moduleTypes",
          (serialize_SharedTypes____namedStampMap record.moduleTypes))|]
and (serialize_Path____t : Path.t -> Js.Json.t) =
  fun constructor ->
    match constructor with
    | Pident arg0 ->
        Json.Array [|(Json.String "Pident");(serialize_Ident____t arg0)|]
    | Pdot (arg0, arg1, arg2) ->
        Json.Array
          [|(Json.String "Pdot");(serialize_Path____t arg0);(((fun s ->
                                                                 ((Json.String
                                                                    (s))
                                                                 [@explicit_arity
                                                                   ]))) arg1);(
            ((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
              arg2)|]
    | Papply (arg0, arg1) ->
        Json.Array
          [|(Json.String "Papply");(serialize_Path____t arg0);(serialize_Path____t
                                                                 arg1)|]
and (serialize_SharedTypes__Type__t : SharedTypes.Type.t -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("kind", (serialize_SharedTypes__Type__kind record.kind));("params",
                                                                    (
                                                                    ((fun
                                                                    list ->
                                                                    Json.Array
                                                                    (Belt.List.toArray
                                                                    (Belt.List.map
                                                                    list
                                                                    (fun
                                                                    (arg0,
                                                                    arg1) ->
                                                                    Json.Array
                                                                    [|(
                                                                    serialize_SharedTypes____flexibleType
                                                                    arg0);(
                                                                    serialize_Location____t
                                                                    arg1)|])))))
                                                                    record.params));
        ("typ", (serialize_SharedTypes____flexibleDeclaration record.typ))|]
and (serialize_SharedTypes____flexibleType :
  SharedTypes.flexibleType -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("toString", (((fun _ -> failwith "not impl expr")) record.toString));
        ("variableKind",
          (serialize_SharedTypes____kinds record.variableKind));("getConstructorPath",
                                                                  (((fun _ ->
                                                                    failwith
                                                                    "not impl expr"))
                                                                    record.getConstructorPath));
        ("getArguments",
          (((fun _ -> failwith "not impl expr")) record.getArguments));
        ("asSimpleType",
          (((fun _ -> failwith "not impl expr")) record.asSimpleType))|]
and (serialize_Location____t : Location.t -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("loc_start", (serialize_Lexing____position record.loc_start));
        ("loc_end", (serialize_Lexing____position record.loc_end));("loc_ghost",
                                                                    (((fun b
                                                                    ->
                                                                    match b
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Json.True
                                                                    | 
                                                                    false ->
                                                                    Json.False))
                                                                    record.loc_ghost))|]
and serialize_Hashtbl____t :
  'arg0 'arg1 .
    ('arg0 -> Js.Json.t) ->
      ('arg1 -> Js.Json.t) -> ('arg0, 'arg1) Hashtbl.t -> Js.Json.t
  =
  fun aTransformer ->
    fun bTransformer ->
      TransformHelpers.serialize_Hashtbl____t aTransformer bTransformer
and (serialize_TypeMap__DigTypes____reference :
  TypeMap__DigTypes.reference -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("uri",
          (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.uri));
        ("moduleName",
          (((fun s -> ((Json.String (s))[@explicit_arity ])))
             record.moduleName));("declared",
                                   ((serialize_SharedTypes____declared
                                       serialize_SharedTypes__Type__t)
                                      record.declared));("modulePath",
                                                          (((fun list ->
                                                               Json.Array
                                                                 (Belt.List.toArray
                                                                    (
                                                                    Belt.List.map
                                                                    list
                                                                    (fun s ->
                                                                    ((Json.String
                                                                    (s))
                                                                    [@explicit_arity
                                                                    ]))))))
                                                             record.modulePath));
        ("name",
          (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.name));
        ("env", (serialize_Query____queryEnv record.env))|]
and serialize_Belt__Belt_internalBucketsType____container :
  'arg0 'arg1 'arg2 .
    ('arg0 -> Js.Json.t) ->
      ('arg1 -> Js.Json.t) ->
        ('arg2 -> Js.Json.t) ->
          ('arg0, 'arg1, 'arg2) Belt__Belt_internalBucketsType.container ->
            Js.Json.t
  =
  fun hashTransformer ->
    fun eqTransformer ->
      fun cTransformer ->
        TransformHelpers.serialize_Belt__Belt_internalBucketsType____container
          hashTransformer eqTransformer cTransformer
and serialize_Belt__Belt_HashMapInt____t :
  'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 Belt__Belt_HashMapInt.t -> Js.Json.t
  =
  fun bTransformer ->
    fun value ->
      (serialize_Belt__Belt_internalBuckets____t
         (fun _ -> failwith "Not found") (fun _ -> failwith "Not found")
         serialize_Belt__Belt_HashMapInt____key bTransformer) value
and (serialize_SharedTypes__Type__Attribute__t :
  SharedTypes.Type.Attribute.t -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("stamp",
          (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
             record.stamp));("name",
                              ((serialize_Location____loc
                                  (fun s -> ((Json.String (s))
                                     [@explicit_arity ]))) record.name));
        ("typ", (serialize_SharedTypes____flexibleType record.typ));("typLoc",
                                                                    (serialize_Location____t
                                                                    record.typLoc))|]
and serialize_SharedTypes__SimpleType__body :
  'arg0 .
    ('arg0 -> Js.Json.t) -> 'arg0 SharedTypes.SimpleType.body -> Js.Json.t
  =
  fun sourceTransformer ->
    fun constructor ->
      match constructor with
      | Open -> Json.Array [|(Json.String "Open")|]
      | Abstract -> Json.Array [|(Json.String "Abstract")|]
      | Expr arg0 ->
          Json.Array
            [|(Json.String "Expr");((serialize_SharedTypes__SimpleType__expr
                                       sourceTransformer) arg0)|]
      | Record arg0 ->
          Json.Array
            [|(Json.String "Record");(((fun list ->
                                          Json.Array
                                            (Belt.List.toArray
                                               (Belt.List.map list
                                                  (fun (arg0, arg1) ->
                                                     Json.Array
                                                       [|(((fun s ->
                                                              ((Json.String
                                                                  (s))
                                                              [@explicit_arity
                                                                ]))) arg0);(
                                                         (serialize_SharedTypes__SimpleType__expr
                                                            sourceTransformer)
                                                           arg1)|]))))) arg0)|]
      | Variant arg0 ->
          Json.Array
            [|(Json.String "Variant");(((fun list ->
                                           Json.Array
                                             (Belt.List.toArray
                                                (Belt.List.map list
                                                   (fun (arg0, arg1, arg2) ->
                                                      Json.Array
                                                        [|(((fun s ->
                                                               ((Json.String
                                                                   (s))
                                                               [@explicit_arity
                                                                 ]))) arg0);(
                                                          ((fun list ->
                                                              Json.Array
                                                                (Belt.List.toArray
                                                                   (Belt.List.map
                                                                    list
                                                                    (serialize_SharedTypes__SimpleType__expr
                                                                    sourceTransformer)))))
                                                            arg1);((((fun
                                                                    transformer
                                                                    ->
                                                                    function
                                                                    | 
                                                                    None ->
                                                                    Js.Null
                                                                    | 
                                                                    ((Some
                                                                    (v))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    transformer
                                                                    v))
                                                                    (serialize_SharedTypes__SimpleType__expr
                                                                    sourceTransformer))
                                                                    arg2)|])))))
                                         arg0)|]
and serialize_SharedTypes____namedMap :
  'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 SharedTypes.namedMap -> Js.Json.t =
  fun tTransformer ->
    fun value ->
      (serialize_Hashtbl____t
         (fun s -> ((Json.String (s))[@explicit_arity ])) tTransformer) value
and (serialize_SharedTypes____namedStampMap :
  SharedTypes.namedStampMap -> Js.Json.t) =
  fun value ->
    (serialize_SharedTypes____namedMap
       (fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ]))) value
and (serialize_TypeMap__DigTypes____lockfile :
  TypeMap__DigTypes.lockfile -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("version",
          (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
             record.version));("pastVersions",
                                ((serialize_Belt__Belt_HashMapInt____t
                                    (fun list ->
                                       Json.Array
                                         (Belt.List.toArray
                                            (Belt.List.map list
                                               (fun (arg0, arg1) ->
                                                  Json.Array
                                                    [|(serialize_TypeMap__DigTypes____shortReference
                                                         arg0);((serialize_SharedTypes__SimpleType__declaration
                                                                   serialize_TypeMap__DigTypes____typeSource)
                                                                  arg1)|])))))
                                   record.pastVersions));("current",
                                                           (((fun list ->
                                                                Json.Array
                                                                  (Belt.List.toArray
                                                                    (Belt.List.map
                                                                    list
                                                                    (fun
                                                                    (arg0,
                                                                    arg1) ->
                                                                    Json.Array
                                                                    [|(
                                                                    serialize_TypeMap__DigTypes____shortReference
                                                                    arg0);(
                                                                    (serialize_SharedTypes__SimpleType__declaration
                                                                    serialize_TypeMap__DigTypes____typeSource)
                                                                    arg1)|])))))
                                                              record.current))|]
and (serialize_Ident____t : Ident.t -> Js.Json.t) =
  fun record ->
    Json.Object
      [|("stamp",
          (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
             record.stamp));("name",
                              (((fun s -> ((Json.String (s))
                                   [@explicit_arity ]))) record.name));
        ("flags",
          (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
             record.flags))|]
and serialize_SharedTypes__SimpleType__declaration :
  'arg0 .
    ('arg0 -> Js.Json.t) ->
      'arg0 SharedTypes.SimpleType.declaration -> Js.Json.t
  =
  fun sourceTransformer ->
    fun record ->
      Json.Object
        [|("name",
            (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.name));
          ("variables",
            (((fun list ->
                 Json.Array
                   (Belt.List.toArray
                      (Belt.List.map list
                         (serialize_SharedTypes__SimpleType__expr
                            sourceTransformer))))) record.variables));
          ("body",
            ((serialize_SharedTypes__SimpleType__body sourceTransformer)
               record.body))|]
and (serialize_Analyze__TopTypes____moduleName :
  Analyze__TopTypes.moduleName -> Js.Json.t) =
  fun value -> (fun s -> ((Json.String (s))[@explicit_arity ])) value
and serialize_Location____loc :
  'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 Location.loc -> Js.Json.t =
  fun aTransformer ->
    fun record ->
      Json.Object
        [|("txt", (aTransformer record.txt));("loc",
                                               (serialize_Location____t
                                                  record.loc))|]