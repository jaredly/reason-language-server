let rec (deserialize_Hello__Tryit____lockfile :
  Js.Json.t -> (Tryit.Hello.lockfile, string) Belt.Result.t) =
  fun record ->
    match Js.Json.classify record with
    | ((JSONObject (dict))[@explicit_arity ]) ->
        (match Js.Dict.get dict "current" with
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
                                 (match (fun number ->
                                           match Js.Json.classify number with
                                           | ((JSONNumber
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
                                               [@explicit_arity ])) arg1
                                  with
                                  | Belt.Result.Ok arg1 ->
                                      (match deserialize_Hello__Tryit____shortReference
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
                                  | ((Belt.Result.Ok
                                      (value))[@explicit_arity ]) ->
                                      (match loop rest with
                                       | ((Belt.Result.Error
                                           (error))[@explicit_arity ]) ->
                                           ((Belt.Result.Error (error))
                                           [@explicit_arity ])
                                       | ((Belt.Result.Ok
                                           (rest))[@explicit_arity ]) ->
                                           ((Belt.Result.Ok ((value :: rest)))
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
                  (match Js.Dict.get dict "pastVersions" with
                   | None ->
                       ((Belt.Result.Error
                           (((("No attribute ")
                               [@reason.raw_literal "No attribute "]) ^
                               "pastVersions")))
                       [@explicit_arity ])
                   | ((Some (json))[@explicit_arity ]) ->
                       (match (deserialize_Belt_HashMapInt____t
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
                                              (match (fun number ->
                                                        match Js.Json.classify
                                                                number
                                                        with
                                                        | ((JSONNumber
                                                            (number))
                                                            [@explicit_arity
                                                              ])
                                                            ->
                                                            ((Belt.Result.Ok
                                                                ((int_of_float
                                                                    number)))
                                                            [@explicit_arity
                                                              ])
                                                        | _ ->
                                                            ((Error
                                                                ((("Expected a float")
                                                                  [@reason.raw_literal
                                                                    "Expected a float"])))
                                                            [@explicit_arity
                                                              ])) arg1
                                               with
                                               | Belt.Result.Ok arg1 ->
                                                   (match deserialize_Hello__Tryit____shortReference
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
                            (match Js.Dict.get dict "version" with
                             | None ->
                                 ((Belt.Result.Error
                                     (((("No attribute ")
                                         [@reason.raw_literal
                                           "No attribute "])
                                         ^ "version")))
                                 [@explicit_arity ])
                             | ((Some (json))[@explicit_arity ]) ->
                                 (match (fun number ->
                                           match Js.Json.classify number with
                                           | ((JSONNumber
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
and (deserialize_Hello__Tryit____shortReference :
  Js.Json.t -> (Tryit.Hello.shortReference, string) Belt.Result.t) =
  fun value ->
    (fun json ->
       match Js.Json.classify json with
       | ((JSONArray ([|arg0;arg1;arg2|]))[@explicit_arity ]) ->
           (match (fun string ->
                     match Js.Json.classify string with
                     | ((JSONString (string))[@explicit_arity ]) ->
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
                                match Js.Json.classify string with
                                | ((JSONString (string))[@explicit_arity ])
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
                              loop (Belt.List.fromArray items)
                          | _ ->
                              ((Belt.Result.Error
                                  ((("expected an array")
                                    [@reason.raw_literal "expected an array"])))
                              [@explicit_arity ])) arg1
                 with
                 | Belt.Result.Ok arg1 ->
                     (match (fun string ->
                               match Js.Json.classify string with
                               | ((JSONString (string))[@explicit_arity ]) ->
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
                          Belt.Result.Ok (arg0, arg1, arg2)
                      | Error error -> Error error)
                 | Error error -> Error error)
            | Error error -> Error error)
       | _ ->
           ((Belt.Result.Error
               ((("Expected array")[@reason.raw_literal "Expected array"])))
           [@explicit_arity ])) value
and deserialize_Belt_HashMapInt____t :
  'arg0 .
    (Js.Json.t -> ('arg0, string) Belt.Result.t) ->
      Js.Json.t -> ('arg0 Belt_HashMapInt.t, string) Belt.Result.t
  =
  fun bTransformer ->
    TransformHelpers.deserialize_Belt_HashMapInt____t bTransformer
let rec (serialize_Hello__Tryit____lockfile :
  Tryit.Hello.lockfile -> Js.Json.t) =
  fun record ->
    Js.Json.object_
      (Js.Dict.fromArray
         [|("version",
             (((fun int -> Js.Json.number (float_of_int int))) record.version));
           ("pastVersions",
             ((serialize_Belt_HashMapInt____t
                 (fun list ->
                    Js.Json.array
                      (Belt.List.toArray
                         (Belt.List.map list
                            (fun (arg0, arg1) ->
                               Js.Json.array
                                 [|(serialize_Hello__Tryit____shortReference
                                      arg0);(((fun int ->
                                                 Js.Json.number
                                                   (float_of_int int))) arg1)|])))))
                record.pastVersions));("current",
                                        (((fun list ->
                                             Js.Json.array
                                               (Belt.List.toArray
                                                  (Belt.List.map list
                                                     (fun (arg0, arg1) ->
                                                        Js.Json.array
                                                          [|(serialize_Hello__Tryit____shortReference
                                                               arg0);(
                                                            ((fun int ->
                                                                Js.Json.number
                                                                  (float_of_int
                                                                    int)))
                                                              arg1)|])))))
                                           record.current))|])
and (serialize_Hello__Tryit____shortReference :
  Tryit.Hello.shortReference -> Js.Json.t) =
  fun value ->
    (fun (arg0, arg1, arg2) ->
       Js.Json.array
         [|(Js.Json.string arg0);(((fun list ->
                                      Js.Json.array
                                        (Belt.List.toArray
                                           (Belt.List.map list Js.Json.string))))
                                    arg1);(Js.Json.string arg2)|]) value
and serialize_Belt_HashMapInt____t :
  'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 Belt_HashMapInt.t -> Js.Json.t =
  fun bTransformer ->
    TransformHelpers.serialize_Belt_HashMapInt____t bTransformer