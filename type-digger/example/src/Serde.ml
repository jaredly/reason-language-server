module Version1 =
  struct
    type _Types__Current__household = Types.Current.household =
      {
      people: _Types__Current__person list ;
      pets: _Types__Current__pet list }
    and _Types__Current__person = Types.Current.person =
      {
      name: string ;
      age: int }
    and _Types__Current__pet = Types.Current.pet =
      | Dog 
      | Cat 
    let rec (deserialize_Types__Current__household :
      Js.Json.t -> (_Types__Current__household, string) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "pets" with
             | None -> ((Belt.Result.Error (("No attribute " ^ "pets")))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun list ->
                           match Js.Json.classify list with
                           | ((JSONArray (items))[@explicit_arity ]) ->
                               let transformer =
                                 deserialize_Types__Current__pet in
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
                           | _ -> ((Belt.Result.Error ("expected an array"))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Ok (attr_pets))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "people" with
                       | None ->
                           ((Belt.Result.Error (("No attribute " ^ "people")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types__Current__person in
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
                                         loop (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             ("expected an array"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Ok (attr_people))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { people = attr_people; pets = attr_pets }))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and (deserialize_Types__Current__person :
      Js.Json.t -> (_Types__Current__person, string) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "age" with
             | None -> ((Belt.Result.Error (("No attribute " ^ "age")))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun number ->
                           match Js.Json.classify number with
                           | ((JSONNumber (number))[@explicit_arity ]) ->
                               ((Belt.Result.Ok ((int_of_float number)))
                               [@explicit_arity ])
                           | _ -> ((Error ("Expected a float"))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Ok (attr_age))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "name" with
                       | None ->
                           ((Belt.Result.Error (("No attribute " ^ "name")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun string ->
                                     match Js.Json.classify string with
                                     | ((JSONString
                                         (string))[@explicit_arity ]) ->
                                         ((Belt.Result.Ok (string))
                                         [@explicit_arity ])
                                     | _ -> ((Error ("epected a string"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Ok (attr_name))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { name = attr_name; age = attr_age }))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and (deserialize_Types__Current__pet :
      Js.Json.t -> (_Types__Current__pet, string) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Dog") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Dog : _Types__Current__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__Current__pet)
        | _ -> Error "Expected an array"
    and (serialize_Types__Current__household :
      _Types__Current__household -> Js.Json.t) =
      fun record ->
        Js.Json.object_
          (Js.Dict.fromArray
             [|("people",
                 (((fun list ->
                      Js.Json.array
                        (Belt.List.toArray
                           (Belt.List.map list
                              serialize_Types__Current__person))))
                    record.people));("pets",
                                      (((fun list ->
                                           Js.Json.array
                                             (Belt.List.toArray
                                                (Belt.List.map list
                                                   serialize_Types__Current__pet))))
                                         record.pets))|])
    and (serialize_Types__Current__person :
      _Types__Current__person -> Js.Json.t) =
      fun record ->
        Js.Json.object_
          (Js.Dict.fromArray
             [|("name", (Js.Json.string record.name));("age",
                                                        (((fun int ->
                                                             Js.Json.number
                                                               (float_of_int
                                                                  int)))
                                                           record.age))|])
    and (serialize_Types__Current__pet : _Types__Current__pet -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Dog -> Js.Json.array [|(Js.Json.string "Dog")|]
        | Cat -> Js.Json.array [|(Js.Json.string "Cat")|]
  end
let currentVersion = 1
let parseVersion json =
  match Js.Json.classify json with
  | ((JSONArray ([|version;payload|]))[@explicit_arity ]) ->
      (match Js.Json.classify version with
       | ((JSONNumber (version))[@explicit_arity ]) ->
           ((Belt.Result.Ok ((int_of_float version), payload))
           [@implicit_arity ])
       | _ -> ((Belt.Result.Error ("Invalid version"))[@explicit_arity ]))
  | _ -> ((Belt.Result.Error ("Must have a version"))[@explicit_arity ])
let wrapWithVersion version payload =
  Js.Json.array [|(Js.Json.number (float_of_int version));payload|]
let serializeHousehold data =
  wrapWithVersion currentVersion
    (Version1.serialize_Types__Current__household data)
and deserializeHousehold data =
  match parseVersion data with
  | ((Belt.Result.Error (err))[@explicit_arity ]) ->
      ((Belt.Result.Error (err))[@explicit_arity ])
  | ((Ok (version, data))[@implicit_arity ]) ->
      (match version with
       | 1 ->
           (match Version1.deserialize_Types__Current__household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) -> ((Belt.Result.Ok (data))
                [@explicit_arity ]))
       | _ ->
           ((Belt.Result.Error
               (("Unexpected version " ^ (string_of_int version))))
           [@explicit_arity ]))