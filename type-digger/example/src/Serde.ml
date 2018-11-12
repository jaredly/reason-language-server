module V1_Locked =
  struct
    type _Types__Current__dogBreed = Types.Current.dogBreed =
      | Schnouser 
      | Lab 
      | Retriever 
      | Poodle 
    and _Types__Current__household = Types.Current.household =
      {
      people: _Types__Current__person list ;
      pets: _Types__Current__pet list }
    and _Types__Current__person = Types.Current.person =
      {
      name: string ;
      age: float }
    and _Types__Current__pet = Types.Current.pet =
      | Dog of _Types__Current__dogBreed option 
      | Cat 
      | Mouse 
  end
module DeserializeRaw =
  struct
    let rec (deserialize_Types__Current__dogBreed :
      Js.Json.t -> (Types.Current.dogBreed, string) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Schnouser") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Schnouser : Types.Current.dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Lab") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Lab : Types.Current.dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Retriever") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Retriever : Types.Current.dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Poodle") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Poodle : Types.Current.dogBreed)
        | _ -> Error "Expected an array"
    and (deserialize_Types__Current__household :
      Js.Json.t -> (Types.Current.household, string) Belt.Result.t) =
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
      Js.Json.t -> (Types.Current.person, string) Belt.Result.t) =
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
                               ((Belt.Result.Ok (number))[@explicit_arity ])
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
      Js.Json.t -> (Types.Current.pet, string) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag;arg0|] when
            (Js.Json.JSONString "Dog") = (Js.Json.classify tag) ->
            (match ((fun transformer ->
                       fun option ->
                         match Js.Json.classify option with
                         | JSONNull -> ((Belt.Result.Ok (None))
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
                      deserialize_Types__Current__dogBreed) arg0
             with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (Dog (arg0) : Types.Current.pet)
             | Error error -> Error error)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : Types.Current.pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : Types.Current.pet)
        | _ -> Error "Expected an array"
  end
module SerializeRaw =
  struct
    let rec (serialize_Types__Current__dogBreed :
      Types.Current.dogBreed -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Schnouser -> Js.Json.array [|(Js.Json.string "Schnouser")|]
        | Lab -> Js.Json.array [|(Js.Json.string "Lab")|]
        | Retriever -> Js.Json.array [|(Js.Json.string "Retriever")|]
        | Poodle -> Js.Json.array [|(Js.Json.string "Poodle")|]
    and (serialize_Types__Current__household :
      Types.Current.household -> Js.Json.t) =
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
      Types.Current.person -> Js.Json.t) =
      fun record ->
        Js.Json.object_
          (Js.Dict.fromArray
             [|("name", (Js.Json.string record.name));("age",
                                                        (Js.Json.number
                                                           record.age))|])
    and (serialize_Types__Current__pet : Types.Current.pet -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Dog arg0 ->
            Js.Json.array
              [|(Js.Json.string "Dog");((((fun transformer ->
                                             function
                                             | ((Some
                                                 (inner))[@explicit_arity ])
                                                 -> transformer inner
                                             | None -> Js.Json.null))
                                           serialize_Types__Current__dogBreed)
                                          arg0)|]
        | Cat -> Js.Json.array [|(Js.Json.string "Cat")|]
        | Mouse -> Js.Json.array [|(Js.Json.string "Mouse")|]
  end
let serializeCurrent_household =
  SerializeRaw.serialize_Types__Current__household
and deserializeCurrent_household =
  DeserializeRaw.deserialize_Types__Current__household
include SerializeRaw
include DeserializeRaw