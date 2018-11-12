module Version1 =
  struct
    type _Types__Current__household =
      {
      people: _Types__Current__person list ;
      pets: _Types__Current__pet list }
    and _Types__Current__person = {
      name: string ;
      age: int }
    and _Types__Current__pet =
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
module Version2 =
  struct
    type _Types__Current__household =
      {
      people: _Types__Current__person list ;
      pets: _Types__Current__pet list }
    and _Types__Current__person = Types.Current.person =
      {
      name: string ;
      age: float }
    and _Types__Current__pet = Version1._Types__Current__pet =
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
                                                        (Js.Json.number
                                                           record.age))|])
    and (serialize_Types__Current__pet : _Types__Current__pet -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Dog -> Js.Json.array [|(Js.Json.string "Dog")|]
        | Cat -> Js.Json.array [|(Js.Json.string "Cat")|]
    let rec upgrade_Types__Current__household data = wat
    and upgrade_Types__Current__person data = wat
    and upgrade_Types__Current__pet data = data
  end
module Version3 =
  struct
    type _Types__Current__household =
      {
      people: _Types__Current__person list ;
      pets: _Types__Current__pet list }
    and _Types__Current__person = Types.Current.person =
      {
      name: string ;
      age: float }
    and _Types__Current__pet =
      | Dog 
      | Cat 
      | Mouse 
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
      Js.Json.t -> (_Types__Current__pet, string) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Dog") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Dog : _Types__Current__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__Current__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : _Types__Current__pet)
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
                                                        (Js.Json.number
                                                           record.age))|])
    and (serialize_Types__Current__pet : _Types__Current__pet -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Dog -> Js.Json.array [|(Js.Json.string "Dog")|]
        | Cat -> Js.Json.array [|(Js.Json.string "Cat")|]
        | Mouse -> Js.Json.array [|(Js.Json.string "Mouse")|]
    let rec upgrade_Types__Current__household data = wat
    and upgrade_Types__Current__person data = data
    and upgrade_Types__Current__pet data = wat
  end
module Version4 =
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
    let rec (deserialize_Types__Current__dogBreed :
      Js.Json.t -> (_Types__Current__dogBreed, string) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Schnouser") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Schnouser : _Types__Current__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Lab") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Lab : _Types__Current__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Retriever") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Retriever : _Types__Current__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Poodle") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Poodle : _Types__Current__dogBreed)
        | _ -> Error "Expected an array"
    and (deserialize_Types__Current__household :
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
      Js.Json.t -> (_Types__Current__pet, string) Belt.Result.t) =
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
                 Belt.Result.Ok (Dog (arg0) : _Types__Current__pet)
             | Error error -> Error error)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__Current__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : _Types__Current__pet)
        | _ -> Error "Expected an array"
    and (serialize_Types__Current__dogBreed :
      _Types__Current__dogBreed -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Schnouser -> Js.Json.array [|(Js.Json.string "Schnouser")|]
        | Lab -> Js.Json.array [|(Js.Json.string "Lab")|]
        | Retriever -> Js.Json.array [|(Js.Json.string "Retriever")|]
        | Poodle -> Js.Json.array [|(Js.Json.string "Poodle")|]
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
                                                        (Js.Json.number
                                                           record.age))|])
    and (serialize_Types__Current__pet : _Types__Current__pet -> Js.Json.t) =
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
    let rec upgrade_Types__Current__dogBreed data = wat
    and upgrade_Types__Current__household data = wat
    and upgrade_Types__Current__person data = data
    and upgrade_Types__Current__pet data = wat
  end
let currentVersion = 4
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
    (Version4.serialize_Types__Current__household data)
and deserializeHousehold data =
  match parseVersion data with
  | ((Belt.Result.Error (err))[@explicit_arity ]) ->
      ((Belt.Result.Error (err))[@explicit_arity ])
  | ((Ok (version, data))[@implicit_arity ]) ->
      (match version with
       | 4 ->
           (match Version4.deserialize_Types__Current__household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) -> ((Belt.Result.Ok (data))
                [@explicit_arity ]))
       | 3 ->
           (match Version3.deserialize_Types__Current__household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                (match Version4.upgrade_Types__Current__household data with
                 | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                     ((Belt.Result.Error (error))[@explicit_arity ])
                 | ((Ok (data))[@explicit_arity ]) ->
                     ((Belt.Result.Ok (data))[@explicit_arity ])))
       | 2 ->
           (match Version2.deserialize_Types__Current__household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                (match Version3.upgrade_Types__Current__household data with
                 | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                     ((Belt.Result.Error (error))[@explicit_arity ])
                 | ((Ok (data))[@explicit_arity ]) ->
                     (match Version4.upgrade_Types__Current__household data
                      with
                      | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                          ((Belt.Result.Error (error))[@explicit_arity ])
                      | ((Ok (data))[@explicit_arity ]) ->
                          ((Belt.Result.Ok (data))[@explicit_arity ]))))
       | 1 ->
           (match Version1.deserialize_Types__Current__household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                (match Version2.upgrade_Types__Current__household data with
                 | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                     ((Belt.Result.Error (error))[@explicit_arity ])
                 | ((Ok (data))[@explicit_arity ]) ->
                     (match Version3.upgrade_Types__Current__household data
                      with
                      | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                          ((Belt.Result.Error (error))[@explicit_arity ])
                      | ((Ok (data))[@explicit_arity ]) ->
                          (match Version4.upgrade_Types__Current__household
                                   data
                           with
                           | ((Belt.Result.Error (error))[@explicit_arity ])
                               -> ((Belt.Result.Error (error))
                               [@explicit_arity ])
                           | ((Ok (data))[@explicit_arity ]) ->
                               ((Belt.Result.Ok (data))[@explicit_arity ])))))
       | _ ->
           ((Belt.Result.Error
               (("Unexpected version " ^ (string_of_int version))))
           [@explicit_arity ]))