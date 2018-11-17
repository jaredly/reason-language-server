module Version1 =
  struct
    type _Types__household =
      {
      people: _Types__person list ;
      pets: _Types__pet list }
    and _Types__person =
      {
      name: string ;
      age: int ;
      coords: (float * float) ;
      parents: (_Types__person * _Types__person) option }
    and _Types__pet =
      | Dog 
      | Cat 
    let rec (deserialize_Types____household :
      Js.Json.t -> (_Types__household, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "pets" with
             | None -> ((Belt.Result.Error (["No attribute pets"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun list ->
                           match Js.Json.classify list with
                           | ((JSONArray (items))[@explicit_arity ]) ->
                               let transformer = deserialize_Types____pet in
                               let rec loop i items =
                                 match items with
                                 | [] -> ((Belt.Result.Ok ([]))
                                     [@explicit_arity ])
                                 | one::rest ->
                                     (match transformer one with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              ((("list element " ^
                                                   (string_of_int i)) ::
                                                error)))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (value))[@explicit_arity ]) ->
                                          (match loop (i + 1) rest with
                                           | ((Belt.Result.Error
                                               (error))[@explicit_arity ]) ->
                                               ((Belt.Result.Error (error))
                                               [@explicit_arity ])
                                           | ((Belt.Result.Ok
                                               (rest))[@explicit_arity ]) ->
                                               ((Belt.Result.Ok
                                                   ((value :: rest)))
                                               [@explicit_arity ]))) in
                               loop 0 (Belt.List.fromArray items)
                           | _ ->
                               ((Belt.Result.Error (["expected an array"]))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute pets" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_pets))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "people" with
                       | None ->
                           ((Belt.Result.Error (["No attribute people"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types____person in
                                         let rec loop i items =
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
                                                        ((("list element " ^
                                                             (string_of_int i))
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop (i + 1) rest
                                                     with
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
                                         loop 0 (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute people" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_people))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { people = attr_people; pets = attr_pets }))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and (deserialize_Types____person :
      Js.Json.t -> (_Types__person, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "parents" with
             | None -> ((Belt.Result.Error (["No attribute parents"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match ((fun transformer ->
                            fun option ->
                              match Js.Json.classify option with
                              | JSONNull -> ((Belt.Result.Ok (None))
                                  [@explicit_arity ])
                              | _ ->
                                  (match transformer option with
                                   | ((Belt.Result.Error
                                       (error))[@explicit_arity ]) ->
                                       ((Belt.Result.Error
                                           (("optional value" :: error)))
                                       [@explicit_arity ])
                                   | ((Ok (value))[@explicit_arity ]) ->
                                       ((Ok
                                           (((Some (value))
                                             [@explicit_arity ])))
                                       [@explicit_arity ])))
                           (fun json ->
                              match Js.Json.classify json with
                              | ((JSONArray
                                  ([|arg0;arg1|]))[@explicit_arity ]) ->
                                  (match deserialize_Types____person arg1
                                   with
                                   | Belt.Result.Ok arg1 ->
                                       (match deserialize_Types____person
                                                arg0
                                        with
                                        | Belt.Result.Ok arg0 ->
                                            Belt.Result.Ok (arg0, arg1)
                                        | Error error ->
                                            Error ("tuple element 0" ::
                                              error))
                                   | Error error ->
                                       Error ("tuple element 1" :: error))
                              | _ ->
                                  ((Belt.Result.Error (["Expected an array"]))
                                  [@explicit_arity ]))) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute parents" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_parents))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "coords" with
                       | None ->
                           ((Belt.Result.Error (["No attribute coords"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun json ->
                                     match Js.Json.classify json with
                                     | ((JSONArray
                                         ([|arg0;arg1|]))[@explicit_arity ])
                                         ->
                                         (match (fun number ->
                                                   match Js.Json.classify
                                                           number
                                                   with
                                                   | ((JSONNumber
                                                       (number))[@explicit_arity
                                                                  ])
                                                       ->
                                                       ((Belt.Result.Ok
                                                           (number))
                                                       [@explicit_arity ])
                                                   | _ ->
                                                       ((Error
                                                           (["Expected a float"]))
                                                       [@explicit_arity ]))
                                                  arg1
                                          with
                                          | Belt.Result.Ok arg1 ->
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
                                                                (number))
                                                            [@explicit_arity
                                                              ])
                                                        | _ ->
                                                            ((Error
                                                                (["Expected a float"]))
                                                            [@explicit_arity
                                                              ])) arg0
                                               with
                                               | Belt.Result.Ok arg0 ->
                                                   Belt.Result.Ok
                                                     (arg0, arg1)
                                               | Error error ->
                                                   Error ("tuple element 0"
                                                     :: error))
                                          | Error error ->
                                              Error ("tuple element 1" ::
                                                error))
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["Expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute coords" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_coords))[@explicit_arity ]) ->
                                (match Js.Dict.get dict "age" with
                                 | None ->
                                     ((Belt.Result.Error
                                         (["No attribute age"]))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun number ->
                                               match Js.Json.classify number
                                               with
                                               | ((JSONNumber
                                                   (number))[@explicit_arity
                                                              ])
                                                   ->
                                                   ((Belt.Result.Ok
                                                       ((int_of_float number)))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   ((Error
                                                       (["Expected a float"]))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              (("attribute age" :: error)))
                                          [@explicit_arity ])
                                      | ((Ok (attr_age))[@explicit_arity ])
                                          ->
                                          (match Js.Dict.get dict "name" with
                                           | None ->
                                               ((Belt.Result.Error
                                                   (["No attribute name"]))
                                               [@explicit_arity ])
                                           | ((Some
                                               (json))[@explicit_arity ]) ->
                                               (match (fun string ->
                                                         match Js.Json.classify
                                                                 string
                                                         with
                                                         | ((JSONString
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
                                                                 (["expected a string"]))
                                                             [@explicit_arity
                                                               ])) json
                                                with
                                                | ((Belt.Result.Error
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (("attribute name" ::
                                                          error)))
                                                    [@explicit_arity ])
                                                | ((Ok
                                                    (attr_name))[@explicit_arity
                                                                  ])
                                                    ->
                                                    Belt.Result.Ok
                                                      {
                                                        name = attr_name;
                                                        age = attr_age;
                                                        coords = attr_coords;
                                                        parents =
                                                          attr_parents
                                                      }))))))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and (deserialize_Types____pet :
      Js.Json.t -> (_Types__pet, string list) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Dog") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Dog : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__pet)
        | _ -> Error ["Expected an array"]
  end
module Version2 =
  struct
    type _Types__household =
      {
      people: _Types__person list ;
      pets: _Types__pet list }
    and _Types__person =
      {
      name: string ;
      age: float ;
      coords: (float * float) ;
      parents: (_Types__person * _Types__person) option }
    and _Types__pet = Version1._Types__pet =
      | Dog 
      | Cat 
    let rec (deserialize_Types____household :
      Js.Json.t -> (_Types__household, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "pets" with
             | None -> ((Belt.Result.Error (["No attribute pets"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun list ->
                           match Js.Json.classify list with
                           | ((JSONArray (items))[@explicit_arity ]) ->
                               let transformer = deserialize_Types____pet in
                               let rec loop i items =
                                 match items with
                                 | [] -> ((Belt.Result.Ok ([]))
                                     [@explicit_arity ])
                                 | one::rest ->
                                     (match transformer one with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              ((("list element " ^
                                                   (string_of_int i)) ::
                                                error)))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (value))[@explicit_arity ]) ->
                                          (match loop (i + 1) rest with
                                           | ((Belt.Result.Error
                                               (error))[@explicit_arity ]) ->
                                               ((Belt.Result.Error (error))
                                               [@explicit_arity ])
                                           | ((Belt.Result.Ok
                                               (rest))[@explicit_arity ]) ->
                                               ((Belt.Result.Ok
                                                   ((value :: rest)))
                                               [@explicit_arity ]))) in
                               loop 0 (Belt.List.fromArray items)
                           | _ ->
                               ((Belt.Result.Error (["expected an array"]))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute pets" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_pets))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "people" with
                       | None ->
                           ((Belt.Result.Error (["No attribute people"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types____person in
                                         let rec loop i items =
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
                                                        ((("list element " ^
                                                             (string_of_int i))
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop (i + 1) rest
                                                     with
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
                                         loop 0 (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute people" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_people))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { people = attr_people; pets = attr_pets }))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and (deserialize_Types____person :
      Js.Json.t -> (_Types__person, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "parents" with
             | None -> ((Belt.Result.Error (["No attribute parents"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match ((fun transformer ->
                            fun option ->
                              match Js.Json.classify option with
                              | JSONNull -> ((Belt.Result.Ok (None))
                                  [@explicit_arity ])
                              | _ ->
                                  (match transformer option with
                                   | ((Belt.Result.Error
                                       (error))[@explicit_arity ]) ->
                                       ((Belt.Result.Error
                                           (("optional value" :: error)))
                                       [@explicit_arity ])
                                   | ((Ok (value))[@explicit_arity ]) ->
                                       ((Ok
                                           (((Some (value))
                                             [@explicit_arity ])))
                                       [@explicit_arity ])))
                           (fun json ->
                              match Js.Json.classify json with
                              | ((JSONArray
                                  ([|arg0;arg1|]))[@explicit_arity ]) ->
                                  (match deserialize_Types____person arg1
                                   with
                                   | Belt.Result.Ok arg1 ->
                                       (match deserialize_Types____person
                                                arg0
                                        with
                                        | Belt.Result.Ok arg0 ->
                                            Belt.Result.Ok (arg0, arg1)
                                        | Error error ->
                                            Error ("tuple element 0" ::
                                              error))
                                   | Error error ->
                                       Error ("tuple element 1" :: error))
                              | _ ->
                                  ((Belt.Result.Error (["Expected an array"]))
                                  [@explicit_arity ]))) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute parents" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_parents))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "coords" with
                       | None ->
                           ((Belt.Result.Error (["No attribute coords"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun json ->
                                     match Js.Json.classify json with
                                     | ((JSONArray
                                         ([|arg0;arg1|]))[@explicit_arity ])
                                         ->
                                         (match (fun number ->
                                                   match Js.Json.classify
                                                           number
                                                   with
                                                   | ((JSONNumber
                                                       (number))[@explicit_arity
                                                                  ])
                                                       ->
                                                       ((Belt.Result.Ok
                                                           (number))
                                                       [@explicit_arity ])
                                                   | _ ->
                                                       ((Error
                                                           (["Expected a float"]))
                                                       [@explicit_arity ]))
                                                  arg1
                                          with
                                          | Belt.Result.Ok arg1 ->
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
                                                                (number))
                                                            [@explicit_arity
                                                              ])
                                                        | _ ->
                                                            ((Error
                                                                (["Expected a float"]))
                                                            [@explicit_arity
                                                              ])) arg0
                                               with
                                               | Belt.Result.Ok arg0 ->
                                                   Belt.Result.Ok
                                                     (arg0, arg1)
                                               | Error error ->
                                                   Error ("tuple element 0"
                                                     :: error))
                                          | Error error ->
                                              Error ("tuple element 1" ::
                                                error))
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["Expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute coords" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_coords))[@explicit_arity ]) ->
                                (match Js.Dict.get dict "age" with
                                 | None ->
                                     ((Belt.Result.Error
                                         (["No attribute age"]))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun number ->
                                               match Js.Json.classify number
                                               with
                                               | ((JSONNumber
                                                   (number))[@explicit_arity
                                                              ])
                                                   ->
                                                   ((Belt.Result.Ok (number))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   ((Error
                                                       (["Expected a float"]))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              (("attribute age" :: error)))
                                          [@explicit_arity ])
                                      | ((Ok (attr_age))[@explicit_arity ])
                                          ->
                                          (match Js.Dict.get dict "name" with
                                           | None ->
                                               ((Belt.Result.Error
                                                   (["No attribute name"]))
                                               [@explicit_arity ])
                                           | ((Some
                                               (json))[@explicit_arity ]) ->
                                               (match (fun string ->
                                                         match Js.Json.classify
                                                                 string
                                                         with
                                                         | ((JSONString
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
                                                                 (["expected a string"]))
                                                             [@explicit_arity
                                                               ])) json
                                                with
                                                | ((Belt.Result.Error
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (("attribute name" ::
                                                          error)))
                                                    [@explicit_arity ])
                                                | ((Ok
                                                    (attr_name))[@explicit_arity
                                                                  ])
                                                    ->
                                                    Belt.Result.Ok
                                                      {
                                                        name = attr_name;
                                                        age = attr_age;
                                                        coords = attr_coords;
                                                        parents =
                                                          attr_parents
                                                      }))))))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and deserialize_Types____pet = Version1.deserialize_Types____pet
    let rec migrate_Types____household =
      (fun _input_data ->
         let _converted_people =
           (Belt.List.map _input_data.people)
             (fun _item -> migrate_Types____person _item) in
         let _converted_pets =
           (Belt.List.map _input_data.pets)
             (fun _item -> migrate_Types____pet _item) in
         { pets = _converted_pets; people = _converted_people } : Version1._Types__household
                                                                    ->
                                                                    _Types__household)
    and migrate_Types____person =
      (fun _input_data ->
         let _converted_name = _input_data.name in
         let _converted_age =
           (fun person -> float_of_int (person.age * 7) : Version1._Types__person
                                                            -> float)
             _input_data in
         let _converted_coords =
           let (arg0, arg1) = _input_data.coords in (arg0, arg1) in
         let _converted_parents =
           match _input_data.parents with
           | None -> None
           | ((Some (_item))[@explicit_arity ]) ->
               ((Some
                   ((let (arg0, arg1) = _item in
                     ((migrate_Types____person arg0),
                       (migrate_Types____person arg1)))))
               [@explicit_arity ]) in
         {
           parents = _converted_parents;
           coords = _converted_coords;
           age = _converted_age;
           name = _converted_name
         } : Version1._Types__person -> _Types__person)
    and migrate_Types____pet =
      (fun _input_data -> _input_data : Version1._Types__pet -> _Types__pet)
  end
module Version3 =
  struct
    type _Types__household =
      {
      people: _Types__person list ;
      pets: _Types__pet list }
    and _Types__person = Types.person =
      {
      name: string ;
      age: float ;
      coords: (float * float) }
    and _Types__pet = Types.pet =
      | Dog 
      | Cat 
      | Mouse 
    let rec (deserialize_Types____household :
      Js.Json.t -> (_Types__household, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "pets" with
             | None -> ((Belt.Result.Error (["No attribute pets"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun list ->
                           match Js.Json.classify list with
                           | ((JSONArray (items))[@explicit_arity ]) ->
                               let transformer = deserialize_Types____pet in
                               let rec loop i items =
                                 match items with
                                 | [] -> ((Belt.Result.Ok ([]))
                                     [@explicit_arity ])
                                 | one::rest ->
                                     (match transformer one with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              ((("list element " ^
                                                   (string_of_int i)) ::
                                                error)))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (value))[@explicit_arity ]) ->
                                          (match loop (i + 1) rest with
                                           | ((Belt.Result.Error
                                               (error))[@explicit_arity ]) ->
                                               ((Belt.Result.Error (error))
                                               [@explicit_arity ])
                                           | ((Belt.Result.Ok
                                               (rest))[@explicit_arity ]) ->
                                               ((Belt.Result.Ok
                                                   ((value :: rest)))
                                               [@explicit_arity ]))) in
                               loop 0 (Belt.List.fromArray items)
                           | _ ->
                               ((Belt.Result.Error (["expected an array"]))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute pets" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_pets))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "people" with
                       | None ->
                           ((Belt.Result.Error (["No attribute people"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types____person in
                                         let rec loop i items =
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
                                                        ((("list element " ^
                                                             (string_of_int i))
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop (i + 1) rest
                                                     with
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
                                         loop 0 (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute people" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_people))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { people = attr_people; pets = attr_pets }))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and (deserialize_Types____person :
      Js.Json.t -> (_Types__person, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "coords" with
             | None -> ((Belt.Result.Error (["No attribute coords"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun json ->
                           match Js.Json.classify json with
                           | ((JSONArray ([|arg0;arg1|]))[@explicit_arity ])
                               ->
                               (match (fun number ->
                                         match Js.Json.classify number with
                                         | ((JSONNumber
                                             (number))[@explicit_arity ]) ->
                                             ((Belt.Result.Ok (number))
                                             [@explicit_arity ])
                                         | _ ->
                                             ((Error (["Expected a float"]))
                                             [@explicit_arity ])) arg1
                                with
                                | Belt.Result.Ok arg1 ->
                                    (match (fun number ->
                                              match Js.Json.classify number
                                              with
                                              | ((JSONNumber
                                                  (number))[@explicit_arity ])
                                                  ->
                                                  ((Belt.Result.Ok (number))
                                                  [@explicit_arity ])
                                              | _ ->
                                                  ((Error
                                                      (["Expected a float"]))
                                                  [@explicit_arity ])) arg0
                                     with
                                     | Belt.Result.Ok arg0 ->
                                         Belt.Result.Ok (arg0, arg1)
                                     | Error error ->
                                         Error ("tuple element 0" :: error))
                                | Error error ->
                                    Error ("tuple element 1" :: error))
                           | _ ->
                               ((Belt.Result.Error (["Expected an array"]))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute coords" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_coords))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "age" with
                       | None -> ((Belt.Result.Error (["No attribute age"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun number ->
                                     match Js.Json.classify number with
                                     | ((JSONNumber
                                         (number))[@explicit_arity ]) ->
                                         ((Belt.Result.Ok (number))
                                         [@explicit_arity ])
                                     | _ -> ((Error (["Expected a float"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute age" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_age))[@explicit_arity ]) ->
                                (match Js.Dict.get dict "name" with
                                 | None ->
                                     ((Belt.Result.Error
                                         (["No attribute name"]))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun string ->
                                               match Js.Json.classify string
                                               with
                                               | ((JSONString
                                                   (string))[@explicit_arity
                                                              ])
                                                   ->
                                                   ((Belt.Result.Ok (string))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   ((Error
                                                       (["expected a string"]))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              (("attribute name" :: error)))
                                          [@explicit_arity ])
                                      | ((Ok (attr_name))[@explicit_arity ])
                                          ->
                                          Belt.Result.Ok
                                            {
                                              name = attr_name;
                                              age = attr_age;
                                              coords = attr_coords
                                            }))))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and (deserialize_Types____pet :
      Js.Json.t -> (_Types__pet, string list) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Dog") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Dog : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : _Types__pet)
        | _ -> Error ["Expected an array"]
    let rec migrate_Types____household =
      (fun _input_data ->
         let _converted_people =
           (Belt.List.map _input_data.people)
             (fun _item -> migrate_Types____person _item) in
         let _converted_pets =
           (Belt.List.map _input_data.pets)
             (fun _item -> migrate_Types____pet _item) in
         { pets = _converted_pets; people = _converted_people } : Version2._Types__household
                                                                    ->
                                                                    _Types__household)
    and migrate_Types____person =
      (fun _input_data ->
         let _converted_name = _input_data.name in
         let _converted_age = _input_data.age in
         let _converted_coords =
           let (arg0, arg1) = _input_data.coords in (arg0, arg1) in
         {
           coords = _converted_coords;
           age = _converted_age;
           name = _converted_name
         } : Version2._Types__person -> _Types__person)
    and migrate_Types____pet =
      (fun _input_data -> match _input_data with | Dog -> Dog | Cat -> Cat : 
      Version2._Types__pet -> _Types__pet)
  end
module Version4 =
  struct
    type _Types__dogBreed =
      | Schnouser 
      | Lab 
      | Retriever 
      | Poodle 
    and _Types__household =
      {
      people: _Types__person list ;
      pets: _Types__pet list }
    and _Types__person = Types.person =
      {
      name: string ;
      age: float ;
      coords: (float * float) }
    and _Types__pet =
      | Dog of _Types__dogBreed option 
      | Cat 
      | Mouse 
    let rec (deserialize_Types____dogBreed :
      Js.Json.t -> (_Types__dogBreed, string list) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Schnouser") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Schnouser : _Types__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Lab") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Lab : _Types__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Retriever") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Retriever : _Types__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Poodle") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Poodle : _Types__dogBreed)
        | _ -> Error ["Expected an array"]
    and (deserialize_Types____household :
      Js.Json.t -> (_Types__household, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "pets" with
             | None -> ((Belt.Result.Error (["No attribute pets"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun list ->
                           match Js.Json.classify list with
                           | ((JSONArray (items))[@explicit_arity ]) ->
                               let transformer = deserialize_Types____pet in
                               let rec loop i items =
                                 match items with
                                 | [] -> ((Belt.Result.Ok ([]))
                                     [@explicit_arity ])
                                 | one::rest ->
                                     (match transformer one with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              ((("list element " ^
                                                   (string_of_int i)) ::
                                                error)))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (value))[@explicit_arity ]) ->
                                          (match loop (i + 1) rest with
                                           | ((Belt.Result.Error
                                               (error))[@explicit_arity ]) ->
                                               ((Belt.Result.Error (error))
                                               [@explicit_arity ])
                                           | ((Belt.Result.Ok
                                               (rest))[@explicit_arity ]) ->
                                               ((Belt.Result.Ok
                                                   ((value :: rest)))
                                               [@explicit_arity ]))) in
                               loop 0 (Belt.List.fromArray items)
                           | _ ->
                               ((Belt.Result.Error (["expected an array"]))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute pets" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_pets))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "people" with
                       | None ->
                           ((Belt.Result.Error (["No attribute people"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types____person in
                                         let rec loop i items =
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
                                                        ((("list element " ^
                                                             (string_of_int i))
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop (i + 1) rest
                                                     with
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
                                         loop 0 (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute people" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_people))[@explicit_arity ]) ->
                                Belt.Result.Ok
                                  { people = attr_people; pets = attr_pets }))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and deserialize_Types____person = Version3.deserialize_Types____person
    and (deserialize_Types____pet :
      Js.Json.t -> (_Types__pet, string list) Belt.Result.t) =
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
                                  ((Belt.Result.Error
                                      (("optional value" :: error)))
                                  [@explicit_arity ])
                              | ((Ok (value))[@explicit_arity ]) ->
                                  ((Ok (((Some (value))[@explicit_arity ])))
                                  [@explicit_arity ])))
                      deserialize_Types____dogBreed) arg0
             with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (Dog (arg0) : _Types__pet)
             | Error error -> Error ("constructor argument 0" :: error))
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : _Types__pet)
        | _ -> Error ["Expected an array"]
    let rec migrate_Types____household =
      (fun _input_data ->
         let _converted_people =
           (Belt.List.map _input_data.people)
             (fun _item -> migrate_Types____person _item) in
         let _converted_pets =
           (Belt.List.map _input_data.pets)
             (fun _item -> migrate_Types____pet _item) in
         { pets = _converted_pets; people = _converted_people } : Version3._Types__household
                                                                    ->
                                                                    _Types__household)
    and migrate_Types____person =
      (fun _input_data -> _input_data : Version3._Types__person ->
                                          _Types__person)
    and migrate_Types____pet =
      (fun _input_data ->
         match _input_data with
         | Dog -> ((Dog None)[@explicit_arity ])
         | Cat -> Cat
         | Mouse -> Mouse : Version3._Types__pet -> _Types__pet)
  end
module Version5 =
  struct
    type _Types__dogBreed =
      | Schnouser of string 
      | Lab 
      | Retriever 
      | Poodle 
    and _Types__household =
      {
      people: _Types__person list ;
      pets: _Types__pet list ;
      county: int _Types__named }
    and 'a _Types__named = {
      name: string ;
      contents: 'a }
    and _Types__person = Types.person =
      {
      name: string ;
      age: float ;
      coords: (float * float) }
    and _Types__pet =
      | Dog of _Types__dogBreed option 
      | Cat 
      | Mouse 
    let rec (deserialize_Types____dogBreed :
      Js.Json.t -> (_Types__dogBreed, string list) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag;arg0|] when
            (Js.Json.JSONString "Schnouser") = (Js.Json.classify tag) ->
            (match (fun string ->
                      match Js.Json.classify string with
                      | ((JSONString (string))[@explicit_arity ]) ->
                          ((Belt.Result.Ok (string))[@explicit_arity ])
                      | _ -> ((Error (["expected a string"]))
                          [@explicit_arity ])) arg0
             with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (Schnouser (arg0) : _Types__dogBreed)
             | Error error -> Error ("constructor argument 0" :: error))
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Lab") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Lab : _Types__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Retriever") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Retriever : _Types__dogBreed)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Poodle") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Poodle : _Types__dogBreed)
        | _ -> Error ["Expected an array"]
    and (deserialize_Types____household :
      Js.Json.t -> (_Types__household, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "county" with
             | None -> ((Belt.Result.Error (["No attribute county"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (deserialize_Types____named
                           (fun number ->
                              match Js.Json.classify number with
                              | ((JSONNumber (number))[@explicit_arity ]) ->
                                  ((Belt.Result.Ok ((int_of_float number)))
                                  [@explicit_arity ])
                              | _ -> ((Error (["Expected a float"]))
                                  [@explicit_arity ]))) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute county" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_county))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "pets" with
                       | None -> ((Belt.Result.Error (["No attribute pets"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types____pet in
                                         let rec loop i items =
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
                                                        ((("list element " ^
                                                             (string_of_int i))
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop (i + 1) rest
                                                     with
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
                                         loop 0 (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute pets" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_pets))[@explicit_arity ]) ->
                                (match Js.Dict.get dict "people" with
                                 | None ->
                                     ((Belt.Result.Error
                                         (["No attribute people"]))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun list ->
                                               match Js.Json.classify list
                                               with
                                               | ((JSONArray
                                                   (items))[@explicit_arity ])
                                                   ->
                                                   let transformer =
                                                     deserialize_Types____person in
                                                   let rec loop i items =
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
                                                                  (((
                                                                    "list element "
                                                                    ^
                                                                    (string_of_int
                                                                    i)) ::
                                                                    error)))
                                                              [@explicit_arity
                                                                ])
                                                          | ((Belt.Result.Ok
                                                              (value))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              (match 
                                                                 loop (
                                                                   i + 1)
                                                                   rest
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
                                                   loop 0
                                                     (Belt.List.fromArray
                                                        items)
                                               | _ ->
                                                   ((Belt.Result.Error
                                                       (["expected an array"]))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              (("attribute people" :: error)))
                                          [@explicit_arity ])
                                      | ((Ok
                                          (attr_people))[@explicit_arity ])
                                          ->
                                          Belt.Result.Ok
                                            {
                                              people = attr_people;
                                              pets = attr_pets;
                                              county = attr_county
                                            }))))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and deserialize_Types____named :
      'arg0 .
        (Js.Json.t -> ('arg0, string list) Belt.Result.t) ->
          Js.Json.t -> ('arg0 _Types__named, string list) Belt.Result.t
      =
      fun aTransformer ->
        fun record ->
          match Js.Json.classify record with
          | ((JSONObject (dict))[@explicit_arity ]) ->
              (match Js.Dict.get dict "contents" with
               | None -> ((Belt.Result.Error (["No attribute contents"]))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match aTransformer json with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (("attribute contents" :: error)))
                        [@explicit_arity ])
                    | ((Ok (attr_contents))[@explicit_arity ]) ->
                        (match Js.Dict.get dict "name" with
                         | None ->
                             ((Belt.Result.Error (["No attribute name"]))
                             [@explicit_arity ])
                         | ((Some (json))[@explicit_arity ]) ->
                             (match (fun string ->
                                       match Js.Json.classify string with
                                       | ((JSONString
                                           (string))[@explicit_arity ]) ->
                                           ((Belt.Result.Ok (string))
                                           [@explicit_arity ])
                                       | _ ->
                                           ((Error (["expected a string"]))
                                           [@explicit_arity ])) json
                              with
                              | ((Belt.Result.Error
                                  (error))[@explicit_arity ]) ->
                                  ((Belt.Result.Error
                                      (("attribute name" :: error)))
                                  [@explicit_arity ])
                              | ((Ok (attr_name))[@explicit_arity ]) ->
                                  Belt.Result.Ok
                                    {
                                      name = attr_name;
                                      contents = attr_contents
                                    }))))
          | _ -> ((Belt.Result.Error (["Expected an object"]))
              [@explicit_arity ])
    and deserialize_Types____person = Version4.deserialize_Types____person
    and (deserialize_Types____pet :
      Js.Json.t -> (_Types__pet, string list) Belt.Result.t) =
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
                                  ((Belt.Result.Error
                                      (("optional value" :: error)))
                                  [@explicit_arity ])
                              | ((Ok (value))[@explicit_arity ]) ->
                                  ((Ok (((Some (value))[@explicit_arity ])))
                                  [@explicit_arity ])))
                      deserialize_Types____dogBreed) arg0
             with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (Dog (arg0) : _Types__pet)
             | Error error -> Error ("constructor argument 0" :: error))
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : _Types__pet)
        | _ -> Error ["Expected an array"]
    let rec migrate_Types____dogBreed =
      (fun _input_data ->
         match _input_data with
         | Schnouser -> ((Schnouser "white")[@explicit_arity ])
         | Lab -> Lab
         | Retriever -> Retriever
         | Poodle -> Poodle : Version4._Types__dogBreed -> _Types__dogBreed)
    and migrate_Types____household =
      (fun _input_data ->
         let _converted_people =
           (Belt.List.map _input_data.people)
             (fun _item -> migrate_Types____person _item) in
         let _converted_pets =
           (Belt.List.map _input_data.pets)
             (fun _item -> migrate_Types____pet _item) in
         let _converted_county =
           (fun household -> { name = "Nowhere"; contents = 0 } : Version4._Types__household
                                                                    ->
                                                                    int
                                                                    _Types__named)
             _input_data in
         {
           county = _converted_county;
           pets = _converted_pets;
           people = _converted_people
         } : Version4._Types__household -> _Types__household)
    and migrate_Types____person =
      (fun _input_data -> _input_data : Version4._Types__person ->
                                          _Types__person)
    and migrate_Types____pet =
      (fun _input_data ->
         match _input_data with
         | Dog (arg0) ->
             Dog
               (((match arg0 with
                  | None -> None
                  | ((Some (_item))[@explicit_arity ]) ->
                      ((Some ((migrate_Types____dogBreed _item)))
                      [@explicit_arity ]))))
         | Cat -> Cat
         | Mouse -> Mouse : Version4._Types__pet -> _Types__pet)
  end
module Version6 =
  struct
    type _Types__household = Types.household =
      {
      people: _Types__person list ;
      pets: _Types__pet list ;
      visitors: _Types__person list ;
      county: int _Types__named }
    and 'a _Types__named = 'a Types.named =
      {
      name: string ;
      contents: 'a ;
      isClosed: bool }
    and _Types__person = Types.person =
      {
      name: string ;
      age: float ;
      coords: (float * float) }
    and _Types__pet = Types.pet =
      | Dog 
      | Cat 
      | Mouse 
    let rec (deserialize_Types____household :
      Js.Json.t -> (_Types__household, string list) Belt.Result.t) =
      fun record ->
        match Js.Json.classify record with
        | ((JSONObject (dict))[@explicit_arity ]) ->
            (match Js.Dict.get dict "county" with
             | None -> ((Belt.Result.Error (["No attribute county"]))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (deserialize_Types____named
                           (fun number ->
                              match Js.Json.classify number with
                              | ((JSONNumber (number))[@explicit_arity ]) ->
                                  ((Belt.Result.Ok ((int_of_float number)))
                                  [@explicit_arity ])
                              | _ -> ((Error (["Expected a float"]))
                                  [@explicit_arity ]))) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (("attribute county" :: error)))
                      [@explicit_arity ])
                  | ((Ok (attr_county))[@explicit_arity ]) ->
                      (match Js.Dict.get dict "visitors" with
                       | None ->
                           ((Belt.Result.Error (["No attribute visitors"]))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match Js.Json.classify list with
                                     | ((JSONArray
                                         (items))[@explicit_arity ]) ->
                                         let transformer =
                                           deserialize_Types____person in
                                         let rec loop i items =
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
                                                        ((("list element " ^
                                                             (string_of_int i))
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (value))[@explicit_arity
                                                              ])
                                                    ->
                                                    (match loop (i + 1) rest
                                                     with
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
                                         loop 0 (Belt.List.fromArray items)
                                     | _ ->
                                         ((Belt.Result.Error
                                             (["expected an array"]))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                ->
                                ((Belt.Result.Error
                                    (("attribute visitors" :: error)))
                                [@explicit_arity ])
                            | ((Ok (attr_visitors))[@explicit_arity ]) ->
                                (match Js.Dict.get dict "pets" with
                                 | None ->
                                     ((Belt.Result.Error
                                         (["No attribute pets"]))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun list ->
                                               match Js.Json.classify list
                                               with
                                               | ((JSONArray
                                                   (items))[@explicit_arity ])
                                                   ->
                                                   let transformer =
                                                     deserialize_Types____pet in
                                                   let rec loop i items =
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
                                                                  (((
                                                                    "list element "
                                                                    ^
                                                                    (string_of_int
                                                                    i)) ::
                                                                    error)))
                                                              [@explicit_arity
                                                                ])
                                                          | ((Belt.Result.Ok
                                                              (value))
                                                              [@explicit_arity
                                                                ])
                                                              ->
                                                              (match 
                                                                 loop (
                                                                   i + 1)
                                                                   rest
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
                                                   loop 0
                                                     (Belt.List.fromArray
                                                        items)
                                               | _ ->
                                                   ((Belt.Result.Error
                                                       (["expected an array"]))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error
                                              (("attribute pets" :: error)))
                                          [@explicit_arity ])
                                      | ((Ok (attr_pets))[@explicit_arity ])
                                          ->
                                          (match Js.Dict.get dict "people"
                                           with
                                           | None ->
                                               ((Belt.Result.Error
                                                   (["No attribute people"]))
                                               [@explicit_arity ])
                                           | ((Some
                                               (json))[@explicit_arity ]) ->
                                               (match (fun list ->
                                                         match Js.Json.classify
                                                                 list
                                                         with
                                                         | ((JSONArray
                                                             (items))
                                                             [@explicit_arity
                                                               ])
                                                             ->
                                                             let transformer
                                                               =
                                                               deserialize_Types____person in
                                                             let rec loop i
                                                               items =
                                                               match items
                                                               with
                                                               | [] ->
                                                                   ((
                                                                   Belt.Result.Ok
                                                                    ([]))
                                                                   [@explicit_arity
                                                                    ])
                                                               | one::rest ->
                                                                   (match 
                                                                    transformer
                                                                    one
                                                                    with
                                                                    | 
                                                                    ((Belt.Result.Error
                                                                    (error))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Error
                                                                    (((
                                                                    "list element "
                                                                    ^
                                                                    (string_of_int
                                                                    i)) ::
                                                                    error)))
                                                                    [@explicit_arity
                                                                    ])
                                                                    | 
                                                                    ((Belt.Result.Ok
                                                                    (value))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    (match 
                                                                    loop
                                                                    (i + 1)
                                                                    rest
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
                                                                    ((Belt.Result.Ok
                                                                    (rest))
                                                                    [@explicit_arity
                                                                    ]) ->
                                                                    ((Belt.Result.Ok
                                                                    ((value
                                                                    :: rest)))
                                                                    [@explicit_arity
                                                                    ]))) in
                                                             loop 0
                                                               (Belt.List.fromArray
                                                                  items)
                                                         | _ ->
                                                             ((Belt.Result.Error
                                                                 (["expected an array"]))
                                                             [@explicit_arity
                                                               ])) json
                                                with
                                                | ((Belt.Result.Error
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (("attribute people"
                                                          :: error)))
                                                    [@explicit_arity ])
                                                | ((Ok
                                                    (attr_people))[@explicit_arity
                                                                    ])
                                                    ->
                                                    Belt.Result.Ok
                                                      {
                                                        people = attr_people;
                                                        pets = attr_pets;
                                                        visitors =
                                                          attr_visitors;
                                                        county = attr_county
                                                      }))))))))
        | _ -> ((Belt.Result.Error (["Expected an object"]))
            [@explicit_arity ])
    and deserialize_Types____named :
      'arg0 .
        (Js.Json.t -> ('arg0, string list) Belt.Result.t) ->
          Js.Json.t -> ('arg0 _Types__named, string list) Belt.Result.t
      =
      fun aTransformer ->
        fun record ->
          match Js.Json.classify record with
          | ((JSONObject (dict))[@explicit_arity ]) ->
              (match Js.Dict.get dict "isClosed" with
               | None -> ((Belt.Result.Error (["No attribute isClosed"]))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match (fun bool ->
                             match Js.Json.classify bool with
                             | JSONTrue -> ((Belt.Result.Ok (true))
                                 [@explicit_arity ])
                             | JSONFalse -> ((Belt.Result.Ok (false))
                                 [@explicit_arity ])
                             | _ ->
                                 ((Belt.Result.Error (["Expected a bool"]))
                                 [@explicit_arity ])) json
                    with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (("attribute isClosed" :: error)))
                        [@explicit_arity ])
                    | ((Ok (attr_isClosed))[@explicit_arity ]) ->
                        (match Js.Dict.get dict "contents" with
                         | None ->
                             ((Belt.Result.Error (["No attribute contents"]))
                             [@explicit_arity ])
                         | ((Some (json))[@explicit_arity ]) ->
                             (match aTransformer json with
                              | ((Belt.Result.Error
                                  (error))[@explicit_arity ]) ->
                                  ((Belt.Result.Error
                                      (("attribute contents" :: error)))
                                  [@explicit_arity ])
                              | ((Ok (attr_contents))[@explicit_arity ]) ->
                                  (match Js.Dict.get dict "name" with
                                   | None ->
                                       ((Belt.Result.Error
                                           (["No attribute name"]))
                                       [@explicit_arity ])
                                   | ((Some (json))[@explicit_arity ]) ->
                                       (match (fun string ->
                                                 match Js.Json.classify
                                                         string
                                                 with
                                                 | ((JSONString
                                                     (string))[@explicit_arity
                                                                ])
                                                     ->
                                                     ((Belt.Result.Ok
                                                         (string))
                                                     [@explicit_arity ])
                                                 | _ ->
                                                     ((Error
                                                         (["expected a string"]))
                                                     [@explicit_arity ]))
                                                json
                                        with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error
                                                (("attribute name" :: error)))
                                            [@explicit_arity ])
                                        | ((Ok
                                            (attr_name))[@explicit_arity ])
                                            ->
                                            Belt.Result.Ok
                                              {
                                                name = attr_name;
                                                contents = attr_contents;
                                                isClosed = attr_isClosed
                                              }))))))
          | _ -> ((Belt.Result.Error (["Expected an object"]))
              [@explicit_arity ])
    and deserialize_Types____person = Version5.deserialize_Types____person
    and (deserialize_Types____pet :
      Js.Json.t -> (_Types__pet, string list) Belt.Result.t) =
      fun constructor ->
        match Js.Json.classify constructor with
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Dog") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Dog : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Cat") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Cat : _Types__pet)
        | JSONArray [|tag|] when
            (Js.Json.JSONString "Mouse") = (Js.Json.classify tag) ->
            Belt.Result.Ok (Mouse : _Types__pet)
        | _ -> Error ["Expected an array"]
    and (serialize_Types____household : _Types__household -> Js.Json.t) =
      fun record ->
        Js.Json.object_
          (Js.Dict.fromArray
             [|("people",
                 (((fun list ->
                      Js.Json.array
                        (Belt.List.toArray
                           (Belt.List.map list serialize_Types____person))))
                    record.people));("pets",
                                      (((fun list ->
                                           Js.Json.array
                                             (Belt.List.toArray
                                                (Belt.List.map list
                                                   serialize_Types____pet))))
                                         record.pets));("visitors",
                                                         (((fun list ->
                                                              Js.Json.array
                                                                (Belt.List.toArray
                                                                   (Belt.List.map
                                                                    list
                                                                    serialize_Types____person))))
                                                            record.visitors));
               ("county",
                 ((serialize_Types____named
                     (fun int -> Js.Json.number (float_of_int int)))
                    record.county))|])
    and serialize_Types____named :
      'arg0 . ('arg0 -> Js.Json.t) -> 'arg0 _Types__named -> Js.Json.t =
      fun aTransformer ->
        fun record ->
          Js.Json.object_
            (Js.Dict.fromArray
               [|("name", (Js.Json.string record.name));("contents",
                                                          (aTransformer
                                                             record.contents));
                 ("isClosed", (Js.Json.boolean record.isClosed))|])
    and (serialize_Types____person : _Types__person -> Js.Json.t) =
      fun record ->
        Js.Json.object_
          (Js.Dict.fromArray
             [|("name", (Js.Json.string record.name));("age",
                                                        (Js.Json.number
                                                           record.age));
               ("coords",
                 (((fun (arg0, arg1) ->
                      Js.Json.array
                        [|(Js.Json.number arg0);(Js.Json.number arg1)|]))
                    record.coords))|])
    and (serialize_Types____pet : _Types__pet -> Js.Json.t) =
      fun constructor ->
        match constructor with
        | Dog -> Js.Json.array [|(Js.Json.string "Dog")|]
        | Cat -> Js.Json.array [|(Js.Json.string "Cat")|]
        | Mouse -> Js.Json.array [|(Js.Json.string "Mouse")|]
    let rec migrate_Types____household =
      (fun _input_data ->
         let _converted_people =
           (Belt.List.map _input_data.people)
             (fun _item -> migrate_Types____person _item) in
         let _converted_pets =
           (Belt.List.map _input_data.pets)
             (fun _item -> migrate_Types____pet _item) in
         let _converted_visitors =
           (fun household -> [] : Version5._Types__household ->
                                    _Types__person list) _input_data in
         let _converted_county =
           migrate_Types____named (fun arg -> arg) _input_data.county in
         {
           county = _converted_county;
           visitors = _converted_visitors;
           pets = _converted_pets;
           people = _converted_people
         } : Version5._Types__household -> _Types__household)
    and migrate_Types____named =
      (fun contentsMigrator ->
         fun named ->
           {
             name = (named.name);
             contents = (contentsMigrator named.contents);
             isClosed = false
           } : ('a -> 'a_migrated) ->
                 'a Version5._Types__named -> 'a_migrated _Types__named)
    and migrate_Types____person =
      (fun _input_data -> _input_data : Version5._Types__person ->
                                          _Types__person)
    and migrate_Types____pet =
      (fun _input_data ->
         match _input_data with
         | ((Dog dogBreed)[@explicit_arity ]) -> Dog
         | Cat -> Cat
         | Mouse -> Mouse : Version5._Types__pet -> _Types__pet)
  end
let currentVersion = 6
let parseVersion json =
  match Js.Json.classify json with
  | ((JSONObject (dict))[@explicit_arity ]) ->
      (match Js.Dict.get dict "schemaVersion" with
       | ((Some (schemaVersion))[@explicit_arity ]) ->
           (match Js.Json.classify schemaVersion with
            | ((JSONNumber (version))[@explicit_arity ]) ->
                ((Belt.Result.Ok ((int_of_float version), json))
                [@implicit_arity ])
            | _ -> ((Belt.Result.Error ("Invalid schemaVersion"))
                [@explicit_arity ]))
       | None -> ((Belt.Result.Error ("No schemaVersion present"))
           [@explicit_arity ]))
  | ((JSONArray ([|version;payload|]))[@explicit_arity ]) ->
      (match Js.Json.classify version with
       | ((JSONNumber (version))[@explicit_arity ]) ->
           ((Belt.Result.Ok ((int_of_float version), payload))
           [@implicit_arity ])
       | _ -> ((Belt.Result.Error ("Invalid wrapped version"))
           [@explicit_arity ]))
  | _ -> ((Belt.Result.Error ("Must have a schema version"))
      [@explicit_arity ])
let wrapWithVersion version payload =
  match Js.Json.classify payload with
  | ((JSONObject (dict))[@explicit_arity ]) ->
      (Js.Dict.set dict "schemaVersion"
         (Js.Json.number (float_of_int version));
       Js.Json.object_ dict)
  | _ -> Js.Json.array [|(Js.Json.number (float_of_int version));payload|]
let serializeHousehold data =
  wrapWithVersion currentVersion (Version6.serialize_Types____household data)
and deserializeHousehold data =
  match parseVersion data with
  | ((Belt.Result.Error (err))[@explicit_arity ]) ->
      ((Belt.Result.Error ([err]))[@explicit_arity ])
  | ((Ok (version, data))[@implicit_arity ]) ->
      (match version with
       | 6 ->
           (match Version6.deserialize_Types____household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) -> ((Belt.Result.Ok (data))
                [@explicit_arity ]))
       | 5 ->
           (match Version5.deserialize_Types____household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                let data = Version6.migrate_Types____household data in
                ((Belt.Result.Ok (data))[@explicit_arity ]))
       | 4 ->
           (match Version4.deserialize_Types____household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                let data = Version5.migrate_Types____household data in
                let data = Version6.migrate_Types____household data in
                ((Belt.Result.Ok (data))[@explicit_arity ]))
       | 3 ->
           (match Version3.deserialize_Types____household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                let data = Version4.migrate_Types____household data in
                let data = Version5.migrate_Types____household data in
                let data = Version6.migrate_Types____household data in
                ((Belt.Result.Ok (data))[@explicit_arity ]))
       | 2 ->
           (match Version2.deserialize_Types____household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                let data = Version3.migrate_Types____household data in
                let data = Version4.migrate_Types____household data in
                let data = Version5.migrate_Types____household data in
                let data = Version6.migrate_Types____household data in
                ((Belt.Result.Ok (data))[@explicit_arity ]))
       | 1 ->
           (match Version1.deserialize_Types____household data with
            | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                ((Belt.Result.Error (error))[@explicit_arity ])
            | ((Ok (data))[@explicit_arity ]) ->
                let data = Version2.migrate_Types____household data in
                let data = Version3.migrate_Types____household data in
                let data = Version4.migrate_Types____household data in
                let data = Version5.migrate_Types____household data in
                let data = Version6.migrate_Types____household data in
                ((Belt.Result.Ok (data))[@explicit_arity ]))
       | _ ->
           ((Belt.Result.Error
               (["Unexpected version " ^ (string_of_int version)]))
           [@explicit_arity ]))