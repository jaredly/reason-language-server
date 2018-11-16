module V1_Locked =
  struct
    type _Analyze__TopTypes__moduleName = string
    and 'a _Asttypes__loc = 'a Asttypes.loc = {
      txt: 'a ;
      loc: _Location__t }
    and 'arg0 _Belt__Belt_HashMapInt__t = 'arg0 Belt__Belt_HashMapInt.t
    and _Location__t = Location.t =
      {
      loc_start: _Stdlib__lexing__position ;
      loc_end: _Stdlib__lexing__position ;
      loc_ghost: bool }
    and _Parsetree__attribute = (string _Asttypes__loc * _Parsetree__payload)
    and _Parsetree__attributes = _Parsetree__attribute list
    and _Parsetree__core_type = Parsetree.core_type
    and _Parsetree__expression = Parsetree.expression
    and _Parsetree__pattern = Parsetree.pattern
    and _Parsetree__payload = Parsetree.payload =
      | PStr of _Parsetree__structure 
      | PSig of _Parsetree__signature 
      | PTyp of _Parsetree__core_type 
      | PPat of _Parsetree__pattern * _Parsetree__expression option 
    and _Parsetree__signature = Parsetree.signature
    and _Parsetree__structure = Parsetree.structure
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
    and _Stdlib__lexing__position = Stdlib__lexing.position =
      {
      pos_fname: string ;
      pos_lnum: int ;
      pos_bol: int ;
      pos_cnum: int }
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
      type_: string ;
      publicName: string option }
    and _TypeMapSerde__Config__serializableLockfile =
      _TypeMap__DigTypes__shortReference
        _TypeMapSerde__Config__Locked__lockfile
    and _TypeMapSerde__Config__t = TypeMapSerde__Config.t =
      {
      version: int ;
      output: string ;
      engine: _TypeMapSerde__Config__engine ;
      entries: _TypeMapSerde__Config__entry list ;
      custom: _TypeMapSerde__Config__custom list }
    and 'reference _TypeMapSerde__Config__Locked__lockedConfig =
      'reference TypeMapSerde__Config.Locked.lockedConfig =
      {
      entries: _TypeMapSerde__Config__Locked__lockedEntry list ;
      engineVersion: int ;
      typeMap: 'reference _TypeMap__DigTypes__typeMap }
    and _TypeMapSerde__Config__Locked__lockedEntry =
      TypeMapSerde__Config.Locked.lockedEntry =
      {
      moduleName: string ;
      modulePath: string list ;
      name: string }
    and 'reference _TypeMapSerde__Config__Locked__lockfile =
      'reference TypeMapSerde__Config.Locked.lockfile =
      {
      engine: _TypeMapSerde__Config__engine ;
      versions: 'reference _TypeMapSerde__Config__Locked__lockedConfig array }
    and _TypeMap__DigTypes__shortReference =
      (_Analyze__TopTypes__moduleName * string list * string)
    and 'reference _TypeMap__DigTypes__typeMap =
      (_TypeMap__DigTypes__shortReference,
        (_Parsetree__attributes * 'reference _TypeMap__DigTypes__typeSource
          _SharedTypes__SimpleType__declaration))
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
    and deserialize_Asttypes____loc :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t -> ('arg0 Asttypes.loc, string) Belt.Result.t
      =
      fun aTransformer ->
        fun record ->
          match record with
          | ((Json.Object (items))[@explicit_arity ]) ->
              (match Belt.List.getAssoc items "loc" (=) with
               | None -> ((Belt.Result.Error (("No attribute " ^ "loc")))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match deserialize_Location____t json with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (error))[@explicit_arity ])
                    | ((Belt.Result.Ok (attr_loc))[@explicit_arity ]) ->
                        (match Belt.List.getAssoc items "txt" (=) with
                         | None ->
                             ((Belt.Result.Error (("No attribute " ^ "txt")))
                             [@explicit_arity ])
                         | ((Some (json))[@explicit_arity ]) ->
                             (match aTransformer json with
                              | ((Belt.Result.Error
                                  (error))[@explicit_arity ]) ->
                                  ((Belt.Result.Error (error))
                                  [@explicit_arity ])
                              | ((Belt.Result.Ok
                                  (attr_txt))[@explicit_arity ]) ->
                                  Belt.Result.Ok
                                    { txt = attr_txt; loc = attr_loc }))))
          | _ -> ((Belt.Result.Error ("Expected an object"))
              [@explicit_arity ])
    and deserialize_Belt__Belt_HashMapInt____t :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t -> ('arg0 Belt__Belt_HashMapInt.t, string) Belt.Result.t
      =
      fun arg0Transformer ->
        TransformHelpers.deserialize_Belt__Belt_HashMapInt____t
          arg0Transformer
    and (deserialize_Location____t :
      Json.t -> (Location.t, string) Belt.Result.t) =
      fun record ->
        match record with
        | ((Json.Object (items))[@explicit_arity ]) ->
            (match Belt.List.getAssoc items "loc_ghost" (=) with
             | None -> ((Belt.Result.Error (("No attribute " ^ "loc_ghost")))
                 [@explicit_arity ])
             | ((Some (json))[@explicit_arity ]) ->
                 (match (fun bool ->
                           match bool with
                           | Json.True -> ((Belt.Result.Ok (true))
                               [@explicit_arity ])
                           | Json.False -> ((Belt.Result.Ok (false))
                               [@explicit_arity ])
                           | _ -> ((Belt.Result.Error ("Expected a bool"))
                               [@explicit_arity ])) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Belt.Result.Ok (attr_loc_ghost))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "loc_end" (=) with
                       | None ->
                           ((Belt.Result.Error
                               (("No attribute " ^ "loc_end")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match deserialize_Stdlib__lexing____position json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Belt.Result.Ok
                                (attr_loc_end))[@explicit_arity ]) ->
                                (match Belt.List.getAssoc items "loc_start"
                                         (=)
                                 with
                                 | None ->
                                     ((Belt.Result.Error
                                         (("No attribute " ^ "loc_start")))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match deserialize_Stdlib__lexing____position
                                              json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (attr_loc_start))[@explicit_arity ])
                                          ->
                                          Belt.Result.Ok
                                            {
                                              loc_start = attr_loc_start;
                                              loc_end = attr_loc_end;
                                              loc_ghost = attr_loc_ghost
                                            }))))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and (deserialize_Parsetree____attribute :
      Json.t -> (Parsetree.attribute, string) Belt.Result.t) =
      fun value ->
        (fun json ->
           match json with
           | ((Json.Array (arg0::arg1::[]))[@explicit_arity ]) ->
               (match deserialize_Parsetree____payload arg1 with
                | Belt.Result.Ok arg1 ->
                    (match (deserialize_Asttypes____loc
                              (fun string ->
                                 match string with
                                 | ((Json.String (string))[@explicit_arity ])
                                     -> ((Belt.Result.Ok (string))
                                     [@explicit_arity ])
                                 | _ -> ((Error ("epected a string"))
                                     [@explicit_arity ]))) arg0
                     with
                     | Belt.Result.Ok arg0 -> Belt.Result.Ok (arg0, arg1)
                     | Error error -> Error error)
                | Error error -> Error error)
           | _ -> ((Belt.Result.Error ("Expected array"))[@explicit_arity ]))
          value
    and (deserialize_Parsetree____attributes :
      Json.t -> (Parsetree.attributes, string) Belt.Result.t) =
      fun value ->
        (fun list ->
           match list with
           | ((Json.Array (items))[@explicit_arity ]) ->
               let transformer = deserialize_Parsetree____attribute in
               let rec loop items =
                 match items with
                 | [] -> ((Belt.Result.Ok ([]))[@explicit_arity ])
                 | one::rest ->
                     (match transformer one with
                      | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                          ((Belt.Result.Error (error))[@explicit_arity ])
                      | ((Belt.Result.Ok (value))[@explicit_arity ]) ->
                          (match loop rest with
                           | ((Belt.Result.Error (error))[@explicit_arity ])
                               -> ((Belt.Result.Error (error))
                               [@explicit_arity ])
                           | ((Belt.Result.Ok (rest))[@explicit_arity ]) ->
                               ((Belt.Result.Ok ((value :: rest)))
                               [@explicit_arity ]))) in
               loop items
           | _ -> ((Belt.Result.Error ("expected an array"))
               [@explicit_arity ])) value
    and (deserialize_Parsetree____core_type :
      Json.t -> (Parsetree.core_type, string) Belt.Result.t) =
      TransformHelpers.deserialize_Parsetree____core_type
    and (deserialize_Parsetree____expression :
      Json.t -> (Parsetree.expression, string) Belt.Result.t) =
      TransformHelpers.deserialize_Parsetree____expression
    and (deserialize_Parsetree____pattern :
      Json.t -> (Parsetree.pattern, string) Belt.Result.t) =
      TransformHelpers.deserialize_Parsetree____pattern
    and (deserialize_Parsetree____payload :
      Json.t -> (Parsetree.payload, string) Belt.Result.t) =
      fun constructor ->
        match constructor with
        | Json.Array (tag::arg0::[]) when (Json.String "PStr") = tag ->
            (match deserialize_Parsetree____structure arg0 with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (PStr (arg0) : Parsetree.payload)
             | Error error -> Error error)
        | Json.Array (tag::arg0::[]) when (Json.String "PSig") = tag ->
            (match deserialize_Parsetree____signature arg0 with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (PSig (arg0) : Parsetree.payload)
             | Error error -> Error error)
        | Json.Array (tag::arg0::[]) when (Json.String "PTyp") = tag ->
            (match deserialize_Parsetree____core_type arg0 with
             | Belt.Result.Ok arg0 ->
                 Belt.Result.Ok (PTyp (arg0) : Parsetree.payload)
             | Error error -> Error error)
        | Json.Array (tag::arg0::arg1::[]) when (Json.String "PPat") = tag ->
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
                              | ((Belt.Result.Ok (value))[@explicit_arity ])
                                  ->
                                  ((Belt.Result.Ok
                                      (((Some (value))[@explicit_arity ])))
                                  [@explicit_arity ])))
                      deserialize_Parsetree____expression) arg1
             with
             | Belt.Result.Ok arg1 ->
                 (match deserialize_Parsetree____pattern arg0 with
                  | Belt.Result.Ok arg0 ->
                      Belt.Result.Ok (PPat (arg0, arg1) : Parsetree.payload)
                  | Error error -> Error error)
             | Error error -> Error error)
        | _ -> Error "Expected an array"
    and (deserialize_Parsetree____signature :
      Json.t -> (Parsetree.signature, string) Belt.Result.t) =
      TransformHelpers.deserialize_Parsetree____signature
    and (deserialize_Parsetree____structure :
      Json.t -> (Parsetree.structure, string) Belt.Result.t) =
      TransformHelpers.deserialize_Parsetree____structure
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
    and (deserialize_Stdlib__lexing____position :
      Json.t -> (Stdlib__lexing.position, string) Belt.Result.t) =
      fun record ->
        match record with
        | ((Json.Object (items))[@explicit_arity ]) ->
            (match Belt.List.getAssoc items "pos_cnum" (=) with
             | None -> ((Belt.Result.Error (("No attribute " ^ "pos_cnum")))
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
                  | ((Belt.Result.Ok (attr_pos_cnum))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "pos_bol" (=) with
                       | None ->
                           ((Belt.Result.Error
                               (("No attribute " ^ "pos_bol")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun number ->
                                     match number with
                                     | ((Json.Number
                                         (number))[@explicit_arity ]) ->
                                         ((Belt.Result.Ok
                                             ((int_of_float number)))
                                         [@explicit_arity ])
                                     | _ -> ((Error ("Expected a float"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Belt.Result.Ok
                                (attr_pos_bol))[@explicit_arity ]) ->
                                (match Belt.List.getAssoc items "pos_lnum"
                                         (=)
                                 with
                                 | None ->
                                     ((Belt.Result.Error
                                         (("No attribute " ^ "pos_lnum")))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun number ->
                                               match number with
                                               | ((Json.Number
                                                   (number))[@explicit_arity
                                                              ])
                                                   ->
                                                   ((Belt.Result.Ok
                                                       ((int_of_float number)))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   ((Error
                                                       ("Expected a float"))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (attr_pos_lnum))[@explicit_arity ])
                                          ->
                                          (match Belt.List.getAssoc items
                                                   "pos_fname" (=)
                                           with
                                           | None ->
                                               ((Belt.Result.Error
                                                   (("No attribute " ^
                                                       "pos_fname")))
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
                                                    (attr_pos_fname))
                                                    [@explicit_arity ]) ->
                                                    Belt.Result.Ok
                                                      {
                                                        pos_fname =
                                                          attr_pos_fname;
                                                        pos_lnum =
                                                          attr_pos_lnum;
                                                        pos_bol =
                                                          attr_pos_bol;
                                                        pos_cnum =
                                                          attr_pos_cnum
                                                      }))))))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
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
            (match Belt.List.getAssoc items "publicName" (=) with
             | None ->
                 ((Belt.Result.Error (("No attribute " ^ "publicName")))
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
                                   | ((Belt.Result.Ok
                                       (value))[@explicit_arity ]) ->
                                       ((Belt.Result.Ok
                                           (((Some (value))
                                             [@explicit_arity ])))
                                       [@explicit_arity ])))
                           (fun string ->
                              match string with
                              | ((Json.String (string))[@explicit_arity ]) ->
                                  ((Belt.Result.Ok (string))
                                  [@explicit_arity ])
                              | _ -> ((Error ("epected a string"))
                                  [@explicit_arity ]))) json
                  with
                  | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                      ((Belt.Result.Error (error))[@explicit_arity ])
                  | ((Belt.Result.Ok (attr_publicName))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "type_" (=) with
                       | None ->
                           ((Belt.Result.Error (("No attribute " ^ "type_")))
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
                                (attr_type_))[@explicit_arity ]) ->
                                (match Belt.List.getAssoc items "file" (=)
                                 with
                                 | None ->
                                     ((Belt.Result.Error
                                         (("No attribute " ^ "file")))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun string ->
                                               match string with
                                               | ((Json.String
                                                   (string))[@explicit_arity
                                                              ])
                                                   ->
                                                   ((Belt.Result.Ok (string))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   ((Error
                                                       ("epected a string"))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (attr_file))[@explicit_arity ]) ->
                                          Belt.Result.Ok
                                            {
                                              file = attr_file;
                                              type_ = attr_type_;
                                              publicName = attr_publicName
                                            }))))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and (deserialize_TypeMapSerde__Config____serializableLockfile :
      Json.t ->
        (TypeMapSerde__Config.serializableLockfile, string) Belt.Result.t)
      =
      fun value ->
        (deserialize_TypeMapSerde__Config__Locked__lockfile
           deserialize_TypeMap__DigTypes____shortReference) value
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
    and deserialize_TypeMapSerde__Config__Locked__lockedConfig :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t ->
            ('arg0 TypeMapSerde__Config.Locked.lockedConfig, string)
              Belt.Result.t
      =
      fun referenceTransformer ->
        fun record ->
          match record with
          | ((Json.Object (items))[@explicit_arity ]) ->
              (match Belt.List.getAssoc items "typeMap" (=) with
               | None -> ((Belt.Result.Error (("No attribute " ^ "typeMap")))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match (deserialize_TypeMap__DigTypes____typeMap
                             referenceTransformer) json
                    with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (error))[@explicit_arity ])
                    | ((Belt.Result.Ok (attr_typeMap))[@explicit_arity ]) ->
                        (match Belt.List.getAssoc items "engineVersion" (=)
                         with
                         | None ->
                             ((Belt.Result.Error
                                 (("No attribute " ^ "engineVersion")))
                             [@explicit_arity ])
                         | ((Some (json))[@explicit_arity ]) ->
                             (match (fun number ->
                                       match number with
                                       | ((Json.Number
                                           (number))[@explicit_arity ]) ->
                                           ((Belt.Result.Ok
                                               ((int_of_float number)))
                                           [@explicit_arity ])
                                       | _ -> ((Error ("Expected a float"))
                                           [@explicit_arity ])) json
                              with
                              | ((Belt.Result.Error
                                  (error))[@explicit_arity ]) ->
                                  ((Belt.Result.Error (error))
                                  [@explicit_arity ])
                              | ((Belt.Result.Ok
                                  (attr_engineVersion))[@explicit_arity ]) ->
                                  (match Belt.List.getAssoc items "entries"
                                           (=)
                                   with
                                   | None ->
                                       ((Belt.Result.Error
                                           (("No attribute " ^ "entries")))
                                       [@explicit_arity ])
                                   | ((Some (json))[@explicit_arity ]) ->
                                       (match (fun list ->
                                                 match list with
                                                 | ((Json.Array
                                                     (items))[@explicit_arity
                                                               ])
                                                     ->
                                                     let transformer =
                                                       deserialize_TypeMapSerde__Config__Locked__lockedEntry in
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
                                                json
                                        with
                                        | ((Belt.Result.Error
                                            (error))[@explicit_arity ]) ->
                                            ((Belt.Result.Error (error))
                                            [@explicit_arity ])
                                        | ((Belt.Result.Ok
                                            (attr_entries))[@explicit_arity ])
                                            ->
                                            Belt.Result.Ok
                                              {
                                                entries = attr_entries;
                                                engineVersion =
                                                  attr_engineVersion;
                                                typeMap = attr_typeMap
                                              }))))))
          | _ -> ((Belt.Result.Error ("Expected an object"))
              [@explicit_arity ])
    and (deserialize_TypeMapSerde__Config__Locked__lockedEntry :
      Json.t ->
        (TypeMapSerde__Config.Locked.lockedEntry, string) Belt.Result.t)
      =
      fun record ->
        match record with
        | ((Json.Object (items))[@explicit_arity ]) ->
            (match Belt.List.getAssoc items "name" (=) with
             | None -> ((Belt.Result.Error (("No attribute " ^ "name")))
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
                  | ((Belt.Result.Ok (attr_name))[@explicit_arity ]) ->
                      (match Belt.List.getAssoc items "modulePath" (=) with
                       | None ->
                           ((Belt.Result.Error
                               (("No attribute " ^ "modulePath")))
                           [@explicit_arity ])
                       | ((Some (json))[@explicit_arity ]) ->
                           (match (fun list ->
                                     match list with
                                     | ((Json.Array
                                         (items))[@explicit_arity ]) ->
                                         let transformer string =
                                           match string with
                                           | ((Json.String
                                               (string))[@explicit_arity ])
                                               -> ((Belt.Result.Ok (string))
                                               [@explicit_arity ])
                                           | _ ->
                                               ((Error ("epected a string"))
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
                                             ("expected an array"))
                                         [@explicit_arity ])) json
                            with
                            | ((Belt.Result.Error (error))[@explicit_arity ])
                                -> ((Belt.Result.Error (error))
                                [@explicit_arity ])
                            | ((Belt.Result.Ok
                                (attr_modulePath))[@explicit_arity ]) ->
                                (match Belt.List.getAssoc items "moduleName"
                                         (=)
                                 with
                                 | None ->
                                     ((Belt.Result.Error
                                         (("No attribute " ^ "moduleName")))
                                     [@explicit_arity ])
                                 | ((Some (json))[@explicit_arity ]) ->
                                     (match (fun string ->
                                               match string with
                                               | ((Json.String
                                                   (string))[@explicit_arity
                                                              ])
                                                   ->
                                                   ((Belt.Result.Ok (string))
                                                   [@explicit_arity ])
                                               | _ ->
                                                   ((Error
                                                       ("epected a string"))
                                                   [@explicit_arity ])) json
                                      with
                                      | ((Belt.Result.Error
                                          (error))[@explicit_arity ]) ->
                                          ((Belt.Result.Error (error))
                                          [@explicit_arity ])
                                      | ((Belt.Result.Ok
                                          (attr_moduleName))[@explicit_arity
                                                              ])
                                          ->
                                          Belt.Result.Ok
                                            {
                                              moduleName = attr_moduleName;
                                              modulePath = attr_modulePath;
                                              name = attr_name
                                            }))))))
        | _ -> ((Belt.Result.Error ("Expected an object"))[@explicit_arity ])
    and deserialize_TypeMapSerde__Config__Locked__lockfile :
      'arg0 .
        (Json.t -> ('arg0, string) Belt.Result.t) ->
          Json.t ->
            ('arg0 TypeMapSerde__Config.Locked.lockfile, string)
              Belt.Result.t
      =
      fun referenceTransformer ->
        fun record ->
          match record with
          | ((Json.Object (items))[@explicit_arity ]) ->
              (match Belt.List.getAssoc items "versions" (=) with
               | None ->
                   ((Belt.Result.Error (("No attribute " ^ "versions")))
                   [@explicit_arity ])
               | ((Some (json))[@explicit_arity ]) ->
                   (match ((fun transformer ->
                              fun array ->
                                match array with
                                | ((Json.Array (items))[@explicit_arity ]) ->
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
                                                    (error))[@explicit_arity
                                                              ])
                                                    ->
                                                    ((Belt.Result.Error
                                                        (error))
                                                    [@explicit_arity ])
                                                | ((Belt.Result.Ok
                                                    (rest))[@explicit_arity ])
                                                    ->
                                                    ((Belt.Result.Ok
                                                        ((value :: rest)))
                                                    [@explicit_arity ]))) in
                                    (match loop items
                                     with
                                     | ((Belt.Result.Error
                                         (error))[@explicit_arity ]) ->
                                         ((Belt.Result.Error (error))
                                         [@explicit_arity ])
                                     | ((Belt.Result.Ok
                                         (value))[@explicit_arity ]) ->
                                         ((Belt.Result.Ok
                                             ((Belt.List.toArray value)))
                                         [@explicit_arity ]))
                                | _ ->
                                    ((Belt.Result.Error ("expected an array"))
                                    [@explicit_arity ]))
                             (deserialize_TypeMapSerde__Config__Locked__lockedConfig
                                referenceTransformer)) json
                    with
                    | ((Belt.Result.Error (error))[@explicit_arity ]) ->
                        ((Belt.Result.Error (error))[@explicit_arity ])
                    | ((Belt.Result.Ok (attr_versions))[@explicit_arity ]) ->
                        (match Belt.List.getAssoc items "engine" (=) with
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
                                  (attr_engine))[@explicit_arity ]) ->
                                  Belt.Result.Ok
                                    {
                                      engine = attr_engine;
                                      versions = attr_versions
                                    }))))
          | _ -> ((Belt.Result.Error ("Expected an object"))
              [@explicit_arity ])
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
             (fun json ->
                match json with
                | ((Json.Array (arg0::arg1::[]))[@explicit_arity ]) ->
                    (match (deserialize_SharedTypes__SimpleType__declaration
                              (deserialize_TypeMap__DigTypes____typeSource
                                 referenceTransformer)) arg1
                     with
                     | Belt.Result.Ok arg1 ->
                         (match deserialize_Parsetree____attributes arg0 with
                          | Belt.Result.Ok arg0 ->
                              Belt.Result.Ok (arg0, arg1)
                          | Error error -> Error error)
                     | Error error -> Error error)
                | _ -> ((Belt.Result.Error ("Expected array"))
                    [@explicit_arity ]))) value
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
    and serialize_Asttypes____loc :
      'arg0 . ('arg0 -> Json.t) -> 'arg0 Asttypes.loc -> Json.t =
      fun aTransformer ->
        fun record ->
          Json.Object
            [("txt", (aTransformer record.txt));
            ("loc", (serialize_Location____t record.loc))]
    and serialize_Belt__Belt_HashMapInt____t :
      'arg0 . ('arg0 -> Json.t) -> 'arg0 Belt__Belt_HashMapInt.t -> Json.t =
      fun arg0Transformer ->
        TransformHelpers.serialize_Belt__Belt_HashMapInt____t arg0Transformer
    and (serialize_Location____t : Location.t -> Json.t) =
      fun record ->
        Json.Object
          [("loc_start",
             (serialize_Stdlib__lexing____position record.loc_start));
          ("loc_end", (serialize_Stdlib__lexing____position record.loc_end));
          ("loc_ghost",
            (((fun b ->
                 match b with | true -> Json.True | false -> Json.False))
               record.loc_ghost))]
    and (serialize_Parsetree____attribute : Parsetree.attribute -> Json.t) =
      fun value ->
        (fun (arg0, arg1) ->
           Json.Array
             [(serialize_Asttypes____loc
                 (fun s -> ((Json.String (s))[@explicit_arity ]))) arg0;
             serialize_Parsetree____payload arg1]) value
    and (serialize_Parsetree____attributes : Parsetree.attributes -> Json.t)
      =
      fun value ->
        (fun list ->
           Json.Array (Belt.List.map list serialize_Parsetree____attribute))
          value
    and (serialize_Parsetree____core_type : Parsetree.core_type -> Json.t) =
      TransformHelpers.serialize_Parsetree____core_type
    and (serialize_Parsetree____expression : Parsetree.expression -> Json.t)
      = TransformHelpers.serialize_Parsetree____expression
    and (serialize_Parsetree____pattern : Parsetree.pattern -> Json.t) =
      TransformHelpers.serialize_Parsetree____pattern
    and (serialize_Parsetree____payload : Parsetree.payload -> Json.t) =
      fun constructor ->
        match constructor with
        | PStr arg0 ->
            Json.Array
              [Json.String "PStr"; serialize_Parsetree____structure arg0]
        | PSig arg0 ->
            Json.Array
              [Json.String "PSig"; serialize_Parsetree____signature arg0]
        | PTyp arg0 ->
            Json.Array
              [Json.String "PTyp"; serialize_Parsetree____core_type arg0]
        | PPat (arg0, arg1) ->
            Json.Array
              [Json.String "PPat";
              serialize_Parsetree____pattern arg0;
              (((fun transformer ->
                   function
                   | None -> Json.Null
                   | ((Some (v))[@explicit_arity ]) -> transformer v))
                 serialize_Parsetree____expression) arg1]
    and (serialize_Parsetree____signature : Parsetree.signature -> Json.t) =
      TransformHelpers.serialize_Parsetree____signature
    and (serialize_Parsetree____structure : Parsetree.structure -> Json.t) =
      TransformHelpers.serialize_Parsetree____structure
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
    and (serialize_Stdlib__lexing____position :
      Stdlib__lexing.position -> Json.t) =
      fun record ->
        Json.Object
          [("pos_fname",
             (((fun s -> ((Json.String (s))[@explicit_arity ])))
                record.pos_fname));
          ("pos_lnum",
            (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
               record.pos_lnum));
          ("pos_bol",
            (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
               record.pos_bol));
          ("pos_cnum",
            (((fun i -> ((Json.Number ((float_of_int i)))[@explicit_arity ])))
               record.pos_cnum))]
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
            (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.type_));
          ("publicName",
            ((((fun transformer ->
                  function
                  | None -> Json.Null
                  | ((Some (v))[@explicit_arity ]) -> transformer v))
                (fun s -> ((Json.String (s))[@explicit_arity ])))
               record.publicName))]
    and (serialize_TypeMapSerde__Config____serializableLockfile :
      TypeMapSerde__Config.serializableLockfile -> Json.t) =
      fun value ->
        (serialize_TypeMapSerde__Config__Locked__lockfile
           serialize_TypeMap__DigTypes____shortReference) value
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
    and serialize_TypeMapSerde__Config__Locked__lockedConfig :
      'arg0 .
        ('arg0 -> Json.t) ->
          'arg0 TypeMapSerde__Config.Locked.lockedConfig -> Json.t
      =
      fun referenceTransformer ->
        fun record ->
          Json.Object
            [("entries",
               (((fun list ->
                    Json.Array
                      (Belt.List.map list
                         serialize_TypeMapSerde__Config__Locked__lockedEntry)))
                  record.entries));
            ("engineVersion",
              (((fun i -> ((Json.Number ((float_of_int i)))
                   [@explicit_arity ]))) record.engineVersion));
            ("typeMap",
              ((serialize_TypeMap__DigTypes____typeMap referenceTransformer)
                 record.typeMap))]
    and (serialize_TypeMapSerde__Config__Locked__lockedEntry :
      TypeMapSerde__Config.Locked.lockedEntry -> Json.t) =
      fun record ->
        Json.Object
          [("moduleName",
             (((fun s -> ((Json.String (s))[@explicit_arity ])))
                record.moduleName));
          ("modulePath",
            (((fun list ->
                 Json.Array
                   (Belt.List.map list
                      (fun s -> ((Json.String (s))[@explicit_arity ])))))
               record.modulePath));
          ("name",
            (((fun s -> ((Json.String (s))[@explicit_arity ]))) record.name))]
    and serialize_TypeMapSerde__Config__Locked__lockfile :
      'arg0 .
        ('arg0 -> Json.t) ->
          'arg0 TypeMapSerde__Config.Locked.lockfile -> Json.t
      =
      fun referenceTransformer ->
        fun record ->
          Json.Object
            [("engine",
               (serialize_TypeMapSerde__Config____engine record.engine));
            ("versions",
              ((((fun transformer ->
                    fun array ->
                      Json.Array (Belt.List.map (Belt.List.fromArray array) transformer)))
                  (serialize_TypeMapSerde__Config__Locked__lockedConfig
                     referenceTransformer)) record.versions))]
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
             (fun (arg0, arg1) ->
                Json.Array
                  [serialize_Parsetree____attributes arg0;
                  (serialize_SharedTypes__SimpleType__declaration
                     (serialize_TypeMap__DigTypes____typeSource
                        referenceTransformer)) arg1])) value
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
let serializeSerializableLockfile =
  SerializeRaw.serialize_TypeMapSerde__Config____serializableLockfile
and deserializeSerializableLockfile =
  DeserializeRaw.deserialize_TypeMapSerde__Config____serializableLockfile
let serializeT = SerializeRaw.serialize_TypeMapSerde__Config____t
and deserializeT = DeserializeRaw.deserialize_TypeMapSerde__Config____t
include SerializeRaw
include DeserializeRaw