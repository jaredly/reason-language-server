/*

 Ok folks, what I think I want is ...
 to compute for the whole file and then cache that.
 Also that way I can better handle definitions

 What will come out of this?
 A mapping of stamp -> (location, type, option(docs))
 And toplevelname -> stamp
 andddd maybe that's it?
 Oh right, a list of [loc, type, path] for the hover bit
 and probably a happing of stamp -> list(loc) of references

 umm I also want open mapping

 also thinking about providing rename functionality, and "find references"


 err what about stamps that are modules?
 maybe have a separate map for that?

 */
type item =
  | Module(list((string, int)))
  /* | ModuleAlias(Path.t) */
  | Type(Types.type_declaration)
  | Value(Types.type_expr);

type definition =
  | Path(Path.t)
  /* | Location(Location.t) */
  | Constructor(Path.t, string, Location.t)
  | Attribute(Path.t, string, Location.t)
  | IsDefinition;

type moduleData = {
  stamps: Hashtbl.t(int, (string, Location.t, item, option(string))),
  /* TODO track constructor names, and record attribute names */
  /* references: Hashtbl.t(int, list(Location.t)), */
  exported: Hashtbl.t(string, int),
  mutable locations: list((Location.t, Types.type_expr, definition))
};

module Get = {
  /* TODO maybe return loc from this? or have a separate one that
   * finds a thing by name...
   */
  let rec stampNames = items =>
    Typedtree.(
      items
      |> List.map(item =>
           switch item.str_desc {
           | Tstr_value(_, bindings) =>
             bindings
             |> PrepareUtils.filterNil(binding =>
                  switch binding {
                  | {vb_pat: {pat_desc: Tpat_var({stamp, name}, _)}} =>
                    Some((name, stamp))
                  | _ => None
                  }
                )
           | Tstr_type(decls) =>
             decls |> List.map(({typ_id: {stamp, name}}) => (name, stamp))
           | Tstr_module({mb_id: {stamp, name}}) => [(name, stamp)]
           | Tstr_modtype({mtd_id: {stamp, name}}) => [(name, stamp)]
           /* | Tstr_include({incl_type}) */
           | _ => []
           }
         )
      |> List.concat
    );
  module F = (Collector: {let data: moduleData;}) => {
    open Typedtree;
    include TypedtreeIter.DefaultIteratorArgument;
    let addStamp = (stamp, name, loc, item, docs) =>
      if (! Hashtbl.mem(Collector.data.stamps, stamp)) {
        Hashtbl.replace(Collector.data.stamps, stamp, (name, loc, item, docs));
      };
    /* let addReference = (stamp, loc) => {
         let current = Hashtbl.mem(Collector.data.references, stamp) ?
         Hashtbl.find(Collector.data.references, stamp) : [];
         Hashtbl.replace(Collector.data.references, stamp, [loc, ...current])
       }; */
    let addLocation = (loc, typ, definition) =>
      Collector.data.locations = [
        (loc, typ, definition),
        ...Collector.data.locations
      ];
    /* let enter_signature_item = item => switch item.sig_desc {
         | Tsig_value({val_id: {stamp, name}, val_val: {val_type}, val_loc}) => addStamp(stamp, name, val_loc, Value(val_type), None)
         | Tsig_type(decls) => List.iter(({typ_id: {stamp, name}}) => (stamp, addToPath(currentPath, name) |> toFullPath(PType)), decls)
         | Tsig_include({incl_mod, incl_type}) => stampsFromTypesSignature(currentPath, incl_type)
         | Tsig_module({md_id: {stamp, name}, md_type: {mty_desc: Tmty_signature(signature)}}) => {
           let (stamps) = stampsFromTypedtreeInterface(addToPath(currentPath, name), signature.sig_items);
           [(stamp, addToPath(currentPath, name) |> toFullPath(PModule)), ...stamps]
         }
         | Tsig_module({md_id: {stamp, name}}) => [(stamp, addToPath(currentPath, name) |> toFullPath(PModule))]
         | _ => []
       } */
    let enter_structure_item = item =>
      Typedtree.(
        switch item.str_desc {
        | Tstr_value(_rec, bindings) =>
          bindings
          |> List.iter(binding =>
               switch binding {
               | {
                   vb_attributes,
                   vb_pat: {pat_type, pat_desc: Tpat_var({stamp, name}, {loc})}
                 } =>
                 let docs = PrepareUtils.findDocAttribute(vb_attributes);
                 addStamp(stamp, name, loc, Value(pat_type), docs);
               /* addLocation(loc, pat_type, None); */
               | _ => ()
               }
             )
        | Tstr_type(decls) =>
          decls
          |> List.iter(
               (
                 {
                   typ_attributes,
                   typ_id: {stamp, name},
                   typ_type,
                   typ_name: {loc}
                 }
               )
               => {
                 let docs = PrepareUtils.findDocAttribute(typ_attributes);
                 addStamp(stamp, name, loc, Type(typ_type), docs);
               })
               /* addLocation(loc, typ_type, None); */
        | Tstr_module({
            mb_id: {stamp, name},
            mb_name: {loc},
            mb_expr: {
              mod_type,
              mod_desc:
                Tmod_structure(structure) |
                Tmod_constraint({mod_desc: Tmod_structure(structure)}, _, _, _)
            },
            mb_attributes
          }) =>
          let docs = PrepareUtils.findDocAttribute(mb_attributes);
          addStamp(
            stamp,
            name,
            loc,
            Module(stampNames(structure.str_items)),
            docs
          );
        | Tstr_module({mb_attributes, mb_id: {stamp, name}, mb_name: {loc}}) =>
          let docs = PrepareUtils.findDocAttribute(mb_attributes);
          addStamp(stamp, name, loc, Module([]), docs);
        /* | Tstr_modtype */
        | _ => ()
        }
      );
    let enter_core_type = typ =>
      /* open Typedtree; */
      /* Collector.add(~depth=depth^, typ.ctyp_type, typ.ctyp_loc); */
      switch typ.ctyp_desc {
      | Ttyp_constr(path, {txt, loc}, args) =>
        addLocation(loc, typ.ctyp_type, Path(path))
      /* if (usesOpen(txt, path)) {
           add_use((path, Type), txt, loc);
         };
         Collector.ident((path, Type), loc) */
      | _ => ()
      };
    let rec dig = typ =>
      switch typ.Types.desc {
      | Types.Tlink(inner) => dig(inner)
      | Types.Tsubst(inner) => dig(inner)
      | _ => typ
      };
    let enter_pattern = pat =>
      switch pat.pat_desc {
      | Tpat_alias(_, {stamp, name}, {txt, loc})
      | Tpat_var({stamp, name}, {txt, loc}) =>
        addStamp(stamp, name, loc, Value(pat.pat_type), None);
        addLocation(loc, pat.pat_type, IsDefinition);
      | Tpat_construct({txt, loc}, {cstr_name, cstr_loc, cstr_res}, args) =>
        switch (dig(cstr_res).Types.desc) {
        | Tconstr(path, args, _) =>
          addLocation(
            loc,
            pat.pat_type,
            Constructor(path, cstr_name, cstr_loc)
          )
        | _ => ()
        }
      | Tpat_record(items, isClosed) =>
        items
        |> List.iter(
             (({Asttypes.txt, loc}, {Types.lbl_res, lbl_name, lbl_loc}, value)) =>
             switch (dig(lbl_res).Types.desc) {
             | Tconstr(path, args, _) =>
               addLocation(loc, lbl_res, Attribute(path, lbl_name, lbl_loc))
             | _ => ()
             }
           )
      | _ => ()
      };
    let enter_expression = expr =>
      switch expr.exp_desc {
      | Texp_for({stamp, name}, {ppat_loc}, {exp_type}, _, _, _) =>
        addLocation(ppat_loc, exp_type, IsDefinition);
        addStamp(stamp, name, ppat_loc, Value(exp_type), None);
      | Texp_ident(path, {txt, loc}, _) =>
        addLocation(loc, expr.exp_type, Path(path))
      | Texp_field(inner, {txt, loc}, {lbl_name, lbl_res, lbl_loc}) =>
        switch (dig(lbl_res).Types.desc) {
        | Tconstr(path, args, _) =>
          addLocation(loc, expr.exp_type, Attribute(path, lbl_name, lbl_loc))
        | _ => ()
        }
      | Texp_constant(_) =>
        addLocation(expr.exp_loc, expr.exp_type, IsDefinition)
      | Texp_record(items, ext) =>
        items
        |> List.iter(
             (({Asttypes.txt, loc}, {Types.lbl_loc, lbl_name, lbl_res}, ex))
             /* addLocation(loc, ex.exp_type, Location(lbl_loc)) */
             =>
               switch (dig(lbl_res).Types.desc) {
               | Tconstr(path, args, _) =>
                 addLocation(
                   loc,
                   ex.exp_type,
                   Attribute(path, lbl_name, lbl_loc)
                 )
               | _ => ()
               }
             )
      | Texp_construct({txt, loc}, {cstr_name, cstr_loc, cstr_res}, args) =>
        /* Huh, we can jump right to cstr_loc!! Wow */
        switch (dig(cstr_res).Types.desc) {
        | Tconstr(path, args, _) =>
          addLocation(
            loc,
            expr.exp_type,
            Constructor(path, cstr_name, cstr_loc)
          )
        | _ => ()
        }
      | _ => ()
      };
  };
  let process = cmt => {
    let data = {
      stamps: Hashtbl.create(100),
      /* references: Hashtbl.create(100), */
      exported: Hashtbl.create(10),
      locations: []
    };
    module IterIter =
      TypedtreeIter.MakeIterator(
        (
          F(
            {
              let data = data;
            }
          )
        )
      );
    let structure = items => {
      let names = stampNames(items);
      names
      |> List.iter(((name, stamp)) =>
           Hashtbl.replace(data.exported, name, stamp)
         );
      List.iter(IterIter.iter_structure_item, items);
    };
    let iter_part = part =>
      switch part {
      | Cmt_format.Partial_structure(str) => structure(str.str_items)
      | Partial_structure_item(str) => structure([str])
      | Partial_signature(str) => IterIter.iter_signature(str)
      | Partial_signature_item(str) => IterIter.iter_signature_item(str)
      | Partial_expression(expression) => IterIter.iter_expression(expression)
      | Partial_pattern(pattern) => IterIter.iter_pattern(pattern)
      | Partial_class_expr(class_expr) => IterIter.iter_class_expr(class_expr)
      | Partial_module_type(module_type) =>
        IterIter.iter_module_type(module_type)
      };
    switch cmt {
    | Cmt_format.Implementation(str) => structure(str.str_items)
    | Cmt_format.Interface(sign) => IterIter.iter_signature(sign)
    | Cmt_format.Partial_implementation(parts)
    | Cmt_format.Partial_interface(parts) => Array.iter(iter_part, parts)
    | _ => failwith("Not a valid cmt file")
    };
    data.locations = List.rev(data.locations);
    data;
  };
};

let process = Get.process;

open Infix;

let checkPos =
    (
      (line, char),
      {Location.loc_start: {pos_lnum, pos_bol, pos_cnum}, loc_end}
    ) =>
  Lexing.(
    if (line < pos_lnum || line == pos_lnum && char < pos_cnum - pos_bol) {
      false;
    } else if (line > loc_end.pos_lnum
               || line == loc_end.pos_lnum
               && char > loc_end.pos_cnum
               - loc_end.pos_bol) {
      false;
    } else {
      true;
    }
  );

let locationAtPos = ((line, char), data) => {
  let pos = (line + 1, char);
  let rec loop = locations =>
    switch locations {
    | [] => None
    | [(loc, expr, defn), ..._] when checkPos(pos, loc) =>
      Some((loc, expr, defn))
    | [_, ...rest] => loop(rest)
    };
  loop(data.locations);
};

let maybeFound = (fn, a) =>
  switch (fn(a)) {
  | exception Not_found => None
  | x => Some(x)
  };

let findDefinition = (defn, data) =>
  /* Log.log("😍 resolving a definition"); */
  switch defn {
  | IsDefinition =>
    Log.log("Is a definition");
    None;
  | Constructor(path, _, _)
  | Attribute(path, _, _)
  | Path(path) =>
    switch path {
    | Path.Pident({stamp: 0, name}) =>
      Log.log("Global stamp folks");
      None; /* Not handling global ones yett */
    | Path.Pident({stamp, name}) =>
      maybeFound(Hashtbl.find(data.stamps), stamp)
      |?>> x => `Local(x)
    | Path.Pdot(inner, name, _) =>
      let rec loop = p =>
        switch p {
        | Path.Pident({stamp: 0, name}) => {
          `Global(name, [])
        }
        | Path.Pident({stamp, name}) =>
          `Local(maybeFound(Hashtbl.find(data.stamps), stamp))
        | Path.Pdot(inner, name, _) =>
          switch (loop(inner)) {
          | `Global(top, subs) => `Global(top, subs @ [name])
          | `Local(Some((_, _, Module(contents), _))) =>
            `Local(maybeFound(List.assoc(name), contents)
            |?> maybeFound(Hashtbl.find(data.stamps)))
          | _ => `Local(None)
          }
        | _ => `Local(None)
        };
      switch (loop(inner)) {
      | `Global(top, children) => Some(`Global(top, children @ [name]))
      | `Local(Some((_, _, Module(contents), _))) =>
        maybeFound(List.assoc(name), contents)
        |?> maybeFound(Hashtbl.find(data.stamps))
        |?>> x => `Local(x)
      | _ =>  None
      };
    | _ =>
      None
    }
  };

let resolveDefinition = (defn, data) => switch (findDefinition(defn, data)) {
| None => None
| Some(`Global(top, children)) => None
| Some(`Local(defn)) => Some(defn)
};

let definitionForPos = (pos, data) =>
  locationAtPos(pos, data)
  |?> (((_, _, defn)) => resolveDefinition(defn, data));