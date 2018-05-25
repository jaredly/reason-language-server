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
  | ModuleAlias(Path.t)
  | Type(Types.type_declaration)
  | Value(Types.type_expr);

type moduleData = {
  stamps: Hashtbl.t(int, (string, Location.t, item, option(string))),
  /* TODO track constructor names, and record attribute names */
  /* references: Hashtbl.t(int, list(Location.t)), */
  exported: Hashtbl.t(string, int),
  mutable locations: list((Location.t, Types.type_expr, option(Path.t)))
};

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
    Hashtbl.replace(Collector.data.stamps, stamp, (name, loc, item, docs));
  /* let addReference = (stamp, loc) => {
       let current = Hashtbl.mem(Collector.data.references, stamp) ?
       Hashtbl.find(Collector.data.references, stamp) : [];
       Hashtbl.replace(Collector.data.references, stamp, [loc, ...current])
     }; */
  let addLocation = (loc, typ, path) =>
    Collector.data.locations = [(loc, typ, path), ...Collector.data.locations];
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
             | {vb_pat: {pat_type, pat_desc: Tpat_var({stamp, name}, {loc})}} =>
               addStamp
                 (stamp, name, loc, Value(pat_type), None)
                 /* addLocation(loc, pat_type, None); */
             | _ => ()
             }
           )
      | Tstr_type(decls) =>
        decls
        |> List.iter(({typ_id: {stamp, name}, typ_type, typ_name: {loc}}) =>
             addStamp
               (stamp, name, loc, Type(typ_type), None)
               /* addLocation(loc, typ_type, None); */
           )
      | Tstr_module({
          mb_id: {stamp, name},
          mb_name: {loc},
          mb_expr: {
            mod_type,
            mod_desc:
              Tmod_structure(structure) |
              Tmod_constraint({mod_desc: Tmod_structure(structure)}, _, _, _)
          }
        }) =>
        addStamp(
          stamp,
          name,
          loc,
          Module(stampNames(structure.str_items)),
          None
        )
      | Tstr_module({mb_id: {stamp, name}, mb_name: {loc}}) =>
        addStamp(stamp, name, loc, Module([]), None)
      /* | Tstr_modtype */
      | _ => ()
      }
    );
  let enter_core_type = typ =>
    /* open Typedtree; */
    /* Collector.add(~depth=depth^, typ.ctyp_type, typ.ctyp_loc); */
    switch typ.ctyp_desc {
    | Ttyp_constr(path, {txt, loc}, args) =>
      addLocation
        (loc, typ.ctyp_type, Some(path))
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
      addStamp(stamp, name, loc, Value(pat.pat_type), None)
    /*
     TODO allow renaming of constructor items
     | Tpat_construct({txt, loc}, desc, args) => {
     }
     } */
    /* | Tpat_record(items, isClosed) => items |> List.iter((({Asttypes.txt, loc}, {lbl_res, lbl_name}, value)) => {
           switch (dig(lbl_res).Types.desc) {
           | Tconstr(path, args, _) => {
             Collector.ident((path, Attribute(label.Types.lbl_name)), loc);
             let typeTxt = handleRecord(path, txt);
             if (usesOpen(typeTxt, path)) {
               add_use(~inferable=true, (path, Attribute(label.Types.lbl_name)), typeTxt, loc)
             };
           }
           | _ => print_endline("Record not a constr " ++ {
             Printtyp.type_expr(Format.str_formatter, pat.pat_type);
             Format.flush_str_formatter()
           })
           };

       }) */
    | _ => ()
    };
  let enter_expression = expr =>
    switch expr.exp_desc {
    | Texp_for({stamp, name}, {ppat_loc}, {exp_type}, _, _, _) =>
      addLocation(ppat_loc, exp_type, None);
      addStamp(stamp, name, ppat_loc, Value(exp_type), None);
    /* | Texp_let(isrec, bindings, _) => bindings |> List.iter(({vb_expr: {exp_type}, vb_loc})) */
    | Texp_ident(path, {txt, loc}, _) =>
      addLocation(loc, expr.exp_type, Some(path))
    /* TODO record */
    | Texp_field(inner, {txt, loc}, label) =>
      addLocation(loc, expr.exp_type, None)
    | Texp_record(items, ext) =>
      items
      |> List.iter((({Asttypes.txt, loc}, label, ex)) =>
           addLocation(loc, ex.exp_type, None)
         )
    | Texp_construct({txt, loc}, {cstr_name, cstr_loc, cstr_res}, args) =>
      /* Huh, we can jump right to cstr_loc!! Wow */
      addLocation(
        loc,
        cstr_res,
        None
      )
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
  data;
};