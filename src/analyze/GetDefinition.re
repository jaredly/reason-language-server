open Definition;
open Docs.T;
open Infix;

module Get = {
  /* For a list of structure items, get the names and stamps of definted things in there.
   */
  let rec stampNames = (items) =>
    Typedtree.(
      items
      |> List.map(
           (item) =>
             switch item.str_desc {
             | Tstr_value(_, bindings) =>
               bindings
               |> PrepareUtils.filterNil(
                    (binding) =>
                      switch binding {
                      | {vb_pat: {pat_desc: Tpat_var({stamp, name}, _)}} => Some((name, stamp))
                      | _ => None
                      }
                  )
             | Tstr_type(decls) => decls |> List.map(({typ_id: {stamp, name}}) => (name, stamp))
             | Tstr_module({mb_id: {stamp, name}}) => [(name, stamp)]
             | Tstr_modtype({mtd_id: {stamp, name}}) => [(name, stamp)]
             /* | Tstr_include({incl_type}) */
             | _ => []
             }
         )
      |> List.concat
    );
  module F = (Collector: {let data: moduleData; let allOpens: ref(list(anOpen));}) => {
    open Typedtree;
    include TypedtreeIter.DefaultIteratorArgument;
    let posOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => (pos_lnum, pos_cnum - pos_bol);
    let rangeOfLoc = ({Location.loc_start, loc_end}) => (
      posOfLexing(loc_start),
      posOfLexing(loc_end)
    );
    let openScopes = ref([ref([])]);
    let addOpenScope = () => openScopes := [ref([]), ...openScopes^];
    let popOpenScope = () => openScopes := List.tl(openScopes^);
    let addOpen = (path, loc) => {
      let top = List.hd(openScopes^);
      let op = {path, loc, used: [], useCount: 0};
      top := [op, ...top^];
      Collector.allOpens := [op, ...Collector.allOpens^]
    };
    let rec usesOpen = (ident, path) =>
      switch (ident, path) {
      | (Longident.Lident(name), Path.Pdot(path, pname, _)) => true
      | (Longident.Lident(_), Path.Pident(_)) => false
      | (Longident.Ldot(ident, _), Path.Pdot(path, _, _)) => usesOpen(ident, path)
      | (Ldot(_), Pident({name: "*predef*" | "exn"})) => false
      | (Ldot(Lident("*predef*" | "exn"), _), Pident(_)) => false
      | _ =>
        failwith(
          "Cannot open " ++ Path.name(path) ++ " " ++ String.concat(".", Longident.flatten(ident))
        )
      };
    let rec relative = (ident, path) =>
      switch (ident, path) {
      | (Longident.Lident(name), Path.Pdot(path, pname, _)) when pname == name => path
      | (Longident.Ldot(ident, _), Path.Pdot(path, _, _)) => relative(ident, path)
      | (Ldot(Lident("*predef*" | "exn"), _), Pident(_)) => path
      | _ =>
        failwith(
          "Cannot relative "
          ++ Path.name(path)
          ++ " "
          ++ String.concat(".", Longident.flatten(ident))
        )
      };
    let addUse = ((path, tag), ident, loc) => {
      let openNeedle = relative(ident, path);
      let rec loop = (stacks) =>
        switch stacks {
        | [] => ()
        | [stack, ...rest] =>
          let rec inner = (opens) =>
            switch opens {
            | [] => loop(rest)
            | [{path} as one, ...rest] when Path.same(path, openNeedle) =>
              one.used = [(ident, tag, loc), ...one.used];
              one.useCount = one.useCount + 1
            | [{path}, ...rest] => inner(rest)
            };
          inner(stack^)
        };
      loop(openScopes^)
    };
    let scopes = ref([((0, 0), ((-1), (-1)))]);
    let addScope = (loc) => scopes := [loc, ...scopes^];
    let popScope = () =>
      scopes :=
        (
          switch scopes^ {
          | [] => []
          | [_, ...rest] => rest
          }
        );
    let currentScope = () => List.hd(scopes^);
    let addStamp = (stamp, name, loc, item, docs) =>
      if (! Hashtbl.mem(Collector.data.stamps, stamp)) {
        Hashtbl.replace(Collector.data.stamps, stamp, (name, loc, item, docs, currentScope()))
      };

    let addLocation = (loc, typ, definition) => {
      switch definition {
      | ConstructorDefn(path, name, _) => Some((path, Some(name)))
      | AttributeDefn(path, name, _) => Some((path, Some(name)))
      | Path(path) => Some((path, None))
      | _ => None
      } |?< ((path, suffix)) =>
        switch (stampAtPath(path, Collector.data, suffix)) {
        | None => ()
        | Some(`Global(modname, children, suffix)) =>
          let current = maybeFound(Hashtbl.find(Collector.data.externalReferences), modname) |? [];
          Hashtbl.replace(
            Collector.data.externalReferences,
            modname,
            [(children, loc, suffix), ...current]
          )
        | Some(`Local(stamp)) => {
          let current = maybeFound(Hashtbl.find(Collector.data.internalReferences), stamp) |? [];
          Hashtbl.replace(Collector.data.internalReferences, stamp, [loc, ...current])
        }
        };
      Collector.data.locations = [(loc, typ, definition), ...Collector.data.locations]
    };
    let addExplanation = (loc, text) => {
      Collector.data.explanations = [(loc, text), ...Collector.data.explanations]
    };

    let enter_signature_item = (item) =>
      switch item.sig_desc {
        | Tsig_attribute(({Asttypes.txt: "ocaml.explanation", loc}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}]))) => {
          addExplanation(loc, doc)
        }
        | Tsig_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}]))) => {
          if (Collector.data.toplevelDocs == None) {
            Collector.data.toplevelDocs = Some(doc)
          } else {
            ()
          }
        }
      /* | Tsig_value({val_id: {stamp, name}, val_val: {val_type}, val_loc}) =>
        addStamp(stamp, name, val_loc, Value(val_type), None) */
      /* | Tsig_type(decls) =>
        List.iter(
          ({typ_id: {stamp, name}, typ_loc, typ_type}) => {
            addStamp(stamp, name, typ_loc, Type(typ_type), None);
            switch (typ_type.type_kind) {
              | Types.Type_record(labels, _) => {
                labels |> List.iter(({Types.ld_id: {stamp, name: lname}, ld_type, ld_loc}) => {
                  addStamp(stamp, lname, ld_loc, Attribute(ld_type, name, typ_type), None)
                })
              }
              | Types.Type_variant(constructors) => {
                constructors |> List.iter(({Types.cd_id: {stamp, name: cname}, cd_loc} as cd) => {
                  addStamp(stamp, cname, cd_loc, Constructor(cd, name, typ_type), None)
                })

              }
              | _ => ()
            }
          },
          decls
        ) */
      /* TODO add support for these */
      /* | Tsig_include({incl_mod, incl_type}) => stampsFromTypesSignature(currentPath, incl_type) */
      /* | Tsig_module({md_id: {stamp, name}, md_type: {mty_desc: Tmty_signature(signature)}}) => {
           addStamp
           let (stamps) = stampsFromTypedtreeInterface(addToPath(currentPath, name), signature.sig_items);
           [(stamp, addToPath(currentPath, name) |> toFullPath(PModule)), ...stamps]
         } */
      /* | Tsig_module({md_id: {stamp, name}, md_loc, md_type}) =>
        addStamp(stamp, name, md_loc, Module([]), None) */
      | _ => ()
      };
    let enter_structure_item = (item) =>
      Typedtree.(
        switch item.str_desc {
        | Tstr_attribute(({Asttypes.txt: "ocaml.explanation", loc}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}]))) => {
          addExplanation(loc, doc)
        }
        | Tstr_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}]))) => {
          if (Collector.data.toplevelDocs == None) {
            Collector.data.toplevelDocs = Some(doc)
          } else {
            ()
          }
        }
        | Tstr_value(_rec, bindings) =>
          /* TODO limit toplevel value completions */
          bindings
          |> List.iter(
               (binding) =>
                 switch binding {
                 | {vb_attributes, vb_pat: {pat_type, pat_desc: Tpat_var({stamp, name}, {loc})}} =>
                   let docs = PrepareUtils.findDocAttribute(vb_attributes);
                   addStamp(stamp, name, loc, Value(pat_type), docs)
                 /* addLocation(loc, pat_type, None); */
                 | _ => ()
                 }
             )
        | Tstr_type(decls) =>
          decls
          |> List.iter(
               ({typ_attributes, typ_id: {stamp, name}, typ_type, typ_name: {loc}}) => {
                 let docs = PrepareUtils.findDocAttribute(typ_attributes);
                 addStamp(stamp, name, loc, Type(typ_type), docs);

                  switch (typ_type.type_kind) {
                    | Types.Type_record(labels, _) => {
                      labels |> List.iter(({Types.ld_id: {stamp: lstamp, name: lname}, ld_type, ld_loc}) => {
                        let shortLoc = Utils.clampLocation(ld_loc, String.length(lname));
                        addStamp(lstamp, lname, shortLoc, Attribute(ld_type, name, typ_type), docs);
                        if (maybeFound(Hashtbl.find(Collector.data.exported), name) == Some(stamp)) {
                          Collector.data.exportedSuffixes = [(lstamp, name, lname), ...Collector.data.exportedSuffixes];
                        };
                        addLocation(shortLoc, {Types.desc: Types.Tnil, level: 0, id: 0}, IsDefinition(lstamp));
                      })
                    }
                    | Types.Type_variant(constructors) => {
                      constructors |> List.iter(({Types.cd_id: {stamp: cstamp, name: cname}, cd_loc} as cd) => {
                        let shortLoc = Utils.clampLocation(cd_loc, String.length(cname));
                        addStamp(cstamp, cname, shortLoc, Constructor(cd, name, typ_type), docs);
                        addLocation(shortLoc, {Types.desc: Types.Tnil, level: 0, id: 0}, IsDefinition(cstamp));
                        if (maybeFound(Hashtbl.find(Collector.data.exported), name) == Some(stamp)) {
                          Collector.data.exportedSuffixes = [(cstamp, name, cname), ...Collector.data.exportedSuffixes];
                        };
                      })
                    }
                    | _ => ()
                  }
               }
             )
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
          addOpenScope();
          /* addStamp(stamp, name, loc, Module(stampNames(structure.str_items)), docs) */
        | Tstr_module({mb_attributes, mb_id: {stamp, name}, mb_name: {loc}, mb_expr: {mod_type}}) =>
          let docs = PrepareUtils.findDocAttribute(mb_attributes);
          /* addStamp(stamp, name, loc, ModuleWithDocs(Docs.forModuleType(x => x, mod_type)), docs) */
        | Tstr_open({open_path, open_txt: {txt, loc}}) =>
          if (usesOpen(txt, open_path)) {
            addUse((open_path, TagModule), txt, loc)
          };
          addLocation(loc, {Types.desc: Types.Tnil, level: 0, id: 0}, Open(open_path));
          addOpen(open_path, loc)
        /* | Tstr_modtype */
        | _ => ()
        }
      );
    let leave_structure_item = (item) =>
      switch item.str_desc {
      | Tstr_module({
          mb_expr: {
            mod_desc: Tmod_structure(_) | Tmod_constraint({mod_desc: Tmod_structure(_)}, _, _, _)
          }
        }) =>
        popOpenScope()
      | _ => ()
      };
    let enter_core_type = (typ) =>
      /* open Typedtree; */
      /* Collector.add(~depth=depth^, typ.ctyp_type, typ.ctyp_loc); */
      switch typ.ctyp_desc {
      | Ttyp_constr(path, {txt, loc}, args) =>
        addLocation(loc, typ.ctyp_type, Path(path));
        if (usesOpen(txt, path)) {
          addUse((path, TagType), txt, loc)
        }
      /* Collector.ident((path, Type), loc) */
      | _ => ()
      };
    let enter_pattern = (pat) =>
      switch pat.pat_desc {
      | Tpat_alias(_, {stamp, name}, {txt, loc})
      | Tpat_var({stamp, name}, {txt, loc}) =>
        addStamp(stamp, name, loc, Value(pat.pat_type), None);
        addLocation(loc, pat.pat_type, IsDefinition(stamp))
      | Tpat_construct({txt, loc}, {cstr_name, cstr_loc, cstr_res}, args) =>
        switch (dig(cstr_res).Types.desc) {
        | Tconstr(path, args, _) =>
          let (constructorName, typeTxt) = handleConstructor(path, txt);
          if (usesOpen(typeTxt, path)) {
            addUse((path, TagConstructor(constructorName)), typeTxt, loc)
          };
          addLocation(loc, pat.pat_type, ConstructorDefn(path, cstr_name, cstr_loc))
        | _ => ()
        }
      | Tpat_record(items, isClosed) =>
        items
        |> List.iter(
             (({Asttypes.txt, loc}, {Types.lbl_res, lbl_name, lbl_loc}, value)) =>
               switch (dig(lbl_res).Types.desc) {
               | Tconstr(path, args, _) =>
                 addLocation(loc, lbl_res, AttributeDefn(path, lbl_name, lbl_loc));
                 let typeTxt = handleRecord(path, txt);
                 if (usesOpen(typeTxt, path)) {
                   addUse((path, TagAttribute(lbl_name)), typeTxt, loc)
                 }
               | _ => ()
               }
           )
      | _ => ()
      };
    let enter_expression = (expr) => {
      expr.exp_attributes |> List.iter(attr => switch attr {
        | ({Asttypes.txt: "ocaml.explanation", loc}, Parsetree.PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])) => {
          addExplanation(loc, doc)
        }
        | _ => ()
      });

      switch expr.exp_desc {
      | Texp_for({stamp, name}, {ppat_loc}, {exp_type}, _, _, contents) =>
        addLocation(ppat_loc, exp_type, IsDefinition(stamp));
        addScope(rangeOfLoc(contents.exp_loc));
        addStamp(stamp, name, ppat_loc, Value(exp_type), None);
        popScope()
      /* JSX fix */
      | Texp_ident(
          path,
          {txt, loc},
          _
        )
          when locationSize(loc) != String.length(Longident.flatten(txt) |> String.concat(".")) =>
        ()
      | Texp_ident(path, {txt, loc}, _) =>
        addLocation(loc, expr.exp_type, Path(path));
        if (usesOpen(txt, path)) {
          addUse((path, TagValue), txt, loc)
        }
      | Texp_field(inner, {txt, loc}, {lbl_name, lbl_res, lbl_loc}) =>
        switch (dig(lbl_res).Types.desc) {
        | Tconstr(path, args, _) =>
          addLocation(loc, expr.exp_type, AttributeDefn(path, lbl_name, lbl_loc));
          let typeTxt = handleRecord(path, txt);
          if (usesOpen(typeTxt, path)) {
            addUse((path, TagAttribute(lbl_name)), typeTxt, loc)
          }
        | _ => ()
        }
      /* JSX string */
      | Texp_constant(Const_string(text, None)) when locationSize(expr.exp_loc) != String.length(text) => ()
      | Texp_constant(_) => addLocation(expr.exp_loc, expr.exp_type, IsConstant)
      | Texp_record(items, ext) =>
        items
        |> List.iter(
             (({Asttypes.txt, loc}, {Types.lbl_loc, lbl_name, lbl_res}, ex)) =>
               switch (dig(lbl_res).Types.desc) {
               | Tconstr(path, args, _) =>
                 addLocation(loc, ex.exp_type, AttributeDefn(path, lbl_name, lbl_loc));
                 let typeTxt = handleRecord(path, txt);
                 if (usesOpen(typeTxt, path)) {
                   addUse((path, TagAttribute(lbl_name)), typeTxt, loc)
                 }
               | _ => ()
               }
           )
      /* Skip list literals */
      | Texp_construct(
          {txt: Lident("()"), loc: {loc_start: {pos_cnum: cstart}, loc_end: {pos_cnum: cend}}},
          {cstr_name, cstr_loc, cstr_res},
          args
        )
          when cend - cstart != 2 =>
        ()
      | Texp_construct(
          {txt: Lident("::"), loc: {loc_start: {pos_cnum: cstart}, loc_end: {pos_cnum: cend}}},
          {cstr_name, cstr_loc, cstr_res},
          args
        )
          when cend - cstart != 2 =>
        ()
      | Texp_construct({txt, loc}, {cstr_name, cstr_loc, cstr_res}, args) =>
        switch (dig(cstr_res).Types.desc) {
        | Tconstr(path, args, _) =>
          addLocation(loc, expr.exp_type, ConstructorDefn(path, cstr_name, cstr_loc));
          let (constructorName, typeTxt) = handleConstructor(path, txt);
          if (usesOpen(typeTxt, path)) {
            addUse((path, TagConstructor(constructorName)), typeTxt, loc)
          }
        | _ => ()
        }
      | Texp_let(recFlag, bindings, expr) =>
        let start =
          Asttypes.Recursive == recFlag ?
            List.hd(bindings).vb_loc.loc_start : expr.exp_loc.loc_start;
        addScope((posOfLexing(start), posOfLexing(expr.exp_loc.loc_end)))
      | Texp_function(label, cases, _) => addScope(rangeOfLoc(expr.exp_loc))
      | _ => ()
      };
    };
    let leave_expression = (expr) =>
      switch expr.exp_desc {
      | Texp_let(recFlag, bindings, expr) => popScope()
      | Texp_function(_) => popScope()
      | _ => ()
      };
  };

  let process = (cmt) => {
    let data = {
      toplevelDocs: None,
      stamps: Hashtbl.create(100),
      internalReferences: Hashtbl.create(100),
      externalReferences: Hashtbl.create(100),
      exportedSuffixes: [],
      exported: Hashtbl.create(10),
      allOpens: [],
      topLevel: [],
      locations: [],
      explanations: [],
    };
    let allOpens = ref([]);
    module IterIter =
      TypedtreeIter.MakeIterator(
        (
          F(
            {
              let data = data;
              let allOpens = allOpens;
            }
          )
        )
      );
    let rec stampContents = item => {
      Hashtbl.replace(data.stamps, item.stamp, (item.name, item.loc, item.kind, item.docstring, (
        (-1, -1), (-1, -1)
      )));
      switch (item.kind) {
      | Module(contents) => List.iter(stampContents, contents)
      | _ => ()
      };
    };
    let structure = (items) => {
      let (docs, contents) = Docs.forStructure(x => x, items);
      data.topLevel = contents;
      contents |> List.iter(({Docs.T.name, stamp}) => Hashtbl.replace(data.exported, name, stamp));
      List.iter(IterIter.iter_structure_item, items)
    };
    let iter_part = (part) =>
      switch part {
      | Cmt_format.Partial_structure(str) =>
        IterIter.iter_structure(str);
        Docs.forStructure(x => x, str.str_items) |> snd
        /* stampNames(str.str_items) */
      | Partial_structure_item(str) =>
        IterIter.iter_structure_item(str);
        Docs.forStructure(x => x, [str]) |> snd
        /* stampNames([str]) */
      | Partial_signature(str) =>
        IterIter.iter_signature(str);
        []
      | Partial_signature_item(str) =>
        IterIter.iter_signature_item(str);
        []
      | Partial_expression(expression) =>
        IterIter.iter_expression(expression);
        []
      | Partial_pattern(pattern) =>
        IterIter.iter_pattern(pattern);
        []
      | Partial_class_expr(class_expr) =>
        IterIter.iter_class_expr(class_expr);
        []
      | Partial_module_type(module_type) =>
        IterIter.iter_module_type(module_type);
        []
      };
    switch cmt {
    | Cmt_format.Implementation(str) => structure(str.str_items)
    | Cmt_format.Interface(sign) => IterIter.iter_signature(sign)
    | Cmt_format.Partial_implementation(parts)
    | Cmt_format.Partial_interface(parts) =>
      /* TODO TODO */
      let contents = Array.map(iter_part, parts) |> Array.to_list |> List.concat;
      contents |> List.iter(({name, stamp}) => Hashtbl.replace(data.exported, name, stamp));
      data.topLevel = contents;
      ()
    | _ => failwith("Not a valid cmt file")
    };
    data.locations = List.rev(data.locations);
    data.explanations = List.rev(data.explanations);
    /* allOpens^ |> List.iter(({used, path, loc}) => {
         Log.log("An Open! " ++ string_of_int(List.length(used)));
       }); */
    data.allOpens = allOpens^;
    data
  };
};

let process = Get.process;