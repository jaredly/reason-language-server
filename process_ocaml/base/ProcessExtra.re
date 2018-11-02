
#if 407
open Compiler_libs_407;
#elif 406
open Compiler_libs_406;
#elif 402
open Compiler_libs_402;
#endif
open Typedtree;
open SharedTypes;
open Infix;

let handleConstructor = (path, txt) => {
  let typeName =
    switch path {
    | Path.Pdot(path, typename, _) => typename
    | Pident(ident) => Ident.name(ident)
    | _ => assert false
    };
  Longident.(
    switch txt {
    | Longident.Lident(name) => (name, Lident(typeName))
    | Ldot(left, name) => (name, Ldot(left, typeName))
    | Lapply(left, _) => assert false
    }
  )
};

let rec relative = (ident, path) =>
  switch (ident, path) {
  | (Longident.Lident(name), Path.Pdot(path, pname, _)) when pname == name => Some(path)
  | (Longident.Ldot(ident, name), Path.Pdot(path, pname, _)) when pname == name => relative(ident, path)
  /* | (Ldot(Lident("*predef*" | "exn"), _), Pident(_)) => None */
  | _ => None
  };

let addOpen = (extra, path, loc, extent, ident) => {
  let op = {path, loc, used: [], extent, ident};
  Hashtbl.add(extra.opens, loc, op);
};

let findClosestMatchingOpen = (opens, path, ident, loc) => {
  let%opt openNeedle = relative(ident, path);

  let matching = Hashtbl.fold((l, op, res) => {
    if (Utils.locWithinLoc(loc, op.extent) && 
#if 407
        Path.same(op.path, openNeedle)) {
#else 
        Current.samePath(op.path, Shared.mapOldPath(openNeedle))) {
#endif
      [op, ...res]
    } else {
      res
    }
  }, opens, []) |. Belt.List.sort((a, b) => {
    open Location;
    b.loc.loc_start.pos_cnum - a.loc.loc_start.pos_cnum
  });

  switch matching {
    | [] => None
    | [first, ..._] => Some(first)
  }
};

let getTypeAtPath = (~env, path) => {
  switch (Query.fromCompilerPath(~env, path)) {
  | `GlobalMod(_) => `Not_found
  | `Global(moduleName, path) => `Global(moduleName, path)
  | `Not_found => `Not_found
  | `Exported(env, name) => {
      let res = {
        let%opt stamp = Query.hashFind(env.exported.types, name);
        let%opt_wrap declaredType = Query.hashFind(env.file.stamps.types, stamp);
        `Local(declaredType)
      };
      res |? `Not_found
  }
  | `Stamp(stamp) => {
    let res = {
      let%opt_wrap declaredType = Query.hashFind(env.file.stamps.types, stamp);
      `Local(declaredType)
    };
    res |? `Not_found
  }
  }
};

module F = (Collector: {
  let extra: extra;
  let file: file;
  let scopeExtent: ref(list(Location.t));
  let allLocations: bool;
}) => {
  let extra = Collector.extra;

  let maybeAddUse = (path, ident, loc, tip) => {
    let%opt_consume tracker = findClosestMatchingOpen(extra.opens, path, ident, loc);
#if 407
    let%opt_consume relpath = Query.makeRelativePath(tracker.path, path);
#else
    let%opt_consume relpath = Query.makeRelativePath(tracker.path, Shared.mapOldPath(path));
#endif

    tracker.used = [(relpath, tip, loc), ...tracker.used];
  };

  let addLocation = (loc, ident) => extra.locations = [(loc, ident), ...extra.locations];
  let addReference = (stamp, loc) => Hashtbl.replace(extra.internalReferences, stamp, [loc, ...Hashtbl.mem(extra.internalReferences, stamp) ? Hashtbl.find(extra.internalReferences, stamp) : []]);
  let addExternalReference = (moduleName, path, tip, loc) => {
    /* TODO need to follow the path, and be able to load the files to follow module references... */
    Hashtbl.replace(extra.externalReferences, moduleName, [(path, tip, loc), ...Hashtbl.mem(extra.externalReferences, moduleName) ? Hashtbl.find(extra.externalReferences, moduleName) : []]);
  };
  let env = {Query.file: Collector.file, exported: Collector.file.contents.exported};

  let getTypeAtPath = getTypeAtPath(~env);

  let addForPath = (path, lident, loc, typ, tip) => {
    maybeAddUse(path, lident, loc, tip);
    let identName = Longident.last(lident);
    let identLoc = Utils.endOfLocation(loc, String.length(identName));
#if 407
#else
    let path = Shared.mapOldPath(path);
#endif
    let locType = switch (Query.fromCompilerPath(~env, path)) {
      | `Stamp(stamp) => {
        addReference(stamp, identLoc);
        Loc.LocalReference(stamp, tip);
      }
      | `Not_found => Loc.NotFound
      | `Global(moduleName, path) => {
        addExternalReference(moduleName, path, tip, identLoc);
        Loc.GlobalReference(moduleName, path, tip)
      }
      | `Exported(env, name) => {
        let res = {
          let%opt_wrap stamp = Query.hashFind(env.exported.values, name);
          addReference(stamp, identLoc);
          Loc.LocalReference(stamp, tip)
        };
        res |? Loc.NotFound
      }
      | `GlobalMod(_) => Loc.NotFound
    };
    addLocation(loc, Loc.Typed(typ, locType));
  };

  let addForPathParent = (path, lident, loc) => {
    let locType = switch (Query.fromCompilerPath(~env, path)) {
      | `GlobalMod(name) =>
        /* TODO track external references to filenames to handle renames well */
        Loc.TopLevelModule(name)
      | `Stamp(stamp) => {
        addReference(stamp, loc);
        Module(LocalReference(stamp, Module))
      }
      | `Not_found => Module(NotFound)
      | `Global(moduleName, path) => {
        addExternalReference(moduleName, path, Module, loc);
        Module(GlobalReference(moduleName, path, Module))
      }
      | `Exported(env, name) => {
        let res = {
          let%opt_wrap stamp = Query.hashFind(env.exported.modules, name);
          addReference(stamp, loc);
          Loc.Module(LocalReference(stamp, Module))
        };
        res |? Module(NotFound)
      }
    };
    addLocation(loc, locType);
  };

  let addForField = (recordType, item, {Asttypes.txt, loc}) => {
    switch (Shared.dig(recordType).desc) {
      | Tconstr(path, _args, _memo) => {
#if 407
        let t = getTypeAtPath(path);
#else
        let t = getTypeAtPath(Shared.mapOldPath(path));
#endif
        let {Types.lbl_loc, lbl_res} = item;
        let name = Longident.last(txt);

        let (name, typeLident) = handleConstructor(path, txt);
        maybeAddUse(path, typeLident, loc, Attribute(name));

        let nameLoc = Utils.endOfLocation(loc, String.length(name));
        let locType = switch (t) {
          | `Local({stamp, contents: {kind: Record(attributes)}}) => {
            {
              let%opt_wrap {stamp: astamp} = Belt.List.getBy(attributes, a => a.name.txt == name);
              addReference(astamp, nameLoc);
              Loc.LocalReference(stamp, Attribute(name));
            } |? Loc.NotFound
          }
          | `Global(moduleName, path) =>
            addExternalReference(moduleName, path, Attribute(name), nameLoc);
            Loc.GlobalReference(moduleName, path, Attribute(name))
          | _ => Loc.NotFound
        };
        addLocation(nameLoc, Loc.Typed(Shared.makeFlexible(lbl_res), locType))
      }
      | _ => ()
    }
  };

  let addForRecord = (recordType, items) => {
    switch (Shared.dig(recordType).desc) {
      | Tconstr(path, _args, _memo) => {
#if 407
        let t = getTypeAtPath(path);
#else
        let t = getTypeAtPath(Shared.mapOldPath(path));
#endif
        items |> List.iter((({Asttypes.txt, loc}, {Types.lbl_loc, lbl_res}, _)) => {
          /* let name = Longident.last(txt); */

          let (name, typeLident) = handleConstructor(path, txt);
          maybeAddUse(path, typeLident, loc, Attribute(name));

          let nameLoc = Utils.endOfLocation(loc, String.length(name));
          let locType = switch (t) {
            | `Local({stamp, contents: {kind: Record(attributes)}}) => {
              {
                let%opt_wrap {stamp: astamp} = Belt.List.getBy(attributes, a => a.name.txt == name);
                addReference(astamp, nameLoc);
                Loc.LocalReference(stamp, Attribute(name));
              } |? Loc.NotFound
            }
            | `Global(moduleName, path) =>
              addExternalReference(moduleName, path, Attribute(name), nameLoc);
              Loc.GlobalReference(moduleName, path, Attribute(name))
            | _ => Loc.NotFound
          };
          addLocation(nameLoc, Loc.Typed(Shared.makeFlexible(lbl_res), locType))
        })
      }
      | _ => ()
    }
  };

  let addForConstructor = (constructorType, {Asttypes.txt, loc}, {Types.cstr_name, cstr_loc}) => {
    switch (Shared.dig(constructorType).desc) {
      | Tconstr(path, _args, _memo) => {
        /* let name = Longident.last(txt); */

        let (name, typeLident) = handleConstructor(path, txt);
        maybeAddUse(path, typeLident, loc, Constructor(name));

        let nameLoc = Utils.endOfLocation(loc, String.length(name));
#if 407
        let t = getTypeAtPath(path);
#else
        let t = getTypeAtPath(Shared.mapOldPath(path));
#endif
        let locType = switch (t) {
          | `Local({stamp, contents: {kind: Variant(constructos)}}) => {
            {
              let%opt_wrap {stamp: cstamp} = Belt.List.getBy(constructos, a => a.name.txt == cstr_name);
              addReference(cstamp, nameLoc);
              Loc.LocalReference(stamp, Constructor(name))
            } |? Loc.NotFound
          }
          | `Global(moduleName, path) =>
            addExternalReference(moduleName, path, Constructor(name), nameLoc);
            Loc.GlobalReference(moduleName, path, Constructor(name))
          | _ => Loc.NotFound
        };
        addLocation(nameLoc, Loc.Typed(Shared.makeFlexible(constructorType), locType));
      }
      | _ => ()
    }
  };

  let currentScopeExtent = () => {
    if (Collector.scopeExtent^ == []) {
      Location.none
    } else {
      List.hd(Collector.scopeExtent^);
    }
  };
  let addScopeExtent = loc => Collector.scopeExtent := [loc, ...Collector.scopeExtent^];
  let popScopeExtent = () => if (List.length(Collector.scopeExtent^) > 1) {
    Collector.scopeExtent := List.tl(Collector.scopeExtent^);
  };

  let rec addForLongident = (top, path: Path.t, txt: Longident.t, loc) => {
    if (!loc.Location.loc_ghost) {
      let l = Utils.endOfLocation(loc, String.length(Longident.last(txt)));
      switch (top) {
        | Some((t, tip)) => addForPath(path, txt, l, t, tip)
#if 407
        | None => addForPathParent(path, txt, l)
#else
        | None => addForPathParent(Shared.mapOldPath(path), txt, l)
#endif
      };
      switch (path, txt) {
        | (Pdot(pinner, pname, _), Ldot(inner, name)) => {
          addForLongident(None, pinner, inner, Utils.chopLocationEnd(loc, String.length(name) + 1));
        }
        | (Pident(_), Lident(name)) => ()
        | _ => ()
      };
    }
  };

  let rec handle_module_expr = expr => switch expr {
    | Tmod_constraint(expr, _, _, _) => handle_module_expr(expr.mod_desc)
    | Tmod_ident(path, {txt, loc}) =>
      Log.log("Ident!! " ++ String.concat(".", Longident.flatten(txt)));
      maybeAddUse(path, txt, loc, Module);
      addForLongident(None, path, txt, loc);
    | Tmod_functor(ident, argName, maybeType, resultExpr) =>
      handle_module_expr(resultExpr.mod_desc)
    | Tmod_apply(obj, arg, _) =>
      handle_module_expr(obj.mod_desc);
      handle_module_expr(arg.mod_desc);
    | _ => ()
  };

  open Typedtree;
  include TypedtreeIter.DefaultIteratorArgument;
  let enter_structure_item = item => switch (item.str_desc) {
  | Tstr_attribute(({Asttypes.txt: "ocaml.explanation", loc}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
#if 402
      Const_string
#else
      Pconst_string
#endif
      (doc, _))}, _)}]))) => {
    addLocation(loc, Loc.Explanation(doc))
  }
  | Tstr_include({incl_mod: expr, incl_type, incl_loc}) => {
    handle_module_expr(expr.mod_desc)
  }
  | Tstr_module({mb_name, mb_expr}) =>
    handle_module_expr(mb_expr.mod_desc)
  | Tstr_open({open_path, open_txt: {txt, loc} as l}) => {
    /* Log.log("Have an open here"); */
    maybeAddUse(open_path, txt, loc, Module);
    let tracker = {
#if 407
      path: open_path,
#else
      path: Shared.mapOldPath(open_path),
#endif
      loc,
      ident: l,
      used: [],
      extent: {
        loc_ghost: true,
        loc_start: loc.loc_end,
        loc_end: currentScopeExtent().loc_end,
      }
    };
    addForLongident(None, open_path, txt, loc);
    Hashtbl.replace(Collector.extra.opens, loc, tracker);
  }
  | _ => ()
  };

  let enter_structure = ({str_items}) =>
    if (str_items != []) {
      let first = List.hd(str_items);
      let last = List.nth(str_items, List.length(str_items) - 1);

      let extent = {
        Location.loc_ghost: true,
        loc_start: first.str_loc.loc_start,
        loc_end: last.str_loc.loc_end,
      };

      addScopeExtent(extent);
    };

  let leave_structure = str => {
    if (str.str_items != []) {
      popScopeExtent();
    }
  };

  let enter_signature_item = item => switch (item.sig_desc) {
  | Tsig_value({val_id, val_loc, val_name: name, val_desc, val_attributes}) => {
    let stamp = Ident.binding_time(val_id);
    if (!Hashtbl.mem(Collector.file.stamps.values, stamp)) {
      let declared = ProcessAttributes.newDeclared(
        ~name,
        ~stamp,
        ~extent=val_loc,
        ~scope={
          loc_ghost: true,
          loc_start: val_loc.loc_end,
          loc_end: currentScopeExtent().loc_end,
        },
        ~modulePath=NotVisible,
        ~processDoc=x => x,
        ~contents={Value.typ: Shared.makeFlexible(val_desc.ctyp_type), recursive: false},
        false,
        val_attributes
      );
      Hashtbl.add(Collector.file.stamps.values, stamp, declared);
      addReference(stamp, name.loc);
      addLocation(name.loc, Loc.Typed(Shared.makeFlexible(val_desc.ctyp_type), Loc.Definition(stamp, Value)));
    }
  }
  | _ => ()
  };

  let enter_core_type = ({ctyp_loc, ctyp_type, ctyp_desc}) => {
    switch (ctyp_desc) {
      | Ttyp_constr(path, {txt, loc}, args) => {
        /* addForPath(path, txt, loc, Shared.makeFlexible(ctyp_type), Type) */
        addForLongident(
          Some((Shared.makeFlexible(ctyp_type), Type)),
          path,
          txt, loc);
      }
      | _ => ()
    }
  };


  let rec enter_pattern = ({pat_desc, pat_loc, pat_type, pat_attributes}) => {
    let addForPattern = (stamp, name) => {
        if (!Hashtbl.mem(Collector.file.stamps.values, stamp)) {
          let declared = ProcessAttributes.newDeclared(
            ~name,
            ~stamp,
            ~scope={
              loc_ghost: true,
              loc_start: pat_loc.loc_end,
              loc_end: currentScopeExtent().loc_end,
            },
            ~modulePath=NotVisible,
            ~extent=pat_loc,
            ~processDoc=x => x,
            ~contents={Value.typ: Shared.makeFlexible(pat_type), recursive: false},
            false,
            pat_attributes
          );
          Hashtbl.add(Collector.file.stamps.values, stamp, declared);
          addReference(stamp, name.loc);
          addLocation(name.loc, Loc.Typed(Shared.makeFlexible(pat_type), Loc.Definition(stamp, Value)));
        }
    };
    /* Log.log("Entering pattern " ++ Utils.showLocation(pat_loc)); */
    switch (pat_desc) {
      | Tpat_record(items, _) => {
        addForRecord(pat_type, items);
      }
      | Tpat_construct(lident, constructor, _) => {
        addForConstructor(pat_type, lident, constructor)
      }
      | Tpat_alias(inner, ident, name) => {
        let stamp = Ident.binding_time(ident);
        addForPattern(stamp, name);
      }
      | Tpat_var(ident, name) => {
        /* Log.log("Pattern " ++ name.txt); */
        let stamp = Ident.binding_time(ident);
        addForPattern(stamp, name);
      }
      | _ => ()
    };
    if (Collector.allLocations) {
      addLocation(pat_loc, Loc.Typed(Shared.makeFlexible(pat_type), Loc.NotFound));
    };
  };

  let enter_expression = expression => {
    expression.exp_extra |. Belt.List.forEach(((e, eloc, _)) => switch e {
      | Texp_open(_, path, ident, _) => {
        extra.opens |. Hashtbl.add(eloc, {
#if 407
          path,
#else
          path: Shared.mapOldPath(path),
#endif
          ident,
          loc: eloc,
          extent: expression.exp_loc,
          used: [],
        })
      }
      | _ => ()
    });
    switch (expression.exp_desc) {
    /* | Texp_apply({exp_desc: Pexp_ident(_, {txt: Ldot(Lident("ReasonReact"), "element")})}, [(_, {exp_desc: Pexp_apply({exp_desc: Pexp_ident(_, {txt})}, _)})]) =>{

    } */
    | Texp_ident(path, {txt, loc}, {val_type}) => {
      addForLongident(Some((Shared.makeFlexible(val_type), Value)), path, txt, loc);
    }
#if 402
    | Texp_record(items, _) => {
      addForRecord(expression.exp_type, items);
#else
    | Texp_record({fields}) => {
      addForRecord(
        expression.exp_type,
        fields |. Array.to_list |. Belt.List.keepMap(((desc, item)) => {
          switch item {
            | Overridden(loc, _) => Some((loc, desc, ()))
            | _ => None
          }
        })
      );
#endif
    }
    | Texp_constant(constant) => {
      addLocation(expression.exp_loc, Loc.Constant(constant));
    }
    /* Skip unit and list literals */
    | Texp_construct({txt: Lident("()" | "::"), loc}, _, args) when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum != 2 =>
      ()
    | Texp_construct(lident, constructor, _args) => {
      addForConstructor(expression.exp_type, lident, constructor);
    }
    | Texp_field(inner, lident, label_description) => {
      addForField(inner.exp_type, label_description, lident)
    }
    | Texp_let(_, _, _) => {
      addScopeExtent(expression.exp_loc)
      /* TODO this scope tracking won't work for recursive */
    }
#if 402
    | Texp_function(_label, cases, _partial) => {
#else
    | Texp_function({cases}) => {
#endif
      switch cases {
        | [{c_rhs}] => addScopeExtent(c_rhs.exp_loc)
        | _ => ()
      }
    }
    | _ => ()
    };
    if (Collector.allLocations) {
      addLocation(expression.exp_loc, Loc.Typed(Shared.makeFlexible(expression.exp_type), Loc.NotFound));
    };
  };

  let leave_expression = expression => {
    switch (expression.exp_desc) {
    | Texp_let(_isrec, _bindings, _expr) => {
      popScopeExtent();
    }
#if 402
    | Texp_function(_label, cases, _partial) => {
#else
    | Texp_function({cases}) => {
#endif
      switch cases {
        | [{c_rhs}] => popScopeExtent()
        | _ => ()
      }
    }
    | _ => ()
    };

  };
};

let forFile = (~file) => {
  let extra = initExtra();
  let addLocation = (loc, ident) => extra.locations = [(loc, ident), ...extra.locations];
  let addReference = (stamp, loc) => Hashtbl.replace(extra.internalReferences, stamp, [loc, ...Hashtbl.mem(extra.internalReferences, stamp) ? Hashtbl.find(extra.internalReferences, stamp) : []]);
  file.stamps.modules |> Hashtbl.iter((stamp, d) => {
    addLocation(d.name.loc, Loc.Module(Loc.Definition(stamp, Module)));
    addReference(stamp, d.name.loc);
  });
  file.stamps.values |> Hashtbl.iter((stamp, d) => {
    addLocation(d.name.loc, Loc.Typed(d.contents.Value.typ, Loc.Definition(stamp, Value)));
    addReference(stamp, d.name.loc);
  });
  file.stamps.types |> Hashtbl.iter((stamp, d) => {
    addLocation(d.name.loc, Loc.TypeDefinition(d.name.txt, d.contents.Type.typ, stamp));
    addReference(stamp, d.name.loc);
    switch (d.contents.Type.kind) {
      | Record(labels) => labels |> List.iter(({Type.Attribute.stamp, name, typ, typLoc}) => {
        addReference(stamp, name.loc);
        addLocation(name.loc, Loc.Typed(typ, Loc.Definition(d.stamp, Attribute(name.txt))))
      });
      | Variant(constructos) => constructos |> List.iter(({Type.Constructor.stamp, name}) => {
        addReference(stamp, name.loc);
        let t = {
          Types.id: 0,
          level: 0, 
#if 407
          desc: Tconstr(Path.Pident(
            /* makeIdent(d.name.txt, stamp, 0) */
            Ident.create(d.name.txt)
            ), [], ref(Types.Mnil)),
          scope: None
#else
          desc: Tconstr(Path.Pident({Ident.stamp, name: d.name.txt, flags: 0}), [], ref(Types.Mnil))
#endif
         };
        addLocation(name.loc, Loc.Typed(Shared.makeFlexible(t), Loc.Definition(d.stamp, Constructor(name.txt))))
      });
      | _ => ()
    };
  });

  extra;
};

let forItems = (~file, ~allLocations, items, parts) => {
  let extra = forFile(~file);

  let extent = ProcessCmt.itemsExtent(items);
  let extent = {...extent, loc_end: {
    ...extent.loc_end,
    pos_lnum: extent.loc_end.pos_lnum + 1000000,
    pos_cnum: extent.loc_end.pos_cnum + 100000000,
  }};

  /* TODO look through parts and extend the extent */

  let module Iter = TypedtreeIter.MakeIterator(F({
    let scopeExtent = ref([extent]);
    let extra = extra;
    let file = file;
    let allLocations = allLocations;
  }));

  List.iter(Iter.iter_structure_item, items);
  /* Log.log("Parts " ++ string_of_int(Array.length(parts))); */

  parts |. Belt.Array.forEach(part => switch part {
  | Cmt_format.Partial_signature(str) =>
    Iter.iter_signature(str);
  | Partial_signature_item(str) =>
    Iter.iter_signature_item(str);
  | Partial_expression(expression) =>
    Iter.iter_expression(expression);
  | Partial_pattern(pattern) =>
    Iter.iter_pattern(pattern);
  | Partial_class_expr(class_expr) =>
    Iter.iter_class_expr(class_expr);
  | Partial_module_type(module_type) =>
    Iter.iter_module_type(module_type);
  | _ => ()
  });

  extra
};

open RResult;
let forCmt = (~file, ~allLocations, {cmt_modname, cmt_annots}: Cmt_format.cmt_infos) => switch cmt_annots {
| Partial_implementation(parts) => {
  let items = parts |. Array.to_list |. Belt.List.keepMap(p => switch p {
    | Partial_structure(str) => Some(str.str_items)
    | Partial_structure_item(str) => Some([str])
    /* | Partial_expression(exp) => Some([ str]) */
    | _ => None
  }) |> List.concat;
  Ok(forItems(~file, ~allLocations, items, parts))
}
| Implementation(structure) => {
  Ok(forItems(~file, ~allLocations, structure.str_items, [||]))
}
| Partial_interface(_)
| Interface(_) => {
  /** TODO actually process signature items */
  Ok(forItems(~file, ~allLocations, [], [||]))
}
| _ => Error("Invalid cmt file")
};
