
open Typedtree;
open SharedTypes;
open Infix;

let rec dig = (typ) =>
  switch typ.Types.desc {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | _ => typ
  };

module F = (Collector: {let extra: extra; let file: file}) => {
  let extra = Collector.extra;

  let addLocation = (loc, ident) => extra.locations = [(loc, ident), ...extra.locations];
  let addReference = (stamp, loc) => Hashtbl.replace(extra.internalReferences, stamp, [loc, ...Hashtbl.mem(extra.internalReferences, stamp) ? Hashtbl.find(extra.internalReferences, stamp) : []]);
  let addExternalReference = (moduleName, path, tip, loc) => Hashtbl.replace(extra.externalReferences, moduleName, [(path, tip, loc), ...Hashtbl.mem(extra.externalReferences, moduleName) ? Hashtbl.find(extra.externalReferences, moduleName) : []]);
  let env = {Query.file: Collector.file, exported: Collector.file.contents.exported};

  let getTypeAtPath = path => {
    switch (Query.fromCompilerPath(~env, path)) {
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

  let addForPath = (path, lident, loc, typ, tip) => {
    let identName = Longident.last(lident);
    let identLoc = Utils.endOfLocation(loc, String.length(identName));
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
    };
    addLocation(loc, Loc.Typed(typ, locType));
  };

  let addForField = (recordType, item, {Asttypes.txt, loc}) => {
    switch (dig(recordType).desc) {
      | Tconstr(path, _args, _memo) => {
        let t = getTypeAtPath(path);
        let {Types.lbl_loc, lbl_res} = item;
        let name = Longident.last(txt);
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
        addLocation(nameLoc, Loc.Typed(lbl_res, locType))
      }
      | _ => ()
    }
  };

  let addForRecord = (recordType, items) => {
    switch (dig(recordType).desc) {
      | Tconstr(path, _args, _memo) => {
        let t = getTypeAtPath(path);
        items |> List.iter((({Asttypes.txt, loc}, {Types.lbl_loc, lbl_res}, _)) => {
          let name = Longident.last(txt);
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
          addLocation(nameLoc, Loc.Typed(lbl_res, locType))
        })
      }
      | _ => ()
    }
  };

  let addForConstructor = (constructorType, {Asttypes.txt, loc}, {Types.cstr_name, cstr_loc}) => {
    switch (dig(constructorType).desc) {
      | Tconstr(path, _args, _memo) => {
        let name = Longident.last(txt);
        let nameLoc = Utils.endOfLocation(loc, String.length(name));
        let locType = switch (getTypeAtPath(path)) {
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
        addLocation(nameLoc, Loc.Typed(constructorType, locType));
      }
      | _ => ()
    }
  };

  open Typedtree;
  include TypedtreeIter.DefaultIteratorArgument;
  let enter_structure_item = item => switch (item.str_desc) {
  | Tstr_attribute(({Asttypes.txt: "ocaml.explanation", loc}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}]))) => {
    addLocation(loc, Loc.Explanation(doc))
  }
  /* | Tstr_type(decls)  */
  | _ => ()
  };

  let enter_signature_item = item => switch (item.sig_desc) {
  | Tsig_value({val_id: {stamp}, val_loc, val_name: name, val_desc, val_attributes}) => {
    if (!Hashtbl.mem(Collector.file.stamps.values, stamp)) {
      let declared = ProcessCmt.newDeclared(
        ~name,
        ~stamp,
        ~extent=val_loc,
        ~modulePath=NotVisible,
        ~processDoc=x => x,
        ~contents={Value.typ: val_desc.ctyp_type, recursive: false},
        false,
        val_attributes
      );
      Hashtbl.add(Collector.file.stamps.values, stamp, declared);
      addReference(stamp, name.loc);
      addLocation(name.loc, Loc.Typed(val_desc.ctyp_type, Loc.Definition(stamp, Value)));
    }
  }
  | _ => ()
  };

  let enter_core_type = ({ctyp_loc, ctyp_type, ctyp_desc}) => {
    switch (ctyp_desc) {
      | Ttyp_constr(path, {txt, loc}, args) => addForPath(path, txt, loc, ctyp_type, Type)
      | _ => ()
    }
  };

  let enter_pattern = ({pat_desc, pat_loc, pat_type, pat_attributes}) => {
    switch (pat_desc) {
      | Tpat_record(items, _) => {
        addForRecord(pat_type, items);
      }
      | Tpat_var({stamp}, name) => {
        if (!Hashtbl.mem(Collector.file.stamps.values, stamp)) {
          let declared = ProcessCmt.newDeclared(
            ~name,
            ~stamp,
            ~modulePath=NotVisible,
            ~extent=pat_loc,
            ~processDoc=x => x,
            ~contents={Value.typ: pat_type, recursive: false},
            false,
            pat_attributes
          );
          Hashtbl.add(Collector.file.stamps.values, stamp, declared);
          addReference(stamp, name.loc);
          addLocation(name.loc, Loc.Typed(pat_type, Loc.Definition(stamp, Value)));
        }
      }
      | _ => ()
    }
  };

  let enter_expression = expression => {
    expression.exp_extra |. Belt.List.forEach(((e, eloc, _)) => switch e {
      | Texp_open(_, path, ident, _) => {
        extra.opens |. Hashtbl.add(eloc, {
          path,
          ident,
          loc: eloc,
          extent: expression.exp_loc,
          used: Hashtbl.create(5),
          useCount: 0,
        })
      }
      | _ => ()
    });
    switch (expression.exp_desc) {
      | Texp_ident(path, {txt, loc}, {val_type}) => {
        addForPath(path, txt, loc, val_type, Value);
      }
      | Texp_record(items, _) => {
        addForRecord(expression.exp_type, items);
      }
      | Texp_construct(lident, constructor, _args) => {
        addForConstructor(expression.exp_type, lident, constructor);
      }
      | Texp_field(inner, lident, label_description) => {
        addForField(inner.exp_type, label_description, lident)
      }
      | _ => ()
    }
  };
};

let noType = {Types.id: 0, level: 0, desc: Tnil};

let forCmt = (~file, {cmt_modname, cmt_annots}: Cmt_format.cmt_infos) => switch cmt_annots {
| Implementation(structure) => {
  let extra = initExtra();
  let addLocation = (loc, ident) => extra.locations = [(loc, ident), ...extra.locations];
  let addReference = (stamp, loc) => Hashtbl.replace(extra.internalReferences, stamp, [loc, ...Hashtbl.mem(extra.internalReferences, stamp) ? Hashtbl.find(extra.internalReferences, stamp) : []]);
  file.stamps.modules |> Hashtbl.iter((stamp, d) => {
    addLocation(d.name.loc, Loc.Typed(noType, Loc.Definition(stamp, Module)));
    addReference(stamp, d.name.loc);
  });
  file.stamps.values |> Hashtbl.iter((stamp, d) => {
    addLocation(d.name.loc, Loc.Typed(d.contents.Value.typ, Loc.Definition(stamp, Value)));
    addReference(stamp, d.name.loc);
  });
  file.stamps.types |> Hashtbl.iter((stamp, d) => {
    addLocation(d.name.loc, Loc.Typed(noType, Loc.Definition(stamp, Type)));
    addReference(stamp, d.name.loc);
    switch (d.contents.Type.kind) {
      | Record(labels) => labels |> List.iter(({Type.Attribute.stamp, name, typ, typLoc}) => {
        addReference(stamp, name.loc);
        addLocation(name.loc, Loc.Typed(typ, Loc.Definition(d.stamp, Attribute(name.txt))))
      });
      | Variant(constructos) => constructos |> List.iter(({Type.Constructor.stamp, name}) => {
        addReference(stamp, name.loc);
        addLocation(name.loc, Loc.Typed({Types.id: 0, level: 0, desc: Tconstr(Path.Pident({Ident.stamp, name: d.name.txt, flags: 0}), [], ref(Types.Mnil))}, Loc.Definition(d.stamp, Constructor(name.txt))))
      });
      | _ => ()
    };
  });
  let module Iter = TypedtreeIter.MakeIterator(F({let extra = extra; let file = file;}));
  List.iter(Iter.iter_structure_item, structure.str_items);
  Some(extra)
}
| _ => None
};
