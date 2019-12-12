
open Compiler_libs_402;
open Typedtree;
open SharedTypes;
open Infix;



let getTopDoc = structure => {
  switch structure {
  | [{str_desc: Tstr_attribute(
      ({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
    Const_string
    (doc, _))}, _)}])))}, ...rest] =>
    (Some(doc), rest)
  | _ => (None, structure)
  };
};

let getTopSigDoc = structure => {
  switch structure {
  | [{sig_desc: Tsig_attribute(
    ({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
    Const_string
    (doc, _))}, _)}])))}, ...rest] =>
    (Some(doc), rest)
  | _ => (None, structure)
  };
};

let itemsExtent = items => {
  open Typedtree;
  items == [] ? Location.none : {
    let first = List.hd(items);
    let last = List.nth(items, List.length(items) - 1);
    let (first, last) = first.str_loc.loc_start.pos_cnum <
      last.str_loc.loc_start.pos_cnum ? (first, last) : (last, first);

    {
      loc_ghost: true,
      loc_start: first.str_loc.loc_start,
      loc_end: last.str_loc.loc_end,
    };
  };
};

let sigItemsExtent = items => {
  open Typedtree;
  items == [] ? Location.none : {
    let first = List.hd(items);
    let last = List.nth(items, List.length(items) - 1);

    {
      Location.loc_ghost: true,
      loc_start: first.sig_loc.loc_start,
      loc_end: last.sig_loc.loc_end,
    };
  };
};

let locStartPos = ({Location.loc_start: {pos_cnum, pos_lnum, pos_bol}}) => (
  pos_lnum,
  pos_cnum - pos_bol
);

type env = {
  stamps,
  processDoc: string => string,
  modulePath: visibilityPath,
  scope: Location.t,
};

let newDeclared = ProcessAttributes.newDeclared;

let addItem = (~name, ~extent, ~stamp, ~env, ~contents, attributes, exported, stamps) => {
  let declared = newDeclared(
    ~contents,
    ~scope={
      Location.loc_start: extent.Location.loc_end,
      loc_end: env.scope.loc_end,
      loc_ghost: false,
    },
    ~extent,
    ~name,
    ~stamp,
    ~modulePath=env.modulePath,
    ~processDoc=env.processDoc,
    ! Hashtbl.mem(exported, name.txt),
    attributes,
  );
  if (!Hashtbl.mem(exported, name.txt)) {
    Hashtbl.add(exported, name.txt, stamp);
  };
  Hashtbl.add(stamps, stamp, declared);
  declared;
};

let rec forSignatureTypeItem = (env, exported: SharedTypes.Module.exported, item) => {
  open Types;
  switch item {
  | Sig_value(ident, {val_type, val_attributes, val_loc: loc}
  ) => {
    let contents = {
      Value.recursive: false,
      typ: Shared.makeFlexible(val_type),
    };
    let declared = addItem(
      ~name=Location.mknoloc(Ident.name(ident)),
      ~extent=loc,
      ~stamp=Ident.binding_time(ident),
      ~env,
      ~contents,
      val_attributes,
      exported.values,
      env.stamps.values,
    );
    [{...declared, contents: Module.Value(declared.contents)}]
  }
  | Sig_type(
    ident,
    {type_params, type_loc, type_kind, type_manifest, type_attributes} as decl,
    _
    ) => {
    let declared = addItem(~extent=type_loc, ~contents={
      Type.params: type_params |> List.map(t => (Shared.makeFlexible(t), Location.none)),
      typ: Shared.makeDeclaration(decl),
      kind: switch type_kind {
        | Type_abstract =>
        switch (type_manifest) {
          | Some({desc: Tconstr(path, args, _)}) => Abstract(Some((
              Shared.mapOldPath(path),
            args |> List.map(Shared.makeFlexible)
          )))
          | Some({desc: Ttuple(items)}) => Tuple(items |> List.map(Shared.makeFlexible))
          /* TODO dig */
          | _ => Abstract(None)
        }
        | Type_open => Open
        | Type_variant(constructors) => {
          Variant(constructors |. Belt.List.map(({cd_loc, cd_id, cd_args, cd_res, cd_attributes}) => {
            let name = Ident.name(cd_id);
            let stamp = Ident.binding_time(cd_id);
            let contents = {
              Type.Constructor.stamp,
              name: Location.mknoloc(name),
              args: cd_args |>
                List.map(t => (Shared.makeFlexible(t), Location.none)),
              res: cd_res |?>> Shared.makeFlexible,
            };
            let declared = newDeclared(
              ~contents,
              ~extent=cd_loc,
              ~scope={
                Location.loc_start: type_loc.Location.loc_end,
                loc_end: env.scope.loc_end,
                loc_ghost: false,
              },
              ~name=Location.mknoloc(name),
              ~stamp,
              /* TODO maybe this needs another child */
              ~modulePath=env.modulePath,
              ~processDoc=env.processDoc,
              true,
              cd_attributes,
            );
            Hashtbl.add(env.stamps.constructors, stamp, declared);
            contents
          }))
        }
        | Type_record(labels, _) => Record(labels |> List.map(
          ({ld_id, ld_type}) => {
            let astamp = Ident.binding_time(ld_id);
            let name = Ident.name(ld_id);
            {Type.Attribute.stamp: astamp, name: Location.mknoloc(name), typ: Shared.makeFlexible(ld_type), typLoc: Location.none}
          }
        ))
      }
    }, ~name=Location.mknoloc(Ident.name(ident)), ~stamp=Ident.binding_time(ident), ~env, type_attributes, exported.types, env.stamps.types);
    [{...declared, contents: Module.Type(declared.contents)}]
  }
  /* | Sig_module({stamp, name}, {md_type: Mty_ident(path) | Mty_alias(path), md_attributes, md_loc}, _) =>
    let declared = addItem(~contents=Module.Ident(path), ~name=Location.mknoloc(name), ~stamp, ~env, md_attributes, exported.modules, env.stamps.modules);
    [{...declared, contents: Module.Module(declared.contents)}, ...items] */
  | Sig_module(
    ident,
    {md_type, md_attributes, md_loc},
    _
  ) =>
    let declared = addItem(
      ~extent=md_loc,
      ~contents=forModuleType(env, md_type),
      ~name=Location.mknoloc(Ident.name(ident)),
      ~stamp=Ident.binding_time(ident),
      ~env,
      md_attributes,
      exported.modules,
      env.stamps.modules);
    [{...declared, contents: Module.Module(declared.contents)}]
  | _ => []
  }
}

and forSignatureType = (env, signature) => {
  let exported = Module.initExported();
  let topLevel = List.fold_right((item, items) => {
    forSignatureTypeItem(env, exported, item) @ items
  }, signature, []) |> List.rev;

  {Module.exported, topLevel}
} and forModuleType = (env, moduleType) => switch moduleType {
  | Types.Mty_ident(path) => Module.Ident(Shared.mapOldPath(path))
  | Mty_alias(path) =>
    Module.Ident(Shared.mapOldPath(path))
  | Mty_signature(signature) => {
    Module.Structure(forSignatureType(env, signature))
  }
  | Mty_functor(_argIdent, _argType, resultType) => forModuleType(env, resultType)
};

let getModuleTypePath = (mod_desc) => switch mod_desc {
  | Tmty_ident(path, _)
  | Tmty_alias(path, _) => Some(path)
  | Tmty_signature(_)
  | Tmty_functor(_)
  | Tmty_with(_)
  | Tmty_typeof(_) => None
};

let forTypeDeclaration = (~env, ~exported: Module.exported, {typ_id, typ_loc, typ_params, typ_name: name, typ_attributes, typ_type, typ_kind, typ_manifest}) => {
  let stamp = Ident.binding_time(typ_id);
  let declared = addItem(~extent=typ_loc, ~contents={
    Type.params: typ_params |> List.map(((t, _)) => (Shared.makeFlexible(t.ctyp_type), t.ctyp_loc)),
    typ: Shared.makeDeclaration(typ_type),
    kind: switch typ_kind {
      | Ttype_abstract =>
        switch (typ_manifest) {
          | Some({ctyp_desc: Ttyp_constr(path, _lident, args)}) => Abstract(Some((
            Shared.mapOldPath(path),
            args |> List.map(t => Shared.makeFlexible(t.ctyp_type))
          )))
          | Some({ctyp_desc: Ttyp_tuple(items)}) => Tuple(items |> List.map(t => Shared.makeFlexible(t.ctyp_type)))
          /* TODO dig */
          | _ => Abstract(None)
        }
      | Ttype_open => Open
      | Ttype_variant(constructors) => Variant(constructors |> List.map(({cd_id, cd_name: name, cd_args, cd_res}) => {
        let stamp = Ident.binding_time(cd_id);
      {
        Type.Constructor.stamp,
        name,
        args: cd_args |>
          List.map(t => (Shared.makeFlexible(t.ctyp_type), t.ctyp_loc)),
        res: cd_res |?>> t => Shared.makeFlexible(t.ctyp_type),
      }}))
      | Ttype_record(labels) => Record(labels |> List.map(
        ({ld_id, ld_name: name, ld_type: {ctyp_type, ctyp_loc}}) => {
        let astamp = Ident.binding_time(ld_id);
          {Type.Attribute.stamp: astamp, name, typ: Shared.makeFlexible(ctyp_type), typLoc: ctyp_loc}
        }))
    }
  }, ~name, ~stamp, ~env, typ_attributes, exported.types, env.stamps.types);
  {...declared, contents: Module.Type(declared.contents)}
};

let forSignatureItem = (~env, ~exported: Module.exported, item) => {
  switch (item.sig_desc) {
  | Tsig_value({val_id, val_loc, val_name: name, val_desc, val_attributes}) => {
    let declared = addItem(
      ~name,
      ~stamp=Ident.binding_time(val_id),
      ~extent=val_loc,
      ~contents={Value.typ: Shared.makeFlexible(val_desc.ctyp_type), recursive: false},
      ~env,
      val_attributes,
      exported.values,
      env.stamps.values
    );
    [{...declared, contents: Module.Value(declared.contents)}]
  }
  | Tsig_type(decls) => {
    decls |. Belt.List.map(forTypeDeclaration(~env, ~exported))
  }
  | Tsig_module({md_id, md_attributes, md_loc, md_name: name, md_type: {mty_type}}) => {
    let contents = forModuleType(env, mty_type);
    let declared = addItem(~contents, ~name, ~extent=md_loc, ~stamp=Ident.binding_time(md_id), ~env, md_attributes, exported.modules, env.stamps.modules);
    [{...declared, contents: Module.Module(declared.contents)}]
  }
  | Tsig_include({incl_mod, incl_type}) =>
    let env = switch (getModuleTypePath(incl_mod.mty_desc)) {
      | None => env
      | Some(path) => {
        ...env,
        modulePath: IncludedModule(Shared.mapOldPath(path), env.modulePath)
      }
    };
    let topLevel = List.fold_right((item, items) => {
      forSignatureTypeItem(env, exported, item) @ items
    }, incl_type, []) |> List.rev;

    topLevel
  /* TODO: process other things here */
  | _ => []
  }
};

let forSignature = (~env, items) => {
  let (doc, items) = getTopSigDoc(items);
  let exported = Module.initExported();
  let topLevel = Belt.List.map(items, forSignatureItem(~env, ~exported)) |> Belt.List.flatten;
  (doc, {Module.exported, topLevel})
};

let forTreeModuleType = (~env, {mty_desc}) => switch mty_desc {
  | Tmty_ident(_) => None
  | Tmty_signature({sig_items}) => {
    let (_doc, contents) = forSignature(~env, sig_items);
    Some(Module.Structure(contents))
  }
  | _ => None
};

let rec getModulePath = (mod_desc) => switch mod_desc {
  | Tmod_ident(path, _lident) => Some(path)
  | Tmod_structure(_) => None
  | Tmod_functor(_ident, _argName, _maybeType, _resultExpr) => None
  | Tmod_apply(functor_, _arg, _coercion) => getModulePath(functor_.mod_desc)
  | Tmod_unpack(_expr, _moduleType) => None
  | Tmod_constraint(expr, _typ, _constraint, _coercion) => getModulePath(expr.mod_desc)
};

let rec forItem = (
  ~env,
  ~exported: Module.exported,
  item
) => switch (item.str_desc) {
| Tstr_value(_isRec, bindings) =>
  optMap(({vb_loc, vb_pat: {pat_desc, pat_type}, vb_attributes}) =>
    /* TODO get all the things out of the var. */
    switch (pat_desc) {
      | Tpat_var(ident, name) =>
        let contents = {
          Value.recursive: false,
          typ: Shared.makeFlexible(pat_type),
        };
        let declared = addItem(~name, ~stamp=Ident.binding_time(ident), ~env, ~extent=vb_loc, ~contents, vb_attributes, exported.values, env.stamps.values);
        Some({...declared, contents: Module.Value(declared.contents)})
      | _ => None
    },
  bindings)
| Tstr_module({mb_id, mb_attributes, mb_loc, mb_name: name, mb_expr: {mod_desc}}) => {
  let contents = forModule(env, mod_desc, name.txt);
  let declared = addItem(~contents, ~name, ~extent=mb_loc, ~stamp=Ident.binding_time(mb_id), ~env, mb_attributes, exported.modules, env.stamps.modules);
  [{...declared, contents: Module.Module(declared.contents)}]
}
| Tstr_include({incl_mod, incl_type}) =>
  let env = switch (getModulePath(incl_mod.mod_desc)) {
    | None => env
    | Some(path) => {
      ...env,
      modulePath: IncludedModule(Shared.mapOldPath(path), env.modulePath)
     }
  };
  let topLevel = List.fold_right((item, items) => {
    forSignatureTypeItem(env, exported, item) @ items
  }, incl_type, []) |> List.rev;

  topLevel

| Tstr_primitive({val_id, val_name: name, val_loc, val_attributes, val_val: {val_type}}) => {
  let declared = addItem(~extent=val_loc, ~contents={Value.recursive: false, typ: Shared.makeFlexible(val_type)}, ~name, ~stamp=Ident.binding_time(val_id), ~env, val_attributes, exported.values, env.stamps.values);
  [{...declared, contents: Module.Value(declared.contents)}]
}
| Tstr_type(decls) =>
  decls |> List.map(forTypeDeclaration(~env, ~exported))
| _ => []
}

and forModule = (env, mod_desc, moduleName) => switch mod_desc {
  | Tmod_ident(path, _lident) => Module.Ident(Shared.mapOldPath(path))
  | Tmod_structure(structure) => {
    let env = {...env, scope: itemsExtent(structure.str_items), modulePath: ExportedModule(moduleName, env.modulePath)};
    let (_doc, contents) = forStructure(~env, structure.str_items);
    Module.Structure(contents)
  }
  | Tmod_functor(ident, argName, maybeType, resultExpr) => {
    maybeType |?< t => forTreeModuleType(~env, t) |?< kind => {
      let stamp = Ident.binding_time(ident);
      let declared = newDeclared(
        ~contents=kind,
        ~name=argName,
        ~scope={
          Location.loc_start: t.mty_loc.loc_end,
          loc_end: env.scope.loc_end,
          loc_ghost: false,
        },
        ~extent=t.Typedtree.mty_loc,
        ~stamp,
        ~modulePath=NotVisible,
        ~processDoc=env.processDoc,
        false,
        [],
      );
      Hashtbl.add(env.stamps.modules, stamp, declared);
    };
    forModule(env, resultExpr.mod_desc, moduleName)
  }
  | Tmod_apply(functor_, _arg, _coercion) => forModule(env, functor_.mod_desc, moduleName)
  | Tmod_unpack(_expr, moduleType) =>
    let env = {...env, modulePath: ExportedModule(moduleName, env.modulePath)};
    forModuleType(env, moduleType)
  | Tmod_constraint(_expr, typ, _constraint, _coercion) => {
    /* TODO do this better I think */
    let env = {...env, modulePath: ExportedModule(moduleName, env.modulePath)};
    forModuleType(env, typ)
  }
}

and forStructure = (~env, items) => {
  let (doc, items) = getTopDoc(items);
  let exported = Module.initExported();
  let topLevel = List.fold_right((item, results) => {
    forItem(~env, ~exported, item) @ results
  }, items, []);
  (doc, {Module.exported, topLevel})
};

open RResult;
let forCmt = (~moduleName, uri, processDoc, {cmt_modname, cmt_annots}: Cmt_format.cmt_infos) => switch cmt_annots {
| Partial_implementation(parts) => {

  let items = parts |. Array.to_list |. Belt.List.keepMap(p => switch p {
    | Partial_structure(str) => Some(str.str_items)
    | Partial_structure_item(str) => Some([str])
    | _ => None
  }) |> List.concat;
  let extent = itemsExtent(items);
  let extent = {...extent, loc_end: {
    ...extent.loc_end,
    pos_lnum: extent.loc_end.pos_lnum + 1000000,
    pos_cnum: extent.loc_end.pos_cnum + 100000000,
  }};
  let env = {
    scope: extent,
    stamps: initStamps(),
    processDoc,
    modulePath: File(uri, moduleName)
  };
  let (docstring, contents) = forStructure(~env, items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| Partial_interface(parts) => {
  let items = parts |. Array.to_list |. Belt.List.keepMap(p => switch p {
    | Partial_signature(str) => Some(str.sig_items)
    | Partial_signature_item(str) => Some([str])
    | _ => None
  }) |> List.concat;
  let env = {scope: sigItemsExtent(items), stamps: initStamps(), processDoc, modulePath: File(uri, moduleName)};
  let (docstring, contents) = forSignature(~env, items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| Implementation(structure) => {
  let env = {scope: itemsExtent(structure.str_items), stamps: initStamps(), processDoc, modulePath: File(uri, moduleName)};
  let (docstring, contents) = forStructure(~env, structure.str_items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| Interface(signature) => {
  let env = {scope: sigItemsExtent(signature.sig_items), stamps: initStamps(), processDoc, modulePath: File(uri, moduleName)};
  let (docstring, contents) = forSignature(~env, signature.sig_items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| _ => {
  Error("Not a valid cmt")
}
};

let forCmi = (~moduleName, uri, processDoc, {cmi_name, cmi_sign}: Cmi_format.cmi_infos) => {
  let env = {scope: Location.none, stamps: initStamps(), processDoc, modulePath: File(uri, moduleName)};
  let contents = forSignatureType(env, cmi_sign);
  Some({
    uri,
    moduleName: cmi_name,
    stamps: env.stamps,
    docstring: Some("No docstring for cmi files"),
    contents,
  });
};
