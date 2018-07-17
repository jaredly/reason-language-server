
open Typedtree;
open SharedTypes;
open Infix;

let locStartPos = ({Location.loc_start: {pos_cnum, pos_lnum, pos_bol}}) => (
  pos_lnum,
  pos_cnum - pos_bol
);

type env = {
  stamps,
  processDoc: string => string,
  modulePath: visibilityPath,
};

let getTopDoc = structure => {
  switch structure {
  | [{str_desc: Tstr_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])))}, ...rest] => (Some(doc), rest)
  | _ => (None, structure)
  };
};

let getTopSigDoc = structure => {
  switch structure {
  | [{sig_desc: Tsig_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])))}, ...rest] => (Some(doc), rest)
  | _ => (None, structure)
  };
};

let newDeclared = (~contents, ~extent, ~name, ~stamp, ~modulePath, ~processDoc, exported, attributes) => {
  {
    name,
    stamp,
    extentLoc: extent,
    deprecated: PrepareUtils.findDeprecatedAttribute(attributes),
    exported,
    modulePath,
    docstring: PrepareUtils.findDocAttribute(attributes) |?>> processDoc,
    contents,
    /* scopeType: Let, */
    /* scopeStart: env.scopeStart, */
  };
};

let addItem = (~name, ~extent, ~stamp, ~env, ~contents, attributes, exported, stamps) => {
  let declared = newDeclared(
    ~contents,
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

let rec forSignatureType = (env, signature) => {
  open Types;
  let exported = Module.initExported();
  let topLevel = List.fold_right((item, items) => switch item {
  | Sig_value({stamp, name}, {val_type, val_kind, val_attributes, val_loc: loc}) => {
    let contents = {
      Value.recursive: false,
      typ: val_type,
    };
    let declared = addItem(
      ~name=Location.mknoloc(name),
      ~extent=loc,
      ~stamp,
      ~env,
      ~contents,
      val_attributes,
      exported.values,
      env.stamps.values,
    );
    [{...declared, contents: Module.Value(declared.contents)}, ...items]
  }
  | Sig_type({stamp, name}, {type_params, type_loc, type_kind, type_attributes} as decl, _) => {
    let declared = addItem(~extent=type_loc, ~contents={
      Type.params: type_params |> List.map(t => (t, Location.none)),
      typ: decl,
      kind: switch type_kind {
        | Type_abstract => Abstract
        | Type_open => Open
        | Type_variant(constructors) => Variant(constructors |> List.map(({cd_id: {stamp, name}, cd_args, cd_res, cd_attributes}) => {
          Type.Constructor.stamp,
          name: Location.mknoloc(name),
          args: cd_args |> List.map(t => (t, Location.none)),
          res: cd_res,
        }))
        | Type_record(labels, _) => Record(labels |> List.map(
          ({ld_id: {stamp: astamp, name}, ld_type}) => {
            {Type.Attribute.stamp: astamp, name: Location.mknoloc(name), typ: ld_type, typLoc: Location.none}
          }
        ))
      }
    }, ~name=Location.mknoloc(name), ~stamp, ~env, type_attributes, exported.types, env.stamps.types);
    [{...declared, contents: Module.Type(declared.contents)}, ...items]
  }
  /* | Sig_module({stamp, name}, {md_type: Mty_ident(path) | Mty_alias(path), md_attributes, md_loc}, _) =>
    let declared = addItem(~contents=Module.Ident(path), ~name=Location.mknoloc(name), ~stamp, ~env, md_attributes, exported.modules, env.stamps.modules);
    [{...declared, contents: Module.Module(declared.contents)}, ...items] */
  | Sig_module({stamp, name}, {md_type, md_attributes, md_loc}, _) =>
    let declared = addItem(~extent=md_loc, ~contents=forModuleType(env, md_type), ~name=Location.mknoloc(name), ~stamp, ~env, md_attributes, exported.modules, env.stamps.modules);
    [{...declared, contents: Module.Module(declared.contents)}, ...items]
  | _ => items
  }, signature, []) |> List.rev;

  {Module.exported, topLevel}
} and forModuleType = (env, moduleType) => switch moduleType {
  | Types.Mty_ident(path) => Module.Ident(path)
  | Mty_alias(path) => Module.Ident(path)
  | Mty_signature(signature) => {
    Module.Structure(forSignatureType(env, signature))
  }
  | Mty_functor(argIdent, argType, resultType) => forModuleType(env, resultType)
};

let forSignatureItem = (~env, ~exported: Module.exported, item) => {
  switch (item.sig_desc) {
  | Tsig_value({val_id: {stamp}, val_loc, val_name: name, val_desc, val_attributes}) => {
    let declared = addItem(
      ~name,
      ~stamp,
      ~extent=val_loc,
      ~contents={Value.typ: val_desc.ctyp_type, recursive: false},
      ~env,
      val_attributes,
      exported.values,
      env.stamps.values
    );
    Some({...declared, contents: Module.Value(declared.contents)})
  }
  | _ => None
  }
};

let rec forSignature = (~env, items) => {
  let (doc, items) = getTopSigDoc(items);
  let exported = Module.initExported();
  let topLevel = Belt.List.keepMap(items, forSignatureItem(~env, ~exported));
  (doc, {Module.exported, topLevel})
};

let rec forTreeModuleType = (~env, {mty_desc, mty_loc, mty_attributes}) => switch mty_desc {
  | Tmty_ident(_) => None
  | Tmty_signature({sig_items}) => {
    let (doc, contents) = forSignature(~env, sig_items);
    Some(Module.Structure(contents))
  }
  | _ => None
};

let rec forItem = (
  ~env,
  ~exported: Module.exported,
  item
) => switch (item.str_desc) {
| Tstr_value(isRec, bindings) => 
  optMap(({vb_loc, vb_expr, vb_pat: {pat_desc, pat_type}, vb_attributes}) =>
    /* TODO get all the things out of the var. */
    switch (pat_desc) {
      | Tpat_var({stamp}, {txt, loc} as name) =>
        let contents = {
          Value.recursive: false,
          typ: pat_type,
        };
        let declared = addItem(~name, ~stamp, ~env, ~extent=vb_loc, ~contents, vb_attributes, exported.values, env.stamps.values);
        Some({...declared, contents: Module.Value(declared.contents)})
      | _ => None
    },
  bindings)
| Tstr_module({mb_id: {stamp}, mb_attributes, mb_loc, mb_name: name, mb_expr: {mod_desc}}) => {
  let contents = forModule(env, mod_desc, name.txt);
  let declared = addItem(~contents, ~name, ~extent=mb_loc, ~stamp, ~env, mb_attributes, exported.modules, env.stamps.modules);
  [{...declared, contents: Module.Module(declared.contents)}]
}
| Tstr_primitive({val_id: {stamp}, val_name: name, val_loc, val_attributes, val_val: {val_type}}) => {
  let declared = addItem(~extent=val_loc, ~contents={Value.recursive: false, typ: val_type}, ~name, ~stamp, ~env, val_attributes, exported.values, env.stamps.values);
  [{...declared, contents: Module.Value(declared.contents)}]
}
| Tstr_type(decls) => decls |> List.map(({typ_id: {stamp}, typ_loc, typ_params, typ_name: name, typ_attributes, typ_type, typ_kind}) => {
  let declared = addItem(~extent=typ_loc, ~contents={
    Type.params: typ_params |> List.map(((t, _)) => (t.ctyp_type, t.ctyp_loc)),
    typ: typ_type,
    kind: switch typ_kind {
      | Ttype_abstract => Abstract
      | Ttype_open => Open
      | Ttype_variant(constructors) => Variant(constructors |> List.map(({cd_id: {stamp}, cd_name: name, cd_args, cd_res, cd_attributes}) => {
        Type.Constructor.stamp,
        name,
        args: cd_args |> List.map(t => (t.ctyp_type, t.ctyp_loc)),
        res: cd_res |?>> t => t.ctyp_type,
      }))
      | Ttype_record(labels) => Record(labels |> List.map(
        ({ld_id: {stamp: astamp}, ld_name: name, ld_type: {ctyp_type, ctyp_loc}}) => {
          {Type.Attribute.stamp: astamp, name, typ: ctyp_type, typLoc: ctyp_loc}
        }
      ))
    }
  }, ~name, ~stamp, ~env, typ_attributes, exported.types, env.stamps.types);
  {...declared, contents: Module.Type(declared.contents)}
})
  | _ => []
}

and forModule = (env, mod_desc, moduleName) => switch mod_desc {
  | Tmod_ident(path, lident) => Module.Ident(path)
  | Tmod_structure(structure) => {
    let env = {...env, modulePath: ExportedModule(moduleName, env.modulePath)};
    let (doc, contents) = forStructure(~env, structure.str_items);
    Module.Structure(contents)
  }
  | Tmod_functor({stamp}, argName, maybeType, resultExpr) => {
    maybeType |?< t => forTreeModuleType(~env, t) |?< kind => {
      let declared = newDeclared(
        ~contents=kind,
        ~name=argName,
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
  | Tmod_apply(functor_, arg, coersion) => forModule(env, functor_.mod_desc, moduleName)
  | Tmod_unpack(expr, moduleType) => forModuleType(env, moduleType)
  | Tmod_constraint(expr, typ, constraint_, coersion) => {
    /* TODO do this better I think */
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

open Result;
let forCmt = (uri, processDoc, {cmt_modname, cmt_annots}: Cmt_format.cmt_infos) => switch cmt_annots {
| Partial_implementation(parts) => {
  let env = {stamps: initStamps(), processDoc, modulePath: File(uri)};
  let items = parts |. Array.to_list |. Belt.List.keepMap(p => switch p {
    | Partial_structure(str) => Some(str.str_items)
    | Partial_structure_item(str) => Some([str])
    | _ => None
  }) |> List.concat;
  let (docstring, contents) = forStructure(~env, items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| Partial_interface(parts) => {
  let env = {stamps: initStamps(), processDoc, modulePath: File(uri)};
  let items = parts |. Array.to_list |. Belt.List.keepMap(p => switch p {
    | Partial_signature(str) => Some(str.sig_items)
    | Partial_signature_item(str) => Some([str])
    | _ => None
  }) |> List.concat;
  let (docstring, contents) = forSignature(~env, items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| Implementation(structure) => {
  let env = {stamps: initStamps(), processDoc, modulePath: File(uri)};
  let (docstring, contents) = forStructure(~env, structure.str_items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
}
| Interface(signature) => {
  let env = {stamps: initStamps(), processDoc, modulePath: File(uri)};
  let (docstring, contents) = forSignature(~env, signature.sig_items);
  Ok({uri, moduleName: cmt_modname, stamps: env.stamps, docstring, contents})
  /* Some(forSignature(processDoc, signature.sig_items)) */
}
| _ => {
  Error("Not a valid cmt")
}
};

let forCmi = (uri, processDoc, {cmi_name, cmi_sign}: Cmi_format.cmi_infos) => {
  let env = {stamps: initStamps(), processDoc, modulePath: File(uri)};
  let contents = forSignatureType(env, cmi_sign);
  Some({
    uri,
    moduleName: cmi_name,
    stamps: env.stamps,
    docstring: Some("No docstring for cmi files"),
    contents,
  });
  /* Some(forSignature(processDoc, signature.sig_items)) */
};

/* let forCmi = (processDoc, {Cmi_format.cmi_name, cmi_sign}) => {
  let items = forSignatureType(processDoc, cmi_sign);
  /* TODO: want the toplevel docs... */
  Some((None, items))
}; */


