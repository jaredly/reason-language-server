

/*

module ->
- the docs for the module
- also, the module ~signature

but also, I want it to collect the items & serve them up separately.


What jobs do I need done here?

Initially, just get completable items
including, for functions, signature support n stuff

then for the current file, I need
- hover support
- code lenses

also "jump to definition"
and "find all references"

*/
open PrepareUtils;

type item =
  | Module(list(full))
  | Function(list(Types.type_expr), Types.type_expr)
  | Value(Types.type_expr)
  | Type(Types.type_declaration)
and full = (string, option(string), item);
/* TODO module types n that jazz also functors I guess */

let rec forSignatureType = (signature) => {
  open Types;
  List.fold_left((items, item) => switch item {
  | Sig_value({stamp, name}, {val_type, val_kind, val_attributes}) => [(name, findDocAttribute(val_attributes), Value(val_type)), ...items]
  | Sig_type({stamp, name}, decl, _) => [(name, findDocAttribute(decl.type_attributes), Type(decl)), ...items]
  | Sig_module({stamp, name}, {md_type, md_attributes, md_loc}, _) => [(name, findDocAttribute(md_attributes), Module(forModuleType(md_type)))]
  | _ => items
  }, [], signature);
} and forModuleType = modtype => Types.(switch modtype {
  | Mty_ident(path) | Mty_alias(path) => [] /* TODO resolve */
  | Mty_functor(_, _, tt) => forModuleType(tt)
  | Mty_signature(sign) => forSignatureType(sign)
});



/* TODO need a way to resolve module path aliases */

let rec forSignature = (signature) => {
  open Typedtree;
  let (doc, items) = switch signature {
  | [{sig_desc: Tsig_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])))}, ...rest] => (Some(doc), rest)
  | _ => (None, signature)
  };
  (doc, List.map(forSignatureItem, items) |> List.concat)
} and forSignatureItem = item => Typedtree.(switch (item.sig_desc) {
  | Tsig_value({val_name: {txt, loc}, val_val, val_attributes, val_loc}) =>
      [(txt, findDocAttribute(val_attributes), Value(val_val.val_type))]
  | Tsig_include({incl_loc, incl_mod, incl_attributes, incl_type}) => {
      switch incl_mod.mty_desc {
      | Tmty_ident(path, _) | Tmty_alias(path, _) => forSignatureType(incl_type)
      | _ => forSignatureType(incl_type)
      }
  }
  | Tsig_type(decls) => foldOpt(({typ_name: {txt}, typ_loc, typ_attributes, typ_type}) =>
      Some((txt, findDocAttribute(typ_attributes), Type(typ_type)))
    , decls, [])
  | Tsig_module({md_attributes, md_loc, md_name: {txt}, md_type: module_type}) => {
      let (docc, contents) = forModuleSig(module_type);
      [(txt, either(docc, findDocAttribute(md_attributes)), Module(contents))]
    }
  | _ => []
  })

and forModuleSig = ({Typedtree.mty_desc, mty_attributes, mty_loc}) => {
  open Typedtree;
  switch mty_desc {
  | Tmty_signature(signature) => forSignature(signature.sig_items)
  | Tmty_alias(path, _) | Tmty_ident(path, _) => (None, [])
  | Tmty_functor(_, _, _, result) => forModuleSig(result) |> mapFst(either(findDocAttribute(mty_attributes)))
  | Tmty_with(inner, _) => forModuleSig(inner) |> mapFst(either(findDocAttribute(mty_attributes)))
  | Tmty_typeof(modd)
    => assert(false)
  }
};



let rec forStructure = structure => {
  open Typedtree;
  let (doc, items) = switch structure {
  | [{str_desc: Tstr_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])))}, ...rest] => (Some(doc), rest)
  | _ => (None, structure)
  };
  (doc, List.map(forItem, items) |> List.concat)
} and forItem = item => Typedtree.(switch (item.str_desc) {
| Tstr_value(_, bindings) => foldOpt(({vb_loc, vb_expr, vb_pat, vb_attributes}) =>
    switch (vb_pat.pat_desc) {
    | Tpat_var(_, {Asttypes.txt}) => Some((txt, findDocAttribute(vb_attributes), Value(vb_pat.pat_type)))
    | _ => None
    }
    , bindings, [])
| Tstr_primitive({val_name: {txt, loc}, val_val, val_attributes, val_loc}) => {
  [(txt, findDocAttribute(val_attributes), Value(val_val.val_type))]
}
| Tstr_include({incl_loc, incl_mod, incl_attributes, incl_type}) => {
  switch incl_mod.mod_desc {
  | Tmod_ident(path, _) => forSignatureType(incl_type)
  | _ => forSignatureType(incl_type)
  }
}
| Tstr_type(decls) => foldOpt(({typ_name: {txt}, typ_loc, typ_attributes, typ_type}) =>
    Some((txt, findDocAttribute(typ_attributes), Type(typ_type)))
  , decls, [])
| Tstr_module({mb_attributes, mb_loc, mb_name: {txt}, mb_expr}) => {
  let (docc, contents) = forModule(mb_expr);
  [(txt, either(docc, findDocAttribute(mb_attributes)), Module(contents))]
}
| _ => []
})
and forModule = ({Typedtree.mod_desc, mod_attributes, mod_loc}) => {
  open Typedtree;
  switch mod_desc {
  | Tmod_structure(structure) => {
    let (docc, contents) = forStructure(structure.str_items);
    (docc, contents)
  }
  | Tmod_constraint(mmod, mtyp, _, _) => {
    /* TODO this should probably be the mtyp. why? */
    forModule(mmod)
    /* moduleContentsSig(mtyp, typesByLoc) */
  }
  | Tmod_ident(path, _) => {
    /* TODO resolve */
    (findDocAttribute(mod_attributes), [])
  }
  | Tmod_functor(_, _, _, result) => forModule(result) |> mapFst(either(findDocAttribute(mod_attributes)))
  | Tmod_apply(inner, _, _) => forModule(inner) |> mapFst(either(findDocAttribute(mod_attributes)))
  | Tmod_unpack(_, typ) => (findDocAttribute(mod_attributes), forModuleType(typ))
  }
}
;

let forCmt = ({cmt_modname, cmt_annots}: Cmt_format.cmt_infos) => switch cmt_annots {
  | Implementation(structure) => Some(forStructure(structure.str_items))
  | Interface(signature) => Some(forSignature(signature.sig_items))
  | _ => None
  };