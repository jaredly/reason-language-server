
open Infix;

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

module T = {

type item = {
  name: string,
  kind,
  stamp: int,
  loc: Location.t,
  docstring: option(string),
}

and kind =
  | Module(list(item))
  | ModuleAlias(Path.t)
  | Function(list(Types.type_expr), Types.type_expr)
  | Value(Types.type_expr)
  | Type(Types.type_declaration)
  | Constructor(Types.constructor_declaration, string, Types.type_declaration)
  | Attribute(Types.type_expr, string, Types.type_declaration);

type moduleDocs = {
  docstring: option(string),
  stamps: Hashtbl.t(int, item),
  mutable topLevel: list(item),
};
};

open T;

let moduleDocs = (docstring, items) => {
  {docstring, topLevel: items, stamps: Hashtbl.create(1)}
};

module Definitions = {
  type definition =
    | Path(Path.t)
    | Open(Path.t)
    /* | Location(Location.t) */
    | ConstructorDefn(Path.t, string, Location.t)
    | AttributeDefn(Path.t, string, Location.t)
    | IsConstant
    | IsDefinition(int);

  type tag =
    | TagType
    | TagValue
    | TagModule
    | TagConstructor(string)
    | TagAttribute(string);

  type anOpen = {
    path: Path.t,
    loc: Location.t,
    mutable used: list((Longident.t, tag, Location.t)),
    mutable useCount: int
  };

  type moduleDefinitions = {
    docs: moduleDocs,

    internalReferences: Hashtbl.t(int, list(Location.t)),
    externalReferences: Hashtbl.t(string, list((list(string), Location.t, option(string)))),

    exported: Hashtbl.t(string, int),
    mutable exportedSuffixes: list((int, string, string)),

    mutable locations: list((Location.t, Types.type_expr, definition)),

    mutable explanations: list((Location.t, string)),
    mutable allOpens: list(anOpen)
  };
};

/* and full = (string, Location.t, option(string), item); */
/* TODO module types n that jazz also functors I guess */

let show = item => switch item {
  | Module(m) => "Module"
  | ModuleAlias(_) => "ModuleAlias"
  | Function(_, _) => "Function"
  | Value(_) => "Value"
  | Type(_) => "Type"
  | Constructor(_) => "Constructor"
  | Attribute(_)  => "Attribute"
};

let rec find = (needle, docs) => switch docs {
| [] => None
| [{name} as doc, ..._] when needle == name => Some(doc)
| [_, ...rest] => find(needle, rest)
};

/* let rec findPath = (children, docs) => switch (children) {
| [] => None
| [single] => find(single, docs)
| [first, ...rest] => switch (find(first, docs)) {
| Some((_, _, _, Module(contents))) => findPath(rest, contents)
| Some((_, _, _, ModuleAlias(path))) => {
  Log.log("Finding a path in docs and it's through an alias");
  None
}
| _ => None
}
}; */

let rec resolveDocsPath = (~resolveAlias, uri, parts, contents) =>
  switch (parts) {
  | [] => None
  | [single] => Some((uri, contents, single))
  | [first, ...rest] =>
    contents
    |> Utils.find(({name, loc, docstring, kind}) =>
         switch (kind) {
         | Module(contents) when name == first => Some(resolveDocsPath(~resolveAlias, uri, rest, contents))
         | ModuleAlias(path) when name == first =>
            switch (resolveAlias(path, rest)) {
              | None => {
                Log.log("Unable to resolve module alias!!! " ++ name);
                Some(None)
              }
              | Some((uri, contents, items)) => Some(resolveDocsPath(~resolveAlias, uri, items, contents))
            }
         | _ => None
         }
       ) |? None
  };

let rec forSignatureType = (processDoc, signature) => {
  open Types;
  List.fold_left((items, item) => switch item {
  | Sig_value({stamp, name}, {val_type, val_kind, val_attributes, val_loc: loc}) => [{stamp, name, loc, docstring: findDocAttribute(val_attributes) |?>> processDoc, kind: Value(val_type)}, ...items]
  | Sig_type({stamp, name}, decl, _) => [{stamp, name, loc: decl.type_loc, docstring: findDocAttribute(decl.type_attributes) |?>> processDoc, kind: Type(decl)}, ...items]
  | Sig_module({stamp, name}, {md_type: Mty_ident(path) | Mty_alias(path), md_attributes, md_loc}, _) =>
    [{stamp, name, loc: md_loc, docstring: findDocAttribute(md_attributes) |?>> processDoc, kind: ModuleAlias(path)}, ...items]
  | Sig_module({stamp, name}, {md_type, md_attributes, md_loc}, _) =>
    [{stamp, name, loc: md_loc, docstring: findDocAttribute(md_attributes) |?>> processDoc, kind: forModuleType(processDoc, md_type)}, ...items]
  | _ => items
  }, [], signature);
} and forModuleType = (processDoc, modtype) => Types.(switch modtype {
  | Mty_ident(path) | Mty_alias(path) => ModuleAlias(path)
  | Mty_functor(_, _, tt) => forModuleType(processDoc, tt)
  | Mty_signature(sign) => Module(forSignatureType(processDoc, sign))
});



/* TODO need a way to resolve module path aliases */

let rec forSignature = (processDoc, signature) => {
  open Typedtree;
  let (doc, items) = switch signature {
  | [{sig_desc: Tsig_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])))}, ...rest] => (Some(doc), rest)
  | _ => (None, signature)
  };
  (doc, List.map(forSignatureItem(processDoc), items) |> List.concat)
} and forSignatureItem = (processDoc, item) => Typedtree.(switch (item.sig_desc) {
  | Tsig_value({val_id: {stamp}, val_name: {txt, loc}, val_val, val_attributes, val_loc}) =>
      [{stamp, name: txt, loc: val_loc, docstring: findDocAttribute(val_attributes) |?>> processDoc, kind: Value(val_val.val_type)}]
  | Tsig_include({incl_loc, incl_mod, incl_attributes, incl_type}) => {
      switch incl_mod.mty_desc {
      | Tmty_ident(path, _) | Tmty_alias(path, _) => forSignatureType(processDoc, incl_type)
      | _ => forSignatureType(processDoc, incl_type)
      }
  }
  | Tsig_type(decls) => List.map(({typ_id: {stamp}, typ_name: {txt, loc}, typ_loc, typ_attributes, typ_type}) =>
      {stamp, name: txt, loc: typ_loc, docstring: findDocAttribute(typ_attributes) |?>> processDoc, kind: Type(typ_type)}
    , decls)
  | Tsig_module({md_id: {stamp}, md_attributes, md_loc, md_name: {txt, loc}, md_type: {mty_desc: Tmty_alias(path, _) | Tmty_ident(path, _)}}) => {
      [{stamp, name: txt, loc: md_loc, docstring: findDocAttribute(md_attributes) |?>> processDoc, kind: ModuleAlias(path)}]
    }
  | Tsig_module({md_id: {stamp}, md_attributes, md_loc, md_name: {txt, loc}, md_type: module_type}) => {
      let (docc, contents) = forModuleSig(processDoc, module_type);
      [{stamp, name: txt, loc: md_loc, docstring: either(docc, findDocAttribute(md_attributes) |?>> processDoc), kind: Module(contents)}]
    }
  | _ => []
  })

and forModuleSig = (processDoc, {Typedtree.mty_desc, mty_attributes, mty_loc}) => {
  open Typedtree;
  switch mty_desc {
  | Tmty_signature(signature) => forSignature(processDoc, signature.sig_items)
  | Tmty_alias(path, _) | Tmty_ident(path, _) => (None, [{stamp: 0, name: "_alias_", loc: Location.none, docstring: None, kind: Module([])}])
  | Tmty_functor(_, _, _, result) => forModuleSig(processDoc, result) |> mapFst(either(findDocAttribute(mty_attributes) |?>> processDoc))
  | Tmty_with(inner, _) => forModuleSig(processDoc, inner) |> mapFst(either(findDocAttribute(mty_attributes) |?>> processDoc))
  | Tmty_typeof(modd)
    => assert(false)
  }
};



let rec forStructure = (processDoc, structure) => {
  open Typedtree;
  let (doc, items) = switch structure {
  | [{str_desc: Tstr_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}])))}, ...rest] => (Some(doc), rest)
  | _ => (None, structure)
  };
  (doc, List.map(forItem(processDoc), items) |> List.concat)
} and forItem = (processDoc, item) => Typedtree.(switch (item.str_desc) {
| Tstr_value(_, bindings) => foldOpt(({vb_loc, vb_expr, vb_pat, vb_attributes}) =>
    switch (vb_pat.pat_desc) {
    | Tpat_var({Ident.stamp}, {Asttypes.txt, loc}) => Some({stamp, name: txt, loc, docstring: findDocAttribute(vb_attributes) |?>> processDoc, kind: Value(vb_pat.pat_type)})
    | _ => None
    }
    , bindings, [])
| Tstr_primitive({val_id: {stamp}, val_name: {txt, loc}, val_val, val_attributes, val_loc}) => {
  [{stamp, name: txt, loc, docstring: findDocAttribute(val_attributes) |?>> processDoc, kind: Value(val_val.val_type)}]
}
| Tstr_include({incl_loc, incl_mod, incl_attributes, incl_type}) => {
  switch incl_mod.mod_desc {
  | Tmod_ident(path, _) => forSignatureType(processDoc, incl_type)
  | _ => forSignatureType(processDoc, incl_type)
  }
}
| Tstr_type(decls) => foldOpt(({typ_id: {stamp}, typ_name: {txt, loc}, typ_loc, typ_attributes, typ_type}) =>
    Some({stamp, name: txt, loc, docstring: findDocAttribute(typ_attributes) |?>> processDoc, kind: Type(typ_type)})
  , decls, [])
| Tstr_module({mb_id: {stamp}, mb_attributes, mb_loc, mb_name: {txt, loc}, mb_expr: {mod_desc: Tmod_ident(path, _)}}) => {
  [{stamp, name: txt, loc, docstring: findDocAttribute(mb_attributes) |?>> processDoc, kind: ModuleAlias(path)}]
}
| Tstr_module({mb_id: {stamp}, mb_attributes, mb_loc, mb_name: {txt, loc}, mb_expr}) => {
  let (docc, contents) = forModule(processDoc, mb_expr);
  [{stamp, name: txt, loc, docstring: either(docc, findDocAttribute(mb_attributes) |?>> processDoc), kind: contents}]
}
| _ => []
})
and forModule = (processDoc, {Typedtree.mod_desc, mod_attributes, mod_loc}) => {
  open Typedtree;
  switch mod_desc {
  | Tmod_structure(structure) => {
    let (docc, contents) = forStructure(processDoc, structure.str_items);
    (docc, Module(contents))
  }
  | Tmod_constraint(mmod, mtyp, _, _) => {
    /* TODO this should probably be the mtyp. why? */
    forModule(processDoc, mmod)
    /* moduleContentsSig(mtyp, typesByLoc) */
  }
  | Tmod_ident(path, _) => {
    /* TODO resolve */
    (findDocAttribute(mod_attributes) |?>> processDoc, ModuleAlias(path))
  }
  | Tmod_functor(_, _, _, result) => forModule(processDoc, result) |> mapFst(either(findDocAttribute(mod_attributes) |?>> processDoc))
  | Tmod_apply(inner, _, _) => forModule(processDoc, inner) |> mapFst(either(findDocAttribute(mod_attributes) |?>> processDoc))
  | Tmod_unpack(_, typ) => (findDocAttribute(mod_attributes) |?>> processDoc, forModuleType(processDoc, typ))
  }
}
;

let forCmt = (processDoc, {cmt_modname, cmt_annots}: Cmt_format.cmt_infos) => switch cmt_annots {
  | Implementation(structure) => Some(forStructure(processDoc, structure.str_items))
  | Interface(signature) => Some(forSignature(processDoc, signature.sig_items))
  | _ => None
  };

let forCmi = (processDoc, {Cmi_format.cmi_name, cmi_sign}) => {
  let items = forSignatureType(processDoc, cmi_sign);
  /* TODO: want the toplevel docs... */
  Some((None, items))
};