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
open Infix;

type item =
  | Module(list((string, int)))
  /* | ModuleAlias(Path.t) */
  | Type(Types.type_declaration)
  | Constructor(Types.constructor_declaration, string, Types.type_declaration)
  | Attribute(Types.type_expr, string, Types.type_declaration)
  | Value(Types.type_expr);

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

type moduleData = {
  mutable toplevelDocs: option(string),
  stamps: Hashtbl.t(int, (string, Location.t, item, option(string), ((int, int), (int, int)))),
  /* TODO track constructor names, and record attribute names */
  internalReferences: Hashtbl.t(int, list(Location.t)),
  externalReferences: Hashtbl.t(string, list((list(string), Location.t))),
  exported: Hashtbl.t(string, int),
  mutable topLevel: list((string, int)),
  mutable locations: list((Location.t, Types.type_expr, definition)),
  mutable allOpens: list(anOpen)
};

let maybeFound = (fn, a) =>
  switch (fn(a)) {
  | exception Not_found => None
  | x => Some(x)
  };

let rec docsItem = (item, data) =>
  switch item {
  | Type(t) => Docs.Type(t)
  | Constructor(a, b, c) => Docs.Constructor(a, b, c)
  | Attribute(a, b, c) => Docs.Attribute(a, b, c)
  | Value(t) => Docs.Value(t)
  | Module(items) =>
    Docs.Module(
      items
      |> List.map(
           ((name, stamp)) => {
             let (name, loc, item, docs, _) = Hashtbl.find(data.stamps, stamp);
             (name, loc, docs, docsItem(item, data))
           }
         )
    )
  };

let inRange = ((l, c), ((l0, c0), (l1, c1))) => {
  let l = l + 1;
  (l0 < l || l0 == l && c0 <= c) && (l1 == (-1) && c1 == (-1) || l1 > l || l1 == l && c1 > c)
};

/* TODO this is not perfect, because if the user edits and gets outside of the original scope, then
   we no longer give you the completions you need. This is annoying :/
   Not sure how annoying in practice? One hack would be to forgive going a few lines over... */
let completions = ({stamps}, prefix, pos) => {
  Hashtbl.fold(
    (_, (name, loc, item, docs, range), results) =>
      if (inRange(pos, range)) {
        let results = if (Utils.startsWith(name, prefix)) {
          [(name, loc, item, docs), ...results]
        } else {
          results
        };
        /* switch item {
          | Type({type_kind: Type_variant(constructors)}) => {
            List.fold_left((results, {Types.cd_id: {name, stamp}}) => {
              if (Utils.startsWith(name, prefix)) {
                [(name, loc, item, docs), ...results]
              } else {
                results
              }
            }, results, constructors)
          }
          | Type({type_kind: Type_record(labels, _)}) => {
            List.fold_left((results, {Types.ld_id: {name, stamp}}) => {
              if (Utils.startsWith(name, prefix)) {
                [(name, loc, item, docs), ...results]
              } else {
                results
              }
            }, results, labels)
          }
          | _ => results
        } */
        results
      } else { results },
    stamps,
    []
  )
};

module Opens = {
  let pushHashList = (hash, key, value) =>
    Hashtbl.replace(
      hash,
      key,
      switch (Hashtbl.find(hash, key)) {
      | exception Not_found => [value]
      | items => [value, ...items]
      }
    );
  let mapHash = (hash, fn) => Hashtbl.fold((k, v, l) => [fn(k, v), ...l], hash, []);
  let toString = (fn, (a, tag)) =>
    switch tag {
    | TagType => "type: " ++ fn(a)
    | TagValue => "value: " ++ fn(a)
    | TagConstructor(b) => "constr: " ++ fn(a) ++ " - " ++ b
    | TagAttribute(b) => "attr: " ++ fn(a) ++ " - " ++ b
    | TagModule => "module: " ++ fn(a)
    };
  let showLident = (l) => String.concat(".", Longident.flatten(l));
  let rec addLidentToPath = (path, lident) =>
    Path.(
      Longident.(
        switch lident {
        | Lident(text) => Pdot(path, text, 0)
        | Ldot(lident, text) => Pdot(addLidentToPath(path, lident), text, 0)
        | Lapply(_, _) => failwith("I dont know what these are")
        }
      )
    );
  let showUses = (openPath, uses) => {
    let attrs = Hashtbl.create(50);
    let constrs = Hashtbl.create(50);
    let normals =
      List.filter(
        ((innerPath, tag)) =>
          switch tag {
          | TagConstructor(name) =>
            pushHashList(constrs, innerPath, name);
            false
          | TagAttribute(name) =>
            pushHashList(attrs, innerPath, name);
            false
          | _ => true
          },
        uses
      );
    let normals =
      normals
      |> List.filter(
           ((innerPath, tag)) =>
             switch tag {
             | TagType => ! (Hashtbl.mem(attrs, innerPath) || Hashtbl.mem(constrs, innerPath))
             | _ => true
             }
         );
    List.concat([
      normals
      |> List.map(
           ((lident, tag)) => {
             let fullPath = addLidentToPath(openPath, lident);
             showLident(lident)
           }
         ),
      mapHash(
        attrs,
        (path, attrs) => {
          let fullPath = addLidentToPath(openPath, path);
          showLident(path) ++ " {" ++ String.concat(", ", attrs |> List.map((attr) => attr)) ++ "}"
        }
      ),
      mapHash(
        constrs,
        (path, attrs) => {
          let fullPath = addLidentToPath(openPath, path);
          showLident(path)
          ++ " ("
          ++ String.concat(" | ", attrs |> List.map((attr) => attr))
          ++ ")"
        }
      )
    ])
    |> String.concat(", ")
  };
};

let opens = ({allOpens}) =>
  allOpens
  |> Utils.filterMap(
       ({path, loc, used, useCount}) =>
         if (! loc.Location.loc_ghost) {
           let i = loc.Location.loc_end.pos_cnum;
           let isPervasives =
             switch path {
             | Path.Pident({name: "Pervasives"}) => true
             | _ => false
             };
           let used = List.sort_uniq(compare, List.map(((ident, tag, _)) => (ident, tag), used));
           Some((
             "exposing ("
             ++ Opens.showUses(path, used)
             ++ ") "
             ++ string_of_int(useCount)
             ++ " uses",
             loc
           ))
         } else {
           None
         }
     );

let dependencyList = ({externalReferences}) =>
  Hashtbl.fold((k, _, items) => [k, ...items], externalReferences, []);

let listExported = (data) =>
  Hashtbl.fold(
    (name, stamp, results) =>
      switch (Hashtbl.find(data.stamps, stamp)) {
      | exception Not_found => results
      | item => [item, ...results]
      },
    data.exported,
    []
  );

let listTopLevel = (data) =>
  data.topLevel |> List.map(((name, stamp)) => Hashtbl.find(data.stamps, stamp));

let handleConstructor = (path, txt) => {
  let typeName =
    switch path {
    | Path.Pdot(path, typename, _) => typename
    | Pident({Ident.name}) => name
    | _ => assert false
    };
  Longident.(
    switch txt {
    | Longident.Lident(name) => (name, Lident(typeName))
    | Ldot(left, name) => (name, Ldot(left, typeName))
    | Lapply(_) => assert false
    }
  )
};

let handleRecord = (path, txt) => {
  let typeName =
    switch path {
    | Path.Pdot(path, typename, _) => typename
    | Pident({Ident.name}) => name
    | _ => assert false
    };
  Longident.(
    switch txt {
    | Lident(name) => Lident(typeName)
    | Ldot(inner, name) => Ldot(inner, typeName)
    | Lapply(_) => assert false
    }
  )
};

let getSuffix = (declaration, suffix) =>
  switch declaration.Types.type_kind {
  | Type_record(attributes, _) =>
    Utils.find(
      ({Types.ld_id: {name, stamp}, ld_loc}) =>
        if (name == suffix) {
          Some(ld_loc)
        } else {
          None
        },
      attributes
    )
  | Type_variant(constructors) =>
    Utils.find(
      ({Types.cd_id: {name, stamp}, cd_loc}) =>
        if (name == suffix) {
          Some(cd_loc)
        } else {
          None
        },
      constructors
    )
  | _ => None
  };

let resolveNamedPath = (data, path, suffix) =>
  switch path {
  | [] => None
  | [one, ...rest] =>
    switch (Hashtbl.find(data.exported, one)) {
    | exception Not_found => None
    | stamp =>
      let rec loop = (stamp, path) =>
        switch (Hashtbl.find(data.stamps, stamp)) {
        | exception Not_found => None
        | (name, loc, item, docs, scope) =>
          switch (path, item) {
          | ([], _) => switch (item, suffix) {
            | (_, None) => Some((name, loc, item, docs))
            | (Type(t), Some(suffix)) => {
              /* TODO this isn't the right `item` --- it'sstill the type  */
              getSuffix(t, suffix) |?>> loc => (name, loc, item, docs)
            }
            | _ => None
          }
          | ([first, ...rest], Module(contents)) =>
            switch (List.assoc(first, contents)) {
            | exception Not_found => None
            | stamp => loop(stamp, rest)
            }
          | _ => None
          }
        };
      loop(stamp, rest)
    }
  };

let checkPos = ((line, char), {Location.loc_start: {pos_lnum, pos_bol, pos_cnum}, loc_end}) =>
  Lexing.(
    if (line < pos_lnum || line == pos_lnum && char < pos_cnum - pos_bol) {
      false
    } else if (line > loc_end.pos_lnum
               || line == loc_end.pos_lnum
               && char > loc_end.pos_cnum
               - loc_end.pos_bol) {
      false
    } else {
      true
    }
  );

let locationAtPos = ((line, char), data) => {
  let pos = (line + 1, char);
  let rec loop = (locations) =>
    switch locations {
    | [] => None
    | [(loc, expr, defn), ..._] when checkPos(pos, loc) => Some((loc, expr, defn))
    | [_, ...rest] => loop(rest)
    };
  loop(data.locations)
};

let openReferencesAtPos = ({allOpens} as data, pos) => {
  locationAtPos(pos, data) |?> ((loc, expr, defn)) => switch defn {
    | Open(path) => {
      let rec loop = opens => switch opens {
        | [] => None
        | [one, ..._] when one.loc == loc => Some(one)
        | [_, ...rest] => loop(rest)
      };
      loop(allOpens) |?>> openn => openn.used
    }
    | _ => None
  }
};

let isStampExported = (needle, data) =>
  Hashtbl.fold(
    (name, stamp, found) => found != None ? found : stamp == needle ? Some(name) : None,
    data.exported,
    None
  );

let highlightsForStamp = (stamp, data) =>
  maybeFound(Hashtbl.find(data.stamps), stamp)
  |?> (
    ((_, defnLoc, _, _, _)) => {
      let usages = maybeFound(Hashtbl.find(data.internalReferences), stamp) |? [];
      Some([(`Write, defnLoc), ...List.map((l) => (`Read, l), usages)])
    }
  );

let rec stampAtPath = (path, data) =>
  switch path {
  | Path.Pident({stamp: 0, name}) => Some(`Global((name, [])))
  | Path.Pident({stamp, name}) => Some(`Local(stamp))
  | Path.Pdot(inner, name, _) =>
    switch (stampAtPath(inner, data)) {
    | Some(`Global(top, subs)) => Some(`Global((top, subs @ [name])))
    | Some(`Local(stamp)) =>
      maybeFound(Hashtbl.find(data.stamps), stamp)
      |?> (
        (x) =>
          switch x {
          | (_, _, Module(contents), _, _) =>
            maybeFound(List.assoc(name), contents) |?>> ((stamp) => `Local(stamp))
          | _ => None
          }
      )
    | _ => None
    }
  | _ => None
  };

let stampAtPos = (pos, data) =>
  locationAtPos(pos, data)
  |?> (
    ((loc, expr, defn)) =>
      switch defn {
      | IsDefinition(stamp) => Some(stamp)
      | Path(path) =>
        switch (stampAtPath(path, data)) {
        | Some(`Global(name, children)) => None /* TODO resolve cross-file */
        | Some(`Local(stamp)) => Some(stamp)
        | None => None
        }
      | _ => None
      }
  );

let highlights = (pos, data) => stampAtPos(pos, data) |?> ((x) => highlightsForStamp(x, data));

let resolvePath = (path, data, suffix) =>
  switch (stampAtPath(path, data)) {
  | None => None
  | Some(`Global(name, children)) => Some(`Global((name, children, suffix)))
  | Some(`Local(stamp)) => maybeFound(Hashtbl.find(data.stamps), stamp) |?>> ((x) => `Local(x, suffix))
  };

let findDefinition = (defn, data) => {
  /* Log.log("😍 resolving a definition"); */
  switch defn {
  | IsConstant => None
  | IsDefinition(stamp) =>
    Log.log("Is a definition");
    None
  | ConstructorDefn(path, name, _) => resolvePath(path, data, Some(name))
  | AttributeDefn(path, name, _) => resolvePath(path, data, Some(name))
  | Open(path)
  | Path(path) => resolvePath(path, data, None)
  };
};

module Get = {
  /* TODO maybe return loc from this? or have a separate one that
   * finds a thing by name...
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
      | Path(path) =>
        switch (stampAtPath(path, Collector.data)) {
        | None => ()
        | Some(`Global(modname, children)) =>
          let current = maybeFound(Hashtbl.find(Collector.data.externalReferences), modname) |? [];
          Hashtbl.replace(
            Collector.data.externalReferences,
            modname,
            [(children, loc), ...current]
          )
        | Some(`Local(stamp)) =>
          let current = maybeFound(Hashtbl.find(Collector.data.internalReferences), stamp) |? [];
          Hashtbl.replace(Collector.data.internalReferences, stamp, [loc, ...current])
        }
      /* | Path(Path.Pident({stamp, name})) when stamp != 0 => {
           let current = maybeFound(Hashtbl.find(Collector.data.internalReferences), stamp) |? [];
           Hashtbl.replace(Collector.data.internalReferences, stamp, [loc, ...current])
         } */
      | _ => ()
      };
      Collector.data.locations = [(loc, typ, definition), ...Collector.data.locations]
    };
    let enter_signature_item = (item) =>
      switch item.sig_desc {
        | Tsig_attribute(({Asttypes.txt: "ocaml.doc" | "ocaml.text"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(doc, _))}, _)}]))) => {
          if (Collector.data.toplevelDocs == None) {
            Collector.data.toplevelDocs = Some(doc)
          } else {
            ()
          }
        }
      | Tsig_value({val_id: {stamp, name}, val_val: {val_type}, val_loc}) =>
        addStamp(stamp, name, val_loc, Value(val_type), None)
      | Tsig_type(decls) =>
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
        )
      /* TODO add support for these */
      /* | Tsig_include({incl_mod, incl_type}) => stampsFromTypesSignature(currentPath, incl_type) */
      /* | Tsig_module({md_id: {stamp, name}, md_type: {mty_desc: Tmty_signature(signature)}}) => {
           addStamp
           let (stamps) = stampsFromTypedtreeInterface(addToPath(currentPath, name), signature.sig_items);
           [(stamp, addToPath(currentPath, name) |> toFullPath(PModule)), ...stamps]
         } */
      | Tsig_module({md_id: {stamp, name}, md_loc}) =>
        addStamp(stamp, name, md_loc, Module([]), None)
      | _ => ()
      };
    let enter_structure_item = (item) =>
      Typedtree.(
        switch item.str_desc {
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
                      labels |> List.iter(({Types.ld_id: {stamp, name: lname}, ld_type, ld_loc}) => {
                        addStamp(stamp, lname, ld_loc, Attribute(ld_type, name, typ_type), docs)
                      })
                    }
                    | Types.Type_variant(constructors) => {
                      constructors |> List.iter(({Types.cd_id: {stamp, name: cname}, cd_loc} as cd) => {
                        addStamp(stamp, cname, cd_loc, Constructor(cd, name, typ_type), docs)
                      })

                    }
                    | _ => ()
                  }
               }
             )
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
          addOpenScope();
          addStamp(stamp, name, loc, Module(stampNames(structure.str_items)), docs)
        | Tstr_module({mb_attributes, mb_id: {stamp, name}, mb_name: {loc}}) =>
          let docs = PrepareUtils.findDocAttribute(mb_attributes);
          addStamp(stamp, name, loc, Module([]), docs)
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
    let rec dig = (typ) =>
      switch typ.Types.desc {
      | Types.Tlink(inner) => dig(inner)
      | Types.Tsubst(inner) => dig(inner)
      | _ => typ
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
    let enter_expression = (expr) =>
      switch expr.exp_desc {
      | Texp_for({stamp, name}, {ppat_loc}, {exp_type}, _, _, contents) =>
        addLocation(ppat_loc, exp_type, IsDefinition(stamp));
        addScope(rangeOfLoc(contents.exp_loc));
        addStamp(stamp, name, ppat_loc, Value(exp_type), None);
        popScope()
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
      /* Skip array literals */
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
      exported: Hashtbl.create(10),
      allOpens: [],
      topLevel: [],
      locations: []
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
    let structure = (items) => {
      let names = stampNames(items);
      names |> List.iter(((name, stamp)) => Hashtbl.replace(data.exported, name, stamp));
      data.topLevel = names;
      List.iter(IterIter.iter_structure_item, items)
    };
    let iter_part = (part) =>
      switch part {
      | Cmt_format.Partial_structure(str) =>
        IterIter.iter_structure(str);
        stampNames(str.str_items)
      | Partial_structure_item(str) =>
        IterIter.iter_structure_item(str);
        stampNames([str])
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
      let names = Array.map(iter_part, parts) |> Array.to_list |> List.concat;
      names |> List.iter(((name, stamp)) => Hashtbl.replace(data.exported, name, stamp));
      data.topLevel = names
    | _ => failwith("Not a valid cmt file")
    };
    data.locations = List.rev(data.locations);
    /* allOpens^ |> List.iter(({used, path, loc}) => {
         Log.log("An Open! " ++ string_of_int(List.length(used)));
       }); */
    data.allOpens = allOpens^;
    data
  };
};

let process = Get.process;
/* let resolveDefinition = (defn, data) => switch (findDefinition(defn, data)) {
   | None => None
   | Some(`Global(top, children)) => {
   }
   | Some(`Local(defn)) => Some(defn)
   }; */