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

/* type item =
  | Module(list((string, int)))
  | ModuleWithDocs(list(Docs.item))
  /* | ModuleAlias(Path.t) */
  | Type(Types.type_declaration)
  | Constructor(Types.constructor_declaration, string, Types.type_declaration)
  | Attribute(Types.type_expr, string, Types.type_declaration)
  | Value(Types.type_expr); */

open Docs.T;

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
  stamps: Hashtbl.t(int, (string, Location.t, kind, option(string), ((int, int), (int, int)))),
  internalReferences: Hashtbl.t(int, list(Location.t)),
  externalReferences: Hashtbl.t(string, list((list(string), Location.t, option(string)))),
  exported: Hashtbl.t(string, int),
  mutable exportedSuffixes: list((int, string, string)),
  mutable topLevel: list(item),
  mutable locations: list((Location.t, Types.type_expr, definition)),
  mutable explanations: list((Location.t, string)),
  mutable allOpens: list(anOpen),
  file: SharedTypes.file,
  extra: SharedTypes.extra,
};

let maybeFound = (fn, a) =>
  switch (fn(a)) {
  | exception Not_found => None
  | x => Some(x)
  };

/* let rec docsItem = (item, data) =>
  switch item {
  | Type(t) => Docs.Type(t)
  | ModuleWithDocs(docs) => Docs.Module(docs)
  | Constructor(a, b, c) => Docs.Constructor(a, b, c)
  | Attribute(a, b, c) => Docs.Attribute(a, b, c)
  | Value(t) => Docs.Value(t)
  | Module(items) =>
    Docs.Module(
      items
      |> List.map(
           ((name, stamp)) => {
             let (name, loc, item, docstring, _) = Hashtbl.find(data.stamps, stamp);
             {Docs.name, stamp, loc, docstring, kind: docsItem(item, data)}
           }
         )
    )
  }; */

let inRange = ((l, c), ((l0, c0), (l1, c1))) => {
  let l = l + 1;
  (l0 < l || l0 == l && c0 <= c) && (l1 == (-1) && c1 == (-1) || l1 > l || l1 == l && c1 > c)
};

let rec dig = (typ) =>
  switch typ.Types.desc {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | _ => typ
  };

let getSuffix = (declaration, suffix) =>
  switch declaration.Types.type_kind {
  | Type_record(attributes, _) =>
    Utils.find(
      ({Types.ld_id: {name, stamp}, ld_loc}) =>
        if (name == suffix) {
          Some((ld_loc, stamp))
        } else {
          None
        },
      attributes
    )
  | Type_variant(constructors) =>
    Utils.find(
      ({Types.cd_id: {name, stamp}, cd_loc}) =>
        if (name == suffix) {
          Some((cd_loc, stamp))
        } else {
          None
        },
      constructors
    )
  | _ => None
  };

let suffixForStamp = (stamp, suffix, data) => {
  let%opt (name, loc, item, docs, range) = maybeFound(Hashtbl.find(data.stamps), stamp);
  switch item {
    | Type(t, extra) => getSuffix(t, suffix) |?>> ((loc, stamp)) => stamp
    | _ => None
  }
};

let rec stampAtPath = (path, data, suffix) =>
  switch path {
  | Path.Pident({stamp: 0, name}) => Some(`Global((name, [], suffix)))
  | Path.Pident({stamp, name}) => {
    /* Log.log("Local path here " ++ string_of_int(stamp) ++ " named " ++ name); */
    fold(suffix, Some(`Local(stamp)), suffix => suffixForStamp(stamp, suffix, data) |?>> stamp => `Local(stamp))
  }
  | Path.Pdot(inner, name, _) =>
    switch (stampAtPath(inner, data, None)) {
    | Some(`Global(top, subs, _)) => Some(`Global((top, subs @ [name], suffix)))
    | Some(`Local(stamp)) =>
      let%opt x = maybeFound(Hashtbl.find(data.stamps), stamp);
      switch x {
      | (_, _, Module(contents), _, _) =>
        Utils.find(item => item.name == name ? Some(`Local(item.stamp)) : None, contents)
        /* maybeFound(List.assoc(name), contents) |?>> ((stamp) => `Local(stamp)) */
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };

/* TODO this is not perfect, because if the user edits and gets outside of the original scope, then
   we no longer give you the completions you need. This is annoying :/
   Not sure how annoying in practice? One hack would be to forgive going a few lines over... */
let completions = ({stamps}, prefix, pos) => {
  Hashtbl.fold(
    (_, (name, loc, item, docs, range), results) =>
      if (inRange(pos, range)) {
        if (Utils.startsWith(name, prefix)) {
          [(name, loc, item, docs), ...results]
        } else {
          results
        };
      } else { results },
    stamps,
    []
  )
};

let resolvePath = (path, data, suffix) =>
  switch (stampAtPath(path, data, suffix)) {
  | None => None
  | Some(`Global(name, children, suffix)) => Some(`Global((name, children, suffix)))
  | Some(`Local(stamp)) => maybeFound(Hashtbl.find(data.stamps), stamp) |?>> i => `Local(i)
  };

let findDefinition = (defn, data, resolve) => {
  Log.log("😍 resolving a definition");
  switch defn {
  | IsConstant => None
  | IsDefinition(stamp) =>
    Log.log("Is a definition");
    None
  | ConstructorDefn(path, name, _) => resolvePath(path, data, Some(name)) |?> resolve
  | AttributeDefn(path, name, _) => resolvePath(path, data, Some(name)) |?> resolve
  | Open(path)
  | Path(path) => resolvePath(path, data, None) |?> resolve
  };
};

let completionPath = (inDocs, {stamps} as moduleData, first, children, pos, toItem, ~uri, ~resolveDefinition) => {
  let%opt_wrap (name, loc, item, docs) = Hashtbl.fold(
    (_, (name, loc, item, docs, range), result) => {

      /* Log.log(name); */
      switch result {
      | Some(x) => Some(x)
      | None => {
        if (inRange(pos, range) && name == first) {
          Some((name, loc, item, docs))
        } else {
          None
        }
      }
    }
    },
    stamps,
    None
  );

  switch item {
    /* | ModuleWithDocs(docs) => inDocs(children, docs) */
    | Module(contents) => {
      let rec loop = (contents, items) => {
        switch (items) {
          | [] => assert(false)
          | [single] => {
            contents
            |> List.filter(({name, stamp}) => Utils.startsWith(name, single))
            |> List.map(({name, stamp}) => {
              toItem(Hashtbl.find(stamps, stamp))
            })
          }
          | [first, ...more] => {
            switch (List.find(({name, stamp}) => name == first, contents)) {
              | {name, stamp} => {
                let (name, loc, item, docs, range) = Hashtbl.find(stamps, stamp);
                switch item {
                  | Module(contents) => loop(contents, more)
                  /* | ModuleWithDocs(docs) => inDocs(more, docs) */
                  | _ => []
                }
              }
              | exception Not_found => []
            }
          }
        }
      };
      loop(contents, children)
    }
    | Value(t) => {
      let rec loop = (t, children, uri) => {
        Log.log("attribute search");
        switch (dig(t).Types.desc) {
          | Types.Tconstr(path, args, _abbrev) => {
            let%opt stamp = stampAtPath(path, moduleData, None);
            /* TODO need to propagate moduleData from resolveDefinition.
             * Buuuut only localModules have moduleData.
             * And to track things I only need Docs.items, not Definition.moduleData
             */
            let%opt (item, loc, docstring, uri) = findDefinition(Path(path), moduleData, resolveDefinition(uri));
            uri |?< uri => Log.log("now in uri " ++ uri);
            switch item {
              | Type({type_kind: Type_record(labels, _)} as declaration, extra) => {
                switch children {
                  | [] => None
                  | [single] => 
                  labels |> List.filter(({Types.ld_id: {name}}) => Utils.startsWith(name, single))
                  |> List.map(({Types.ld_id: {name}, ld_type, ld_loc}) => toItem((name, ld_loc, Attribute(
                    ld_type, 
                    name,
                    declaration
                  ), None, ((0,0),(0,0))))) |. Some
                  | [first, ...rest] => {
                    Log.log("Drillling in " ++ first);
                    labels |> Utils.find(({Types.ld_id: {name}, ld_type, ld_loc}) => {
                      if (name == first) {
                        Log.log("Found " ++ name);
                        let s = PrintType.default.expr(PrintType.default, ld_type) |> PrintType.prettyString;
                        Log.log("Of type " ++ s);
                        let%opt uri = uri;
                        loop(ld_type, rest, uri)
                      } else {
                        None
                      }
                    })
                  }
                }
              }
              | Value(expr) => {
                let s = PrintType.default.expr(PrintType.default, expr) |> PrintType.prettyString;
                Log.log("Foudn a calue: " ++ s);
                {
                  let%opt_consume loc = loc;
                  let (fname, line, col) = Location.get_pos_info(loc.Location.loc_start);
                  Log.log(Printf.sprintf("File %s: (%d, %d)", fname, line, col));
                };
                None

              }
              /* This really should never happen */
              | _ => {
                Log.log("We found the definition, and it wasn't a type: " ++ Docs.show(item));
                None
              }
            }
          }
          | _ => None
        };
      };
      loop(t, children, uri) |? []
    }
    | _ => []
  }
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

/* let listTopLevel = (data) =>
  data.topLevel |> List.map(((name, stamp)) => Hashtbl.find(data.stamps, stamp)); */

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
            | (Type(t, extra), Some(suffix)) => {
              /* TODO this isn't the right `item` --- it'sstill the type  */
              let%opt_wrap (loc, _) = getSuffix(t, suffix);
              (name, loc, item, docs)
            }
            | _ => None
          }
          | ([first, ...rest], Module(contents)) =>
            contents |> Utils.find(item => item.name == first ? Some(item) : None)
            |?> ({stamp}) => loop(stamp, rest)
            /* switch (List.assoc(first, contents)) {
            | exception Not_found => None
            | stamp => loop(stamp, rest)
            } */
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

let explanationAtPos = ((line, char), data) => {
  let pos = (line + 1, char);
  let rec loop = (locations) =>
    switch locations {
    | [] => None
    | [(loc, explanation), ..._] when checkPos(pos, loc) => Some((loc, explanation))
    | [_, ...rest] => loop(rest)
    };
  loop(data.explanations)
};

let locationAtPos = ((line, char), data) => {
  let pos = (line + 1, char);
  let rec loop = (locations) =>
    switch locations {
    | [] => None
    | [(loc, expr, defn), ..._] when checkPos(pos, loc) => Some((loc, expr, defn))
    | [(loc, _, _), ...rest] => {
      /* Log.log(Printf.sprintf("Loc at %d", loc.Location.loc_start.pos_cnum)); */
      loop(rest)
    }
    };
  loop(data.locations)
};

let openReferencesAtPos = ({allOpens} as data, pos) => {
  let%opt (loc, _expr, defn) = locationAtPos(pos, data);
  switch defn {
  | Open(path) => {
    let rec loop = opens => switch opens {
      | [] => None
      | [one, ..._] when one.loc == loc => Some(one)
      | [_, ...rest] => loop(rest)
    };
    let%opt openn = loop(allOpens);
    Some(openn.used)
  }
  | _ => None
  }
};

let isStampExported = (needle, data) =>
  switch (Hashtbl.fold(
    (name, stamp, found) => found != None ? found : stamp == needle ? Some((name, None)) : None,
    data.exported,
    None
  )) {
    | Some(m) => Some(m)
    | None => data.exportedSuffixes |> Utils.find(((suffixStamp, mainName, suffixName)) => suffixStamp == needle ? Some((mainName, Some(suffixName))) : None)
  };

let highlightsForStamp = (stamp, data) =>{
  let%opt (name, defnLoc, _,_, _) = maybeFound(Hashtbl.find(data.stamps), stamp);
  let usages = maybeFound(Hashtbl.find(data.internalReferences), stamp) |? [];
  Some([(`Write, defnLoc), ...List.map((l) => (`Read, Utils.endOfLocation(l, String.length(name))), usages)])
};

let stampAtPos = (pos, data) => {
  let%opt (loc, expr, defn) = locationAtPos(pos, data);
  switch defn {
  | IsDefinition(stamp) => Some(stamp)
  | AttributeDefn(path, name, _) =>
    switch (stampAtPath(path, data, Some(name))) {
    | Some(`Global(name, children, _)) => None /* TODO resolve cross-file */
    | Some(`Local(stamp)) => Some(stamp)
    | None => None
    }
  | ConstructorDefn(path, name, _) =>
    switch (stampAtPath(path, data, Some(name))) {
    | Some(`Global(name, children, _)) => None /* TODO resolve cross-file */
    | Some(`Local(stamp)) => Some(stamp)
    | None => None
    }
  | Path(path) =>
    switch (stampAtPath(path, data, None)) {
    | Some(`Global(name, children, _)) => None /* TODO resolve cross-file */
    | Some(`Local(stamp)) => Some(stamp)
    | None => None
    }
  | _ => None
  }
};

let highlights = (pos, data) => stampAtPos(pos, data) |?> ((x) => highlightsForStamp(x, data));

let locationSize = ({Location.loc_start, loc_end}) => loc_end.Lexing.pos_cnum - loc_start.Lexing.pos_cnum;

