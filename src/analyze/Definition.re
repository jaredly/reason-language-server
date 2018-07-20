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
  file: SharedTypes.file,
  extra: SharedTypes.extra,
};

let maybeFound = (fn, a) =>
  switch (fn(a)) {
  | exception Not_found => None
  | x => Some(x)
  };

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
    | Lapply(left, _) => assert false
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
