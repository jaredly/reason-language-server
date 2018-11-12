
let upgradeName = (~moduleName, ~modulePath, ~name) =>"upgrade_" ++ Serde.MakeDeserializer.fullName(~moduleName, ~modulePath, ~name);

open Location;
open Longident;
open Ast_helper;
open Asttypes;

let hashFind = (tbl, key) => switch (Hashtbl.find(tbl, key)) {
  | exception Not_found => None
  | v => Some(v)
};

open SharedTypes.SimpleType;

let upgradeBetween = (~version, ~lockedDeep, name, thisType, prevType) => {
  switch (thisType.body, prevType.body) {
    | (Expr(Reference(source, args)), Expr(Reference(prevSource, prevArgs))) => Some(1)
    | (Record(items), Record(prevItems)) => Some(1)
    | (Variant(items), Record(prevItems)) => Some(1)
    | _ => None
  }
};

let makeUpgrader = (version, prevTypeMap, lockedDeep, ~moduleName, ~modulePath, ~name, decl, pastDecl) => {
  let source = (moduleName, modulePath, name);
  let name = upgradeName(~moduleName, ~modulePath, ~name);

  Vb.mk(
    Pat.var(mknoloc(name)),
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(mknoloc("data")),
      if (lockedDeep[version - 1]->Hashtbl.find(source) == lockedDeep[version]->Hashtbl.find(source)) {
        Exp.ident(mknoloc(Lident("data")));
      } else {
        Exp.ident(mknoloc(Lident("wat")));
        /* TODO allow custom t */
        /* switch (upgradeBetween(~version, ~lockedDeep, name, decl, pastDecl)) {
          | None => 
        } */
      },
    ),
  );
};
