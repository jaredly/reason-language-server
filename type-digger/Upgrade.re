
let upgradeName = (~moduleName, ~modulePath, ~name) =>"upgrade_" ++ Serde.MakeDeserializer.fullName(~moduleName, ~modulePath, ~name);

open Location;
open Longident;
open Ast_helper;
open Asttypes;

let hashFind = (tbl, key) => switch (Hashtbl.find(tbl, key)) {
  | exception Not_found => None
  | v => Some(v)
};

/* ok, so actually I need to go deep I think */
let upgradeBetween = (~version, ~lockedDeep, name, thisType, prevType) => {
  switch (prevType)


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
        Exp.ident(mknoloc(Lident("wat")))
      },
    ),
  );
};
