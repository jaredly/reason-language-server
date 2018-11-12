
let upgradeName = (~moduleName, ~modulePath, ~name) =>"upgrade_" ++ Serde.MakeDeserializer.fullName(~moduleName, ~modulePath, ~name);

open Location;
open Longident;
open Ast_helper;
open Asttypes;

let makeUpgrader = (version, prevTypeMap, lockedDeep, ~moduleName, ~modulePath, ~name, decl) => {
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
