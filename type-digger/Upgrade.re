
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
open Parsetree;
let loc = Location.none;

let upgradeBetween = (~version, ~lockedDeep, name, thisType, prevType) => {
  switch (thisType.body, prevType.body) {
    | (Expr(Reference(source, args)), Expr(Reference(prevSource, prevArgs))) => None
    | (Record(items), Record(prevItems)) when prevItems == items => Some([%expr "Hello"])
    | (Variant(items), Record(prevItems)) => None
    | _ => None
  }
};

let makeUpgrader = (version, prevTypeMap, lockedDeep, ~moduleName, ~modulePath, ~name, (attributes, decl), (_pastAttributes, pastDecl)) => {
  let source = (moduleName, modulePath, name);
  let name = upgradeName(~moduleName, ~modulePath, ~name);

  let upgradeAttribute = attributes |> Util.Utils.find((({Asttypes.txt}, payload)) => switch (txt, payload) {
    | ("upgrade", Parsetree.PStr([{pstr_desc: Parsetree.Pstr_eval(expr, _)}])) => Some(expr)
    | ("upgrade", _) => failwith("Upgrade attribute must be an expression")
    | _ => None
  });

  Vb.mk(
    Pat.var(mknoloc(name)),
    switch (upgradeAttribute) {
      | Some(expr) => expr
      | None =>
        Exp.fun_(
          Nolabel,
          None,
          Pat.var(mknoloc("data")),
          if (lockedDeep[version - 1]->Hashtbl.find(source) == lockedDeep[version]->Hashtbl.find(source)) {
            Exp.ident(mknoloc(Lident("data")));
          } else {
            switch (upgradeBetween(~version, ~lockedDeep, name, decl, pastDecl)) {
              | None => failwith("Must provide upgrader. Cannot upgrade automatically.")
              | Some(expr) => expr
            }
          },
        )
    }
  );
};
