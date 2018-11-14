
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

let expIdent = lident => Ast_helper.Exp.ident(Location.mknoloc(lident));

let rec upgradeExpr = (variable, expr) => {
  switch expr {
  | Reference(TypeMap.DigTypes.Public((moduleName, modulePath, name)), []) =>
    Some(Exp.apply(expIdent(Lident(upgradeName(~moduleName, ~modulePath, ~name))), [(Nolabel, variable)]))
  | Reference(Builtin("list"), [arg]) =>
    let%opt converter = upgradeExpr([%expr _item], arg);
    Some([%expr [%e variable]->Belt.List.map(_item => [%e converter])]);
  | Reference(Builtin("array"), [arg]) =>
    let%opt converter = upgradeExpr([%expr _item], arg);
    Some([%expr [%e variable]->Belt.Array.map(_item => [%e converter])]);
  | Reference(Builtin("option"), [arg]) =>
    let%opt converter = upgradeExpr([%expr _item], arg);
    Some([%expr switch ([%e variable]) {
      | None => None
      | Some(_item) => Some([%e converter])
    }]);
  | _ => None
  }
};

let rec mapAll = (items, fn) => switch items {
  | [] => Some([])
  | [one, ...rest] => switch (fn(one)) {
    | None => None
    | Some(one) => switch (mapAll(rest, fn)) {
      | None => None
      | Some(rest) => Some([one, ...rest])
    }
  }
};

let rec mapAllWithIndex = (items, fn) => {
  let rec loop = (i, items) =>
    switch (items) {
    | [] => Some([])
    | [one, ...rest] =>
      switch (fn(i, one)) {
      | None => None
      | Some(one) =>
        switch (loop(i + 1, rest)) {
        | None => None
        | Some(rest) => Some([one, ...rest])
        }
      }
    };
  loop(0, items);
};

let rec upgradeBetween = (~version, ~lockedDeep, variable, name, thisType, prevType) =>
  switch (thisType.body, prevType.body) {
  | (Expr(current), Expr(prev)) when current == prev => upgradeExpr(variable, current)
  | (Record(items), Record(prevItems)) when items->Belt.List.every(item => prevItems->Belt.List.has(item, (==))) =>
    let rec loop = (items, labels) =>
      switch (items) {
      | [] => Some(Exp.record(labels, None))
      | [(name, expr), ...rest] =>
        let%opt upgrade = upgradeExpr(Exp.field(variable, mknoloc(Lident(name))), expr);
        let%opt inner = loop(rest, [(mknoloc(Lident(name)), expIdent(Lident("_converted_" ++ name))), ...labels]);
        Some(
          [%expr {
            let [%p Pat.var(mknoloc("_converted_" ++ name))] = [%e upgrade];
            [%e inner];
          }]
        )
      };
    loop(items, []);
  | (Variant(items), Variant(prevItems)) when prevItems->Belt.List.every(item => items->Belt.List.has(item, (==))) => {
    let%opt cases = prevItems->mapAll(((name, args, _result)) => {
      let%opt_wrap upgraders = args->mapAllWithIndex((i, arg) => {
        let name = "arg" ++ string_of_int(i)
        let%opt upgrader = upgradeExpr(expIdent(Lident(name)), arg);
        Some((Pat.var(mknoloc(name)), upgrader))
      });
      let (pat, exp) = switch upgraders {
        | [] => (None, None)
        | _ => {
          let (pats, exps) = Belt.List.unzip(upgraders);
          (Some(Pat.tuple(pats)), Some(Exp.tuple(exps)))
        }
      };
      Exp.case(Pat.construct(mknoloc(Lident(name)), pat), Exp.construct(mknoloc(Lident(name)), exp));
    });
    Some(Exp.match(variable, cases));
  }
  | _ => None
  };

let makeUpgrader = (version, prevTypeMap, lockedDeep, ~moduleName, ~modulePath, ~name, (attributes, decl), (_pastAttributes, pastDecl)) => {
  let source = (moduleName, modulePath, name);
  let boundName = upgradeName(~moduleName, ~modulePath, ~name);

  let upgradeAttribute = attributes |> Util.Utils.find((({Asttypes.txt}, payload)) => switch (txt, payload) {
    | ("upgrade", Parsetree.PStr([{pstr_desc: Parsetree.Pstr_eval(expr, _)}])) => Some(expr)
    | ("upgrade", _) => failwith("Upgrade attribute must be an expression")
    | _ => None
  });

  let versionModuleName = version => "Version" ++ string_of_int(version);

  let typeName = Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name);

  Vb.mk(
    Pat.var(mknoloc(boundName)),
    Exp.constraint_(
    switch (upgradeAttribute) {
      | Some(expr) => expr
      | None =>
        Exp.fun_(
          Nolabel,
          None,
          Pat.var(mknoloc("_input_data")),
          if (lockedDeep[version - 1]->Hashtbl.find(source) == lockedDeep[version]->Hashtbl.find(source)) {
            [%expr _input_data]
          } else {
            switch (upgradeBetween(~version, ~lockedDeep, [%expr _input_data], name, decl, pastDecl)) {
              | None => failwith("Must provide upgrader. Cannot upgrade automatically: " ++ name)
              | Some(expr) => expr
            }
          },
        )
    },
    Typ.arrow(Nolabel, Typ.constr(mknoloc(Ldot(Lident(versionModuleName(version - 1)), typeName)), []),
    Typ.constr(mknoloc(Lident(typeName)), [])
    )
    )
  );
};
