
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
    let%opt converter = upgradeExpr([%expr data], arg);
    Some(
      {
        let%expr rec loop = items =>
          switch (items) {
          | [] => Belt.Result.Ok([])
          | [data, ...rest] =>
            switch ([%e converter]) {
            | Belt.Result.Error(msg) => Belt.Result.Error(msg)
            | Ok(newValue) =>
              switch (loop(rest)) {
              | Belt.Result.Error(msg) => Belt.Result.Error(msg)
              | Ok(rest) => Ok([newValue, ...rest])
              }
            }
          };
        loop([%e variable]);
      },
    );
  | _ => None
  }
};

let rec upgradeBetween = (~version, ~lockedDeep, variable, name, thisType, prevType) =>
  switch (thisType.body, prevType.body) {
  | (Expr(current), Expr(prev)) when current == prev => upgradeExpr(variable, current)
  | (Record(items), Record(prevItems)) when prevItems == items =>
    let rec loop = (items, labels) =>
      switch (items) {
      | [] => Some([%expr Belt.Result.Ok([%e Exp.record(labels, None)])])
      | [(name, expr), ...rest] =>
        switch (upgradeExpr(Exp.field(variable, mknoloc(Lident(name))), expr)) {
        | None => None
        | Some(upgrade) =>
          switch (loop(rest, [(mknoloc(Lident(name)), expIdent(Lident("_converted_" ++ name))), ...labels])) {
          | None => None
          | Some(inner) =>
            Some(
                [%expr {
                switch ([%e upgrade]) {
                  | Belt.Result.Error(err) => Belt.Result.Error(err)
                  | Ok([%p Pat.var(mknoloc("_converted_" ++ name))]) => [%e inner];
                }
              }]
            )
          }
        }
      };
    loop(items, []);
  | (Variant(items), Record(prevItems)) => None
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
            [%expr Ok(_input_data)]
          } else {
            switch (upgradeBetween(~version, ~lockedDeep, [%expr _input_data], name, decl, pastDecl)) {
              | None => failwith("Must provide upgrader. Cannot upgrade automatically.")
              | Some(expr) => expr
            }
          },
        )
    },
    Typ.arrow(Nolabel, Typ.constr(mknoloc(Ldot(Lident(versionModuleName(version - 1)), typeName)), []),
    [%type: Belt.Result.t([%t Typ.constr(mknoloc(Lident(typeName)), [])], string)]
    )
    )
  );
};
