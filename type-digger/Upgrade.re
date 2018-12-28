
let migrateName = (~moduleName, ~modulePath, ~name) =>"migrate_" ++ Serde.MakeDeserializer.fullName(~moduleName, ~modulePath, ~name);

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

let mapAllWithIndex = (items, fn) => {
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

let rec migrateExpr = (variable, expr) => {
  switch expr {
  | Reference(TypeMap.DigTypes.Public((moduleName, modulePath, name)), args) =>
    let%opt argMappers = args->mapAll(arg => {
    let%opt mapper = migrateExpr([%expr arg], arg);
      Some((Nolabel, [%expr arg => [%e mapper]]))
    });
    Some(Exp.apply(
      expIdent(Lident(migrateName(~moduleName, ~modulePath, ~name))),
      argMappers @ [(Nolabel, variable)]
    ))
  | Reference(Builtin("list"), [arg]) =>
    let%opt converter = migrateExpr([%expr _item], arg);
    Some([%expr [%e variable]->Belt.List.map(_item => [%e converter])]);
  | Reference(Builtin("array"), [arg]) =>
    let%opt converter = migrateExpr([%expr _item], arg);
    Some([%expr [%e variable]->Belt.Array.map(_item => [%e converter])]);
  | Reference(Builtin("option"), [arg]) =>
    let%opt converter = migrateExpr([%expr _item], arg);
    Some([%expr switch ([%e variable]) {
      | None => None
      | Some(_item) => Some([%e converter])
    }]);
  | Reference(Builtin(_), []) => Some(variable)
  | Tuple(contents) =>
    let%opt migrators = contents->mapAllWithIndex((index, expr) => migrateExpr(expIdent(Lident("arg" ++ string_of_int(index))), expr))
    let pat = Pat.tuple(contents->Belt.List.mapWithIndex((index, _) => Pat.var(mknoloc("arg" ++ string_of_int(index)))));
    Some([%expr {
      let [%p pat] = [%e variable];
      [%e Exp.tuple(migrators)]
    }])
  | _ => {
    print_endline("Cannot automatically migrate this expression");
    print_endline(Vendor.Json.stringify(TypeMapSerde.dumpExpr(expr)));
    None
  }
  }
};

let orLog = (what, message) => {
  if (!what) {
    print_endline(message)
  };
  what
};

let migrateBetween = (~version as _, ~lockedDeep as _, variable, _name, thisType, prevType, ~namedMigrateAttributes, ~prevTypeName) => {
  switch (thisType.body, prevType.body) {
  | (Expr(current), Expr(prev)) when current == prev => migrateExpr(variable, current)
  | (Record(items), Record(prevItems)) when items->Belt.List.every(item => {
    (namedMigrateAttributes->Belt.List.hasAssoc(fst(item), (==))
    || prevItems->Belt.List.has(item, (==)))->orLog("Bad record item: " ++ fst(item))
  }) =>
    let rec loop = (items, labels) =>
      switch (items) {
      | [] => Some(Exp.record(labels, None))
      | [(name, expr), ...rest] =>
        let%opt migrate = switch (namedMigrateAttributes->Belt.List.getAssoc(name, (==))) {
          | Some(migrateExpr) => Some(Exp.apply(
            Exp.constraint_(
              migrateExpr,
              Typ.arrow(Nolabel, prevTypeName, Serde.OutputType.outputExpr(Serde.OutputType.showSource, expr))
            )
            , [(Nolabel, variable)]))
          | None => migrateExpr(Exp.field(variable, mknoloc(Lident(name))), expr);
        };
        let%opt inner = loop(rest, [(mknoloc(Lident(name)), expIdent(Lident("_converted_" ++ name))), ...labels]);
        Some(
          [%expr {
            let [%p Pat.var(mknoloc("_converted_" ++ name))] = [%e migrate];
            [%e inner];
          }]
        )
      };
    loop(items, []);
  | (Variant(items), Variant(prevItems)) when prevItems->Belt.List.every(((name, _, _) as item) => {
    namedMigrateAttributes->Belt.List.hasAssoc(name, (==)) ||
    items->Belt.List.has(item, (==))
  }) => {
    let%opt cases = prevItems->mapAll(((name, args, _result)) => {
      switch (namedMigrateAttributes->Belt.List.getAssoc(name, (==))) {
      | Some({pexp_desc: Pexp_fun(Nolabel, None, pattern, body)}) =>
        Some(Exp.case(pattern, body))
      | Some(_) => failwith("Variant constructor migrator must have the form (pattern) => expression")
      | None =>
        let%opt_wrap migraters = args->mapAllWithIndex((i, arg) => {
          let name = "arg" ++ string_of_int(i)
          let%opt migrater = migrateExpr(expIdent(Lident(name)), arg);
          Some((Pat.var(mknoloc(name)), migrater))
        });
        let (pat, exp) = switch migraters {
          | [] => (None, None)
          | _ => {
            let (pats, exps) = Belt.List.unzip(migraters);
            (Some(Pat.tuple(pats)), Some(Exp.tuple(exps)))
          }
        };
        Exp.case(Pat.construct(mknoloc(Lident(name)), pat), Exp.construct(mknoloc(Lident(name)), exp));
      }
    });
    Some(Exp.match(variable, cases));
  }
  | _ => {
    print_endline("Bailed out");
    None
  }
  };
};

let getExpr = payload => switch payload {
  | Parsetree.PStr([{pstr_desc: Parsetree.Pstr_eval(expr, _)}])
  | Parsetree.PStr([{pstr_desc: Parsetree.Pstr_value(Asttypes.Nonrecursive, [
    {
      pvb_pat: {ppat_desc: Ppat_any},
      pvb_expr: expr
    }
  ])}]) => Some(expr)
  | _ => None
};

let makeUpgrader = (version, _prevTypeMap, lockedDeep, ~moduleName, ~modulePath, ~name, (attributes, decl), (_pastAttributes, pastDecl)) => {
  let source = (moduleName, modulePath, name);
  let boundName = migrateName(~moduleName, ~modulePath, ~name);

  let migrateAttribute = attributes |> Util.Utils.find((({Asttypes.txt}, payload)) => {
    if (txt == "migrate") {
      switch (getExpr(payload)) {
        | Some(expr) =>
        print_endline("Have migrate attr");
        Some(expr)
        | None =>
          /* Printast.structure(0, Stdlib.Format.str_formatter, items);
          print_endline(Stdlib.Format.flush_str_formatter()); */
          failwith("migrate attribute must be an expression")
      }
    } else {
      None
    }
  });

  let namedMigrateAttributes = attributes->Belt.List.keepMap((({Asttypes.txt}, payload)) => {
    switch (Util.Utils.split_on_char('.', txt)) {
      | ["migrate"] => None
      | ["migrate", something] => {
        /* print_endline("migrate -- " ++ something) */
        switch (getExpr(payload)) {
          | None => failwith("migrate attribute must be an expression")
          | Some(expr) => Some((something, expr))
        }
      }
      | ["migrate", ..._] => {
        print_endline("Warning: invalid migrate specifier " ++ txt);
        None
      }
      | _ => None
    }
  });

  let versionModuleName = version => "Version" ++ string_of_int(version);

  let typeName = Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name);

  let args = decl.variables->Belt.List.map(Serde.OutputType.outputExpr(Serde.OutputType.showSource));
  let argNames = decl.variables->Belt.List.map(expr => switch expr {
    | Variable(name) => name
    | _ => failwith("Unnamed variable in type " ++ name)
  });

  let prevTypeName = Typ.constr(
    mknoloc(Ldot(Lident(versionModuleName(version - 1)), typeName)),
    args
  );

  let migratorFunction =
    switch (migrateAttribute) {
    | Some(expr) => expr
    | None =>
      let body =
        if (lockedDeep[version - 1]->Hashtbl.find(source) == lockedDeep[version]->Hashtbl.find(source)) {
          %expr
          _input_data;
        } else {
          switch (
            migrateBetween(
              ~version,
              ~lockedDeep,
              [%expr _input_data],
              name,
              decl,
              pastDecl,
              ~namedMigrateAttributes,
              ~prevTypeName,
            )
          ) {
          | None => failwith("Must provide migrater. Cannot migrate automatically: " ++ name)
          | Some(expr) => expr
          };
        };
      argNames->Belt.List.reduce(Exp.fun_(Nolabel, None, Pat.var(mknoloc("_input_data")), body), (body, arg) =>
        Exp.fun_(Nolabel, None, Pat.var(mknoloc("_migrator_" ++ arg)), body)
      );
    };

  let functionType = argNames->Belt.List.reduce(
    Typ.arrow(Nolabel, prevTypeName, Typ.constr(mknoloc(Lident(typeName)), argNames->Belt.List.map(arg => Typ.var(arg ++ "_migrated")))),
    (inner, arg) => Typ.arrow(Nolabel,
      Typ.arrow(Nolabel, Typ.var(arg), Typ.var(arg ++ "_migrated")),
      inner
    )
  );

  Vb.mk(
    Pat.var(mknoloc(boundName)),
    Exp.constraint_(migratorFunction, functionType,
    ),
  );
};
