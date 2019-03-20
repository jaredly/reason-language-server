open Belt;

open SharedTypes.SimpleType;

let makeLockedTypeName = (moduleName, modulePath, name) => {
  String.concat("__", ["_" ++ moduleName] @ modulePath @ [name]);
};

let unflatten = (items) => switch (Longident.unflatten(items)) {
  | None => assert(false)
  | Some(lident) => lident
};

let showSource = (source, args) =>
  Ast_helper.Typ.constr(
    Location.mknoloc(
      switch (source) {
      | TypeMap.DigTypes.NotFound => failwith("Not found type reference")
      | Builtin(name) => Longident.Lident(name)
      | Public((moduleName, modulePath, name)) => Longident.Lident(makeLockedTypeName(moduleName, modulePath, name))
      },
    ),
    args,
  );

let rec outputDeclaration = (~alias, moduleName, modulePath, name, showSource, declaration) => {
  let declarationName: Ast_helper.str = Location.mknoloc(makeLockedTypeName(moduleName, modulePath, name))
  let fullReference = switch alias {
    | None => None
    | Some(reference) => Some(Ast_helper.Typ.constr(Location.mknoloc(reference), declaration.variables->Belt.List.map(outputExpr(showSource))))
  };

  let mk = Ast_helper.Type.mk(~params=declaration.variables->Belt.List.map(expr => (
    outputExpr(showSource, expr),
    Asttypes.Invariant
  )));
  switch (declaration.body) {
  | Open
    /* mk(~kind=Parsetree.Ptype_open, declarationName) */
  | Abstract
    /* mk(~kind=Parsetree.Ptype_abstract, declarationName) */
    => mk(~manifest=?fullReference, declarationName)
  | Expr(expr)
    => mk(~manifest=outputExpr(showSource, expr), declarationName)
  | Record(items) =>
    mk(~manifest=?fullReference, ~kind=Parsetree.Ptype_record(
      items->List.map(((name, v)) =>
        {
          Parsetree.pld_name: Location.mknoloc(name),
          pld_mutable: Asttypes.Immutable,
          pld_type: outputExpr(showSource, v),
          pld_loc: Location.none,
          pld_attributes: [],
        }
      ),
    ), declarationName);
  | Variant(items) =>
    mk(~manifest=?fullReference, ~kind=Parsetree.Ptype_variant(
         items->List.map(((name, contents, _result)) =>
          Ast_helper.Type.constructor(
            ~args=Parsetree.Pcstr_tuple(contents->List.map(outputExpr(showSource))),
            Location.mknoloc(name)
          )
         )
    ), declarationName)
  };
}

and outputExpr = (~mapVariable=name => Ast_helper.Typ.var(name), showSource, expr) => {
  open Ast_helper.Typ;
  switch (expr) {
  | Variable(name) => mapVariable(name)
  | AnonVariable => any()
  | RowVariant(rows, closed) =>
  print_endline("output " ++ Vendor.Json.stringify(TypeMap.SerializeSimplerType.toJson(x => Vendor.Json.String("<external>"), expr)));
  variant(rows->Belt.List.map(((label, expr)) => {
    Parsetree.Rtag(Location.mknoloc(label), [], closed, switch expr {
      | None => []
      | Some(expr) => [outputExpr(~mapVariable, showSource, expr)]
    })
  }), Closed, None)
  | Reference(source, args) => showSource(source, args->List.map(outputExpr(~mapVariable, showSource)))
  | Tuple(items) => tuple(items->List.map(outputExpr(~mapVariable, showSource)))
  | Fn(args, result) =>
    let rec loop = (args) => switch args {
      | [] => outputExpr(~mapVariable, showSource, result)
      | [(_label, arg), ...rest] => arrow(Nolabel, outputExpr(~mapVariable, showSource, arg), loop(rest))
    };
    loop(args)
  | Other => failwith("unhandled expr type")
  };
};
