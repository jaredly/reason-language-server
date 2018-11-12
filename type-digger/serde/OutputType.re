open Belt;

open SharedTypes.SimpleType;

let makeLockedTypeName = (moduleName, modulePath, name) => {
  String.concat("__", ["_" ++ moduleName] @ modulePath @ [name]);
};

let unflatten = (items) => switch (Longident.unflatten(items)) {
  | None => assert(false)
  | Some(lident) => lident
};

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
         items->List.map(((name, contents, result)) =>
          Ast_helper.Type.constructor(
            ~args=Parsetree.Pcstr_tuple(contents->List.map(outputExpr(showSource))),
            Location.mknoloc(name)
          )
         )
    ), declarationName)
  };
}

and outputExpr = (showSource, expr) => {
  open Ast_helper.Typ;
  switch (expr) {
  | Variable(name) => var(name)
  | AnonVariable => any()
  | Reference(source, args) => showSource(source, args->List.map(outputExpr(showSource)))
  | Tuple(items) => tuple(items->List.map(outputExpr(showSource)))
  | Fn(args, result) =>
    let rec loop = (args) => switch args {
      | [] => outputExpr(showSource, result)
      | [(label, arg), ...rest] => arrow(Nolabel, outputExpr(showSource, arg), loop(rest))
    };
    loop(args)
  | Other => failwith("unhandled expr type")
  };
};
