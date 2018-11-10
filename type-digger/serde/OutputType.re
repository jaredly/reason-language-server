open Belt;

open SharedTypes.SimpleType;

let outputDeclaration = (showSource, declaration) => {

}

/* TODO TODO TODO */
let rec outputDeclaration = (showSource, declaration) => {
  let name: Ast_helper.str = Location.mknoloc(declaration.name)
  let mk = Ast_helper.Type.mk;
  switch (declaration.body) {
  | Open => mk(~kind=Parsetree.Ptype_open, name)
  | Abstract => mk(~kind=Parsetree.Ptype_abstract, name)
  | Expr(expr) => mk(~manifest=outputExpr(showSource, expr), name)
  | Record(items) =>
    mk(~kind=Parsetree.Ptype_record(
      items->List.map(((name, v)) =>
        {
          Parsetree.pld_name: Location.mknoloc(name),
          pld_mutable: Asttypes.Immutable,
          pld_type: outputExpr(showSource, v),
          pld_loc: Location.none,
          pld_attributes: [],
        }
      ),
    ), name);
  | Variant(items) =>
    mk(~kind=Parsetree.Ptype_variant(
         items->List.map(((name, contents, result)) =>
          Ast_helper.Type.constructor(
            ~args=Parsetree.Pcstr_tuple(contents->List.map(outputExpr(showSource))),
            Location.mknoloc(name)
          )
           /* name
           ++ (
             switch (contents) {
             | [] => ""
             | _ =>
               " "
               ++ String.concat(" * ", contents->List.map(outputExpr(showSource)))
               ++ (
                 switch (result) {
                 | None => ""
                 | Some(contents) => ": " ++ outputExpr(showSource, contents)
                 }
               )
             }
           ) */
         )
    ), name)
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
