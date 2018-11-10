open Belt;

open SharedTypes.SimpleType;

let outputDeclaration = (a, b) => failwith("a");

/* TODO TODO TODO */
/* let rec outputDeclaration = (showSource, declaration) =>
  switch (declaration.body) {
  | Open => Parsetree.Ptype_open
  | Abstract => Parsetree.Ptype_abstract
  | Expr(expr) => outputExpr(showSource, expr)
  | Record(items) =>
    Parsetree.Ptype_record(
      items->List.map(((name, v)) =>
        {
          Parsetree.pld_name: Location.mknoloc(name),
          pld_mutable: Asttypes.Immutable,
          pld_type: outputExpr(showSource, v),
        }
      ),
    )
  | Variant(items) =>
    " = "
    ++ String.concat(
         "|\n",
         items->List.map(((name, contents, result)) =>
           name
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
           )
         ),
       )
  }
and outputExpr = (showSource, expr) =>
  switch (expr) {
  | Variable(name) => name
  | AnonVariable => failwith("anon variable")
  | Reference(source, args) => showSource(source, args->List.map(outputExpr(showSource)))
  | Tuple(items) => "(" ++ String.concat(" * ", items->List.map(outputExpr(showSource))) ++ ")"
  | Fn(args, result) =>
    String.concat(
      " -> ",
      args->List.map(((label, arg)) =>
        (
          switch (label) {
          | None => ""
          | Some(label) => "~" ++ label
          }
        )
        ++ outputExpr(showSource, arg)
      ),
    )
    ++ "->"
    ++ outputExpr(showSource, result)
  | Other => failwith("unhandled expr type")
  }; */
