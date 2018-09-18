
open Parsetree;
open Ast_helper;
open Longident;
open Asttypes;

let failer = message => Exp.ident(Lident("failwith"), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]);

let rec expr = (sourceTransformer, t) => switch t {
  | Variable(string) => "transform" ++ string
  | AnonVariable => "failwith(\"cannot\")"
  | Reference(source, args) =>
    sourceTransformer(source)
};

let decl = (sourceTransformer, {name, variables, body}) => {
  Vb.mk(
    Pat.var(name),
    switch body {
      | Open => failer("Cannot transform an open type")
      | _ => failer("Not impl")
    }
  )
};
