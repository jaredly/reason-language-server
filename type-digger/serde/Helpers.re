open Longident;
open Parsetree;
open Ast_helper;
open Asttypes;

let loc = Location.none;

let makeIdent = MakeSerializer.makeIdent;

let failer = message => Exp.fun_(Nolabel, None, Pat.any(),
Exp.apply(Exp.ident(Location.mknoloc(Lident("print_endline"))), [
  (Nolabel, Exp.constant(Pconst_string(message, None)))
]));

let rec makeList = items => switch items {
  | [] => Exp.construct(Location.mknoloc(Lident("[]")), None)
  | [one, ...rest] => Exp.construct(Location.mknoloc(Lident("::")), Some(Exp.tuple([
    one, makeList(rest)
  ])))
};