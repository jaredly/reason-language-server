open Lexing

type location = Warnings.loc = { loc_start: position; loc_end: position; loc_ghost: bool }

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

type longident = Longident.t =
    Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

type abstract_ident = Ident.t

type ident =
  | Local of { name: string; stamp: int }
  | Scoped of { name: string; stamp: int; scope: int }
  | Global of string
  | Predef of { name: string; stamp: int }

let ident_binding_time ident =
  let current_ident = (Obj.magic ident : ident) in
  match current_ident with
  | Predef { stamp }
  | Scoped { stamp }
  | Local { stamp } -> stamp
  | Global _ -> 0

let none = Location.none
let mknoloc = Location.mknoloc

type path = Path.t =
    Pident of Ident.t
  | Pdot of path * string
  | Papply of path * path

let rec samePath p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1), Pdot(p2, s2)) -> s1 = s2 && samePath p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       samePath fun1 fun2 && samePath arg1 arg2
  | (_, _) -> false

type constant = Asttypes.constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type payload = Parsetree.payload =
  | PStr of Parsetree.structure
  | PSig of Parsetree.signature (* : SIG *)
  | PTyp of Parsetree.core_type  (* : T *)
  | PPat of Parsetree.pattern * Parsetree.expression option  (* ? P  or  ? P when E *)

module Parsetree = Parsetree
module Lexing = Lexing
module Parser = Parser
module Lexer = Lexer
