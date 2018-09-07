

open Lexing

type location = Location.t = { loc_start: position; loc_end: position; loc_ghost: bool }
type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

type longident = Longident.t = 
    Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

type ident = Ident.t = { stamp: int; name: string; mutable flags: int }

let none = Location.none
let mknoloc = Location.mknoloc

type path = Path.t =
    Pident of Ident.t
  | Pdot of path * string * int
  | Papply of path * path

type constant = Asttypes.constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint