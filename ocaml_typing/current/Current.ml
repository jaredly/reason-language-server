

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

type ident = Ident.t

let none = Location.none
let mknoloc = Location.mknoloc

type path = Path.t =
    Pident of Ident.t
  | Pdot of path * string * int
  | Papply of path * path

let rec samePath p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, _pos1), Pdot(p2, s2, _pos2)) -> s1 = s2 && samePath p1 p2
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

(* module Parsetree = Parsetree *)
module Lexing = Lexing
module Parser = Parser
module Lexer = Lexer

(* type pconstant = Parsetree.constant =
    Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * string option
  | Pconst_float of string * char option

type arg_label = Asttypes.arg_label =
    Nolabel
  | Labelled of string
  | Optional of string

type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

type closed_flag = Asttypes.closed_flag = Closed | Open

type structure_item = Parsetree.structure_item =
    {
     pstr_desc: Parsetree.structure_item_desc;
     pstr_loc: Location.t;
    }

type signature_item = Parsetree.signature_item =
    {
     psig_desc: Parsetree.signature_item_desc;
     psig_loc: Location.t;
    }

type core_type = Parsetree.core_type =
    {
     ptyp_desc: Parsetree.core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: Parsetree.attributes; (* ... [@id1] [@id2] *)
    }

type object_field = Parsetree.object_field =
  | Otag of Asttypes.label loc * Parsetree.attributes * Parsetree.core_type
  | Oinherit of Parsetree.core_type

type row_field = Parsetree.row_field =
  | Rtag of Asttypes.label loc * Parsetree.attributes * bool * Parsetree.core_type list
  | Rinherit of Parsetree.core_type

type core_type_desc = Parsetree.core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of Asttypes.arg_label * Parsetree.core_type * Parsetree.core_type
  | Ptyp_tuple of Parsetree.core_type list
  | Ptyp_constr of Longident.t loc * Parsetree.core_type list
  | Ptyp_object of object_field list * Asttypes.closed_flag
  | Ptyp_class of Longident.t loc * Parsetree.core_type list
  | Ptyp_alias of Parsetree.core_type * string
  | Ptyp_variant of Parsetree.row_field list * Asttypes.closed_flag * Asttypes.label list option
  | Ptyp_poly of string loc list * Parsetree.core_type
  | Ptyp_package of Parsetree.package_type
  | Ptyp_extension of Parsetree.extension

type pattern = Parsetree.pattern =
    {
     ppat_desc: Parsetree.pattern_desc;
     ppat_loc: Location.t;
     ppat_attributes: Parsetree.attributes; (* ... [@id1] [@id2] *)
    }

type pattern_desc = Parsetree.pattern_desc =
  | Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of Parsetree.pattern * string loc
  | Ppat_constant of Parsetree.constant
  | Ppat_interval of Parsetree.constant * Parsetree.constant
  | Ppat_tuple of Parsetree.pattern list
  | Ppat_construct of Longident.t loc * Parsetree.pattern option
  | Ppat_variant of Asttypes.label * Parsetree.pattern option
  | Ppat_record of (Longident.t loc * Parsetree.pattern) list * Asttypes.closed_flag
  | Ppat_array of Parsetree.pattern list
  | Ppat_or of Parsetree.pattern * Parsetree.pattern
  | Ppat_constraint of Parsetree.pattern * Parsetree.core_type
  | Ppat_type of Longident.t loc
  | Ppat_lazy of Parsetree.pattern
  | Ppat_unpack of string loc
  | Ppat_exception of Parsetree.pattern
  | Ppat_extension of Parsetree.extension
  | Ppat_open of Longident.t loc * Parsetree.pattern

type expression = Parsetree.expression =
    {
     pexp_desc: Parsetree.expression_desc;
     pexp_loc: Location.t;
     pexp_attributes: Parsetree.attributes; (* ... [@id1] [@id2] *)
    }

type expression_desc = Parsetree.expression_desc =
  | Pexp_ident of Longident.t loc
  | Pexp_constant of Parsetree.constant
  | Pexp_let of Asttypes.rec_flag * Parsetree.value_binding list * Parsetree.expression
  | Pexp_function of Parsetree.case list
  | Pexp_fun of Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern * Parsetree.expression
  | Pexp_apply of Parsetree.expression * (Asttypes.arg_label * Parsetree.expression) list
  | Pexp_match of Parsetree.expression * Parsetree.case list
  | Pexp_try of Parsetree.expression * Parsetree.case list
  | Pexp_tuple of Parsetree.expression list
  | Pexp_construct of Longident.t loc * Parsetree.expression option
  | Pexp_variant of Asttypes.label * Parsetree.expression option
  | Pexp_record of (Longident.t loc * Parsetree.expression) list * Parsetree.expression option
  | Pexp_field of Parsetree.expression * Longident.t loc
  | Pexp_setfield of Parsetree.expression * Longident.t loc * Parsetree.expression
  | Pexp_array of Parsetree.expression list
  | Pexp_ifthenelse of Parsetree.expression * Parsetree.expression * Parsetree.expression option
  | Pexp_sequence of Parsetree.expression * Parsetree.expression
  | Pexp_while of Parsetree.expression * Parsetree.expression
  | Pexp_for of
      Parsetree.pattern *  Parsetree.expression * Parsetree.expression * Asttypes.direction_flag * Parsetree.expression
  | Pexp_constraint of Parsetree.expression * Parsetree.core_type
  | Pexp_coerce of Parsetree.expression * Parsetree.core_type option * Parsetree.core_type
  | Pexp_send of Parsetree.expression * Asttypes.label loc
  | Pexp_new of Longident.t loc
  | Pexp_setinstvar of Asttypes.label loc * Parsetree.expression
  | Pexp_override of (Asttypes.label loc * Parsetree.expression) list
  | Pexp_letmodule of string loc * Parsetree.module_expr * Parsetree.expression
  | Pexp_letexception of Parsetree.extension_constructor * Parsetree.expression
  | Pexp_assert of Parsetree.expression
  | Pexp_lazy of Parsetree.expression
  | Pexp_poly of Parsetree.expression * Parsetree.core_type option
  | Pexp_object of Parsetree.class_structure
  | Pexp_newtype of string loc * Parsetree.expression
  | Pexp_pack of Parsetree.module_expr
  | Pexp_open of Asttypes.override_flag * Longident.t loc * Parsetree.expression
  | Pexp_extension of Parsetree.extension
  | Pexp_unreachable

type payload = Parsetree.payload =
  | PStr of Parsetree.structure
  | PSig of Parsetree.signature
  | PTyp of Parsetree.core_type
  | PPat of Parsetree.pattern * Parsetree.expression option *)
