
module Parsetree = Parsetree;
module Typedtree = Typedtree;
module Types = Types;
module Location = Location;
module Path = Path;
module Ident = Ident;
module Asttypes = Asttypes;
module Cmi_format = Cmi_format;
module Cmt_format = Cmt_format;

/* let makeIdent = (name, stamp, flags) => {
  let ident_406 = Compiler_libs_406.{ Ident.name, stamp, flags };
  (Obj.magic(ident_406) : Ident.t)
}; */

/*

Things I had to change


- Const_string -> Pconst_string
- Tstr/sig_type has another arg
- Arglabels
- Constructor records
- Texp_record lost the second arg, oh and uses a record
- Texp_function 3 -> 1 arg - also uses a record

 */
