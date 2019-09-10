module Parsetree = Parsetree;
module Typedtree = Typedtree;
module Types = Types;
module Location = Location;
module Path = Path;
module Ident = Ident;
module Asttypes = Asttypes;
module Cmi_format = Cmi_format;
module Cmt_format = Cmt_format;

/* Unfortunately we need to depend on Compiler_libs_406, otherwise we can't
 * construct an ident with a custom stamp.
 */
let makeIdent = (name, stamp) => {
  let ident = Current.Local({ name, stamp });
  (Obj.magic(ident): Ident.t);
};


