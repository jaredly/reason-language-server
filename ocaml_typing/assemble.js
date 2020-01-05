const items = `
UTILS=utils/config.cmo utils/misc.cmo \
  utils/identifiable.cmo utils/numbers.cmo utils/arg_helper.cmo \
  utils/clflags.cmo utils/tbl.cmo utils/profile.cmo \
  utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo \
  utils/strongly_connected_components.cmo \
  utils/build_path_prefix_map.cmo \
  utils/targetint.cmo

PARSING=parsing/location.cmo parsing/longident.cmo \
  parsing/docstrings.cmo parsing/syntaxerr.cmo \
  parsing/ast_helper.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo \
  parsing/pprintast.cmo \
  parsing/ast_mapper.cmo parsing/ast_iterator.cmo parsing/attr_helper.cmo \
  parsing/builtin_attributes.cmo parsing/ast_invariants.cmo parsing/depend.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/cmi_format.cmo typing/env.cmo \
  typing/typedtree.cmo typing/printtyped.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/envaux.cmo typing/includecore.cmo \
  typing/typedtreeIter.cmo typing/typedtreeMap.cmo \
  typing/tast_mapper.cmo \
  typing/cmt_format.cmo typing/untypeast.cmo \
  typing/includemod.cmo typing/typetexp.cmo typing/printpat.cmo \
  typing/parmatch.cmo typing/stypes.cmo typing/typedecl.cmo typing/typeopt.cmo \
  typing/typecore.cmo typing/typeclass.cmo typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/semantics_of_primitives.cmo \
  bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translattribute.cmo \
  bytecomp/translprim.cmo bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo \
  bytecomp/meta.cmo bytecomp/opcodes.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo \
  bytecomp/symtable.cmo \
  driver/pparse.cmo driver/main_args.cmo \
  driver/compenv.cmo driver/compmisc.cmo \
  driver/compdynlink.cmo driver/compplugin.cmo driver/makedepend.cmo
`.replace('\\\n', '')
.split('\n')
.filter(x => x.trim())
.map(line => line.split('=')[1].split(/\s+/g).map(item => item.replace(/.cmo$/, '')))

const fs = require('fs')
const path = require('path')
const base = '/Users/anmonteiro/Downloads/ocaml'

// NOTE(anmonteiro): This relies on the compiler being built, since some files
// get generated as part of the building process!
//
// Relevant instructions: clone the version of the compiler you want to include
// as part of RLS and run the commands below in the compiler distribution's
// root directory. Don't forget to change the `base` variable above to point
// to your OCaml compiler distribution!
//
// $ ./configure
// $ make world.opt
const names = ['parsing/parsetree', 'parsing/asttypes', 'typing/annot', 'typing/outcometree'].concat(...items)

const allFiles = names.reduce((result, name) => {
  const ml = path.join(base, name + '.ml')
  const mli = path.join(base, name + '.mli')
  // console.log(ml)
  if (fs.existsSync(ml)) result.push(ml)
  if (fs.existsSync(mli)) result.push(mli)
  return result
}, [])

allFiles.forEach(file => {
  const base = path.basename(file)
  fs.writeFileSync(path.join(__dirname, '407', base), fs.readFileSync(file))
})


// cp ../../fork/bsb-native/vendor/ocaml/parsing/parser.ml*
// cp ../../fork/bsb-native/vendor/ocaml/parsing/lexer.ml* ocaml_typing/402
// cp ../../fork/bsb-native/vendor/ocaml/utils/config.ml* ocaml_typing/402
