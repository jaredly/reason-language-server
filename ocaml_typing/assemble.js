
const items = `
UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo

PARSING=parsing/location.cmo parsing/longident.cmo \
  parsing/docstrings.cmo parsing/ast_helper.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo \
  parsing/pprintast.cmo \
  parsing/ast_mapper.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/cmi_format.cmo typing/env.cmo \
  typing/typedtree.cmo typing/printtyped.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/envaux.cmo typing/includecore.cmo \
  typing/typedtreeIter.cmo typing/typedtreeMap.cmo typing/cmt_format.cmo \
  typing/includemod.cmo typing/typetexp.cmo typing/parmatch.cmo \
  typing/stypes.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/typeopt.cmo bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo \
  driver/pparse.cmo driver/main_args.cmo \
  driver/compenv.cmo driver/compmisc.cmo
`.replace('\\\n', '')
.split('\n')
.filter(x => x.trim())
.map(line => line.split('=')[1].split(/\s+/g).map(item => item.replace(/.cmo$/, '')))

const fs = require('fs')
const path = require('path')
const base = '/Users/jared/clone/fork/ocaml'

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
  fs.writeFileSync(path.join(__dirname, '402', base), fs.readFileSync(file))
})


// cp ../../fork/bsb-native/vendor/ocaml/parsing/parser.ml*
// cp ../../fork/bsb-native/vendor/ocaml/parsing/lexer.ml* ocaml_typing/402
// cp ../../fork/bsb-native/vendor/ocaml/utils/config.ml* ocaml_typing/402