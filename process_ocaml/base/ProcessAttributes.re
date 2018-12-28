
#if 407
open Compiler_libs_407;
#elif 406
open Compiler_libs_406;
#elif 402
open Compiler_libs_402;
#endif
open SharedTypes;
open Infix;

/* TODO should I hang on to location? */
let rec findDocAttribute = (attributes) => {
  open Parsetree;
  switch attributes {
  | [] => None
  | [({Asttypes.txt: "ocaml.doc"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
#if 402
    Const_string
#else
    Pconst_string
#endif
    (doc, _))}, _)}])), ..._] => Some(PrepareUtils.cleanOffStars(doc))
  | [_, ...rest] => findDocAttribute(rest)
  }
};

/* TODO should I hang on to location? */
let rec findDeprecatedAttribute = (attributes) => {
  open Parsetree;
  switch attributes {
  | [] => None
  | [({Asttypes.txt: "ocaml.deprecated" | "deprecated"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
#if 402
    Const_string
#else
    Pconst_string
#endif
    (message, _))}, _)}])), ..._] => Some(message)
  | [_, ...rest] => findDeprecatedAttribute(rest)
  }
};

let newDeclared = (~contents, ~scope, ~extent, ~name, ~stamp, ~modulePath, ~processDoc, exported, attributes) => {
  {
    name,
    stamp,
    extentLoc: extent,
    scopeLoc: scope,
    deprecated: findDeprecatedAttribute(attributes),
    exported,
    modulePath,
    docstring: findDocAttribute(attributes) |?>> processDoc,
    contents,
    /* scopeType: Let, */
    /* scopeStart: env.scopeStart, */
  };
};
