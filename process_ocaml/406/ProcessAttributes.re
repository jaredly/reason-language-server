
open Compiler_libs_406;
open SharedTypes;
open Infix;

/* TODO should I hang on to location? */
let rec findDocAttribute = (attributes) => {
  open Parsetree;
  switch attributes {
  | [] => None
  | [({Asttypes.txt: "ocaml.doc"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
    Pconst_string
    (doc, _))}, _)}])), ..._] =>
    Some(PrepareUtils.cleanOffStars(doc))
  | [_, ...rest] => findDocAttribute(rest)
  }
};

/* TODO should I hang on to location? */
let rec findDeprecatedAttribute = (attributes) => {
  open Parsetree;
  switch attributes {
  | [] => None
  | [({Asttypes.txt: "ocaml.deprecated" | "deprecated"}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(
    Pconst_string
    (message, _))}, _)}])), ..._] =>
    Some(message)
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
