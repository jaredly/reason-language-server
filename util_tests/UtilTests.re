
let duneFile = {|
(library
   ; !!!! This dune file is generated from the package.json file. Do NOT modify by hand.
   ; !!!! Instead, edit the package.json and then rerun 'esy pesy' at the project root.
   ; The namespace other code see this as
   (name OcamlScratch)
   (public_name ocaml-scratch.lib)
   (preprocess (pps ppx_deriving ppx_deriving.std))
   (libraries core ppx_deriving.runtime)
   (c_names )  ; From package.json cNames field
   )
|};

try (Util.JbuildFile.parse(duneFile)) {
  | Failure(message) => {
    print_endline("Unable to parse dune file: " ++ message);
    exit(10)
  }
};
print_endline("Success");
exit(0)
