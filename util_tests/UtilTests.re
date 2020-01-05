open TestFramework;

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

describe.plain("DuneFile", ({it}) => {
  it("Complex dune file", ({expect}) => {
    Util.JbuildFile.parse(duneFile) |>  ignore
  })
});

let relpathFixtures = [
  (("/a/b/c", "/a/b/d"), "../d"),
  (("/a/b/c", "/a/b/d/e"), "../d/e"),
  (("/a/b/c", "/d/e/f"), "../../../d/e/f"),
  (("/a/b/c", "/a/b/c/d/e"), "./d/e"),

  (("C:/a/b/c", "C:/a/b/d"), "../d"),
  (("C:/a/b/c", "C:/a/b/d/e"), "../d/e"),
  (("C:/a/b/c", "C:/d/e/f"), "../../../d/e/f"),
  (("C:/a/b/c", "C:/a/b/c/d/e"), "./d/e"),
];

let caseInsensitiveFixtures = [
  (("/a/B/c", "/a/b/C/d/e"), "./d/e"),

  /* windows paths are case insensitive */
  (("C:/a/b/c", "c:/a/B/d"), "../d"),
  (("C:/a/b/c", "c:/a/B/d/e"), "../d/e"),
  (("C:/a/b/c", "c:/D/e/f"), "../../../D/e/f"),
  (("C:/a/b/c", "c:/a/b/C/d/e"), "./d/e"),
];

// Linux is case sensitive
let relpathFixtures = Sys.os_type == "Linux" ? relpathFixtures : relpathFixtures @ caseInsensitiveFixtures;

// These tests are broken on windows -- I need to modify the tests.
describe.skipIf(() => Sys.os_type == "Win32").plain("relpath", ({it}) => {
  relpathFixtures |> List.iter((((base, path), expected)) => {
    it(base ++ " + " ++ path, ({expect}) => {
      expect.string(Util.Files.relpath(base, path)).toEqual(expected)
    })
  })
})