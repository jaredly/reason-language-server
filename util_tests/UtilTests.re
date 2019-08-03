
type test =
  | Single(string, unit => option(string))
  | Multiple(string, 'a => option(string), list('a)) : test;

let tests = ref([]);
let addTest = test => tests := [test, ...tests^];
let single = (name, fn) => addTest(Single(name, fn));
let multiple = (name, fn, items) => addTest(Multiple(name, fn, items));

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

single("DuneFile", () => {
  switch (Util.JbuildFile.parse(duneFile)) {
    | exception Failure(message) => {
      Some("Unable to parse dune file: " ++ message)
    }
    | _ => None
  };
})

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

multiple("relpath", (((base, path), expected)) => {
  let output = Util.Files.relpath(base, path);
  if (output == expected) {
    None
  } else {
    Some(Printf.sprintf("%s + %s => %s :: expected %s", base, path, output, expected))
  }
}, relpathFixtures);

let (failures, total) = tests^ |> List.fold_left(((failures, total), test) => {
  switch test {
    | Single(name, fn) => 
    let (res, message) = switch (fn()) {
      | None => ((failures, total + 1), "✔️")
      | Some(message) => ((failures + 1, total + 1), "❌   " ++ message)
    };
    print_endline(name ++ " :: " ++ message);
    res
    | Multiple(name, fn, items) =>
      let (f2, t2) = items |> List.fold_left(((failures, total), item) => {
        let (res, message) = switch (fn(item)) {
          | None => ((failures, total + 1), "✔️")
          | Some(message) => ((failures + 1, total + 1), "❌   " ++ message)
        };
        Printf.printf("%s (%2d) :: %s\n", name, total, message);
        res
      }, (0, 0));
      (f2 + failures, t2 + total)
  }
}, (0, 0));

if (failures == 0) {
  Printf.printf("✅ All tests passed! %d/%d\n", total, total);
  exit(0)
} else {
  Printf.printf("❌ Failures: %d/%d\n", failures, total);
  exit(0)

}
