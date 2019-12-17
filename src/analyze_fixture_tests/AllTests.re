
module type Test = {
  let name: string;
  let getOutput: (~projectDir: string, list((string, string)), string) => string;
};

let projectDir = "./src/analyze_fixture_tests/old_ocamls/407"
let projectDir = "."

let tests: list((module Test)) = [
  (module TestCodeLens),
  (module TestCompletions),
  (module TestDefinition),
  (module TestReferences),
  (module TestHover),
];

let args = Array.to_list(Sys.argv);
let (args, debug, update) = {
  let rec loop = (items, ok, verbose, update) => switch items {
    | ["-v" | "--verbose", ...rest] => loop(rest, ok, true, update)
    | ["-u" | "--update", ...rest] => loop(rest, ok, verbose, true)
    | [one, ...rest] => loop(rest, [one, ...ok], verbose, update)
    | [] => (ok, verbose, update)
  };
  loop(args |> List.rev, [], false, false)
};

// Util.Log.spamError := true;

let (suite, name) = switch (args) {
  | [_] => (None, None)
  | [_, suite] => (Some(suite), None)
  | [_, suite, name] => (Some(suite), Some(name))
  | _ => failwith("Invalid args")
};

Printexc.record_backtrace(true);
if (debug) {
  Log.spamError := true;
};

// let envs = Unix.environment();
// envs->Belt.Array.forEach(item => {
//   switch (Str.split(Str.regexp_string("="), item)) {
//     | [] => assert(false)
//     | [name, ..._] =>
//       if (
//         Str.string_match(Str.regexp_string("cur__"), name, 0) ||
//         name == "OCAMLLIB" ||
//         name == "OCAMLPATH" ||
//         name == "CAML_LD_LIBRARY_PATH" ||
//         Str.string_match(Str.regexp_string("OCAMLFIND"), name, 0)
//         ) {
//         // clear out esy env vbls
//         Unix.putenv(name, "")
//       }
//   }
// })

print_endline("Test dir: " ++ TestUtils.tmp);
let failures = ref([])
let total = ref(0)
tests |. Belt.List.forEach(m => {
  Files.removeDeep(TestUtils.tmp);
  Files.mkdirp(TestUtils.tmp);
  module M = (val m);
  if (suite == None || suite == Some(M.name)) {
    print_endline("## " ++ M.name);
    let fileName = "./src/analyze_fixture_tests/" ++ M.name ++ ".txt";
    let expected = Files.readFileExn(fileName);
    let (sections, failed_items, total_items) = expected |> Utils.splitLines |. TestUtils.process(M.getOutput(~projectDir));
    failures := failures^ @ (failed_items -> Belt.List.map(item => (M.name, item)));
    let actual = sections |> String.concat("\n");
    total := total^ + total_items;
    if (failed_items != []) {
      if (update) {
        Files.writeFileExn(fileName, actual);
      }
    }
  }
});

if (failures^ == []) {
  print_endline("All clear!")
  exit(0)
} else if (update) {
  print_endline(Printf.sprintf("Updated %d/%d fixtures", List.length(failures^), total^))
} else {
  print_endline(Printf.sprintf("%d/%d fixtures failed!", List.length(failures^), total^));
  exit(1)
}
