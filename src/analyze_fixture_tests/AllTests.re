
module type Test = {
  let name: string;
  let getOutput: (~projectDir: string, list((string, string)), string) => string;
};

let projectDir = "./src/analyze_fixture_tests/old_ocamls/407"

let tests: list((module Test)) = [
  (module TestCodeLens),
  (module TestCompletions),
  (module TestDefinition),
  (module TestReferences),
  (module TestHover),
];

let args = Array.to_list(Sys.argv);
let (args, debug) = {
  let rec loop = (items, ok, verbose) => switch items {
    | ["-v" | "--verbose", ...rest] => loop(rest, ok, true)
    | [one, ...rest] => loop(rest, [one, ...ok], verbose)
    | [] => (ok, verbose)
  };
  loop(args |> List.rev, [], false)
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

print_endline("Test dir: " ++ TestUtils.tmp);
tests |. Belt.List.forEach(m => {
  Files.removeDeep(TestUtils.tmp);
  Files.mkdirp(TestUtils.tmp);
  module M = (val m);
  if (suite == None || suite == Some(M.name)) {
    print_endline("## " ++ M.name);
    let fileName = "./src/analyze_fixture_tests/" ++ M.name ++ ".txt";
    let output = Files.readFileExn(fileName) |> Utils.splitLines |. TestUtils.process(M.getOutput(~projectDir)) |> String.concat("\n");
    Files.writeFileExn(fileName, output);
  }
});

