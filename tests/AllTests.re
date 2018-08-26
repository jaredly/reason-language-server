
module type Test = {
  let name: string;
  let getOutput: (list((string, string)), string) => string;
};

let tests: list((module Test)) = [
  (module TestCodeLens),
  (module TestCompletions),
  (module TestDefinition),
  (module TestReferences),
];

Printexc.record_backtrace(true);
let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);
/* Log.spamError := true; */

tests |. Belt.List.forEach(m => {
  Files.removeDeep(TestUtils.tmp);
  Files.mkdirp(TestUtils.tmp);
  module M = (val m);
  print_endline("## " ++ M.name);
  let fileName = "./tests/" ++ M.name ++ ".txt";
  let output = Files.readFileExn(fileName) |> Utils.splitLines |. TestUtils.process(M.getOutput) |> String.concat("\n");
  Files.writeFileExn(fileName, output);
});
