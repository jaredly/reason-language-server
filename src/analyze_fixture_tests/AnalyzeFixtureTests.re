open TestFramework;

let {describe} =
  describeConfig
  |> withLifecycle(lc =>
       lc
       |> beforeEach(() => {
            Files.removeDeep(TestUtils.tmp);
            Files.mkdirp(TestUtils.tmp);
          })
     )
  |> build;

module type Test = {
  let name: string;
  // let fixture: string;
  let getOutput: (~projectDir: string, list((string, string)), string) => string;
};

let tests: list(module Test) = [
  (module TestCodeLens),
  (module TestCompletions),
  (module TestDefinition),
  (module TestReferences),
  (module TestHover),
];

Printexc.record_backtrace(true);
// if (debug) {
//   Log.spamError := true;
// };

let projectDirs = ["./examples/bs-3.1.5", "./examples/dune", "./examples/example-react"];

projectDirs->Belt.List.forEach(projectDir => {
  // describe(projectDir, ({describe}) => {
    tests->Belt.List.forEach(m => {
      module M = (val m);
      describe(
        M.name ++ " in dir " ++ projectDir,
        ({describe}) => {
    let fileName = "./src/analyze_fixture_tests/" ++ M.name ++ ".txt";
    let expected = Files.readFileExn(fileName);
          let sections = TestParser.collectSections(expected->Utils.splitLines);
          sections->Belt.List.forEach(section => {
            describe(section.heading, ({test}) => {
              section.children
              ->Belt.List.forEach(item => {
                  test(item.name, ({expect}) => {
                    expect.string(
                      M.getOutput(~projectDir, item.otherFiles, item.mainContent)
                      ->String.trim,
                    ).
                      toEqual(
                      String.concat("\n", item.result)->String.trim,
                    )
                  })
                })
            })
          });
        },
      );
    })
  // })
});