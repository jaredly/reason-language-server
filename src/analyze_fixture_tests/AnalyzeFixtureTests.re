open TestFramework;

let {describe, describeOnly} =
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

let projectDirs = [
  "./examples/bs-3.1.5",
  "./examples/dune",
  "./examples/example-react",
  "."
];

let baseDir = Sys.getcwd();
projectDirs->Belt.List.forEach(projectDir => {

  let compilerVersion = switch (TestUtils.compilerForProjectDir(projectDir)) {
    | Error(e) =>
      print_endline("FATAL: Unable to determine compiler + build system for project dir " ++ projectDir);
      exit(1)
    | Ok((buildSystem, compilerPath, compilerVersion)) =>
      compilerVersion
  };

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
                    let expected = item.versionedResults->Belt.List.getBy(((v, _)) => v == compilerVersion);
                    let expected = switch expected {
                      | None => item.result
                      | Some((_, results)) => results
                    };
                    expect.string(
                      M.getOutput(~projectDir=Filename.concat(baseDir, projectDir), item.otherFiles, item.mainContent)
                      ->String.trim,
                    ).
                      toEqual(
                      String.concat("\n", expected)->String.trim,
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