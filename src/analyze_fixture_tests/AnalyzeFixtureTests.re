open TestFramework;

let lc =
  Relite.Suite.beforeAfterEach(
    () => {Files.mkdirp(TestUtils.tmp)},
    () => {Files.removeDeep(TestUtils.tmp)},
  );

// let {describe, describeOnly} =
//   describeConfig
//   |> withLifecycle(lc =>
//        lc
//        |> beforeEach(() => {
//           })
//      )
//   |> build;

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

let (/+) = Filename.concat;

let projectDirs = [
  ("examples" /+ "bs-3.1.5", "npm i"),
  ("examples" /+ "dune", "esy"),
  ("examples" /+ "example-react", "npm i"),
  (".", "esy"),
];

let baseDir = Sys.getcwd();
describe.withLifecycle("AnalyzeFixtureTests", lc, ({describe}) => {
  projectDirs->Belt.List.forEach(((projectDir, prepareCommand)) => {
    describe.withLifecycle(
      projectDir,
      Relite.Suite.beforeAll(() => {
        let (stdout, stderr, success) = Util.Commands.execFull(~pwd=projectDir, prepareCommand);
        if (!success) {
          failwith("Unable to run " ++ prepareCommand ++ " in " ++ projectDir);
        };

        switch (TestUtils.compilerForProjectDir(projectDir)) {
        | Error(e) =>
          failwith(
            "FATAL: Unable to determine compiler + build system for project dir " ++ projectDir ++"  "++ e,
          );
        | Ok((buildSystem, compilerPath, compilerVersion)) => compilerVersion
        };
      }),
      ({describe}) => {
      tests->Belt.List.forEach(m => {
        module M = (val m);
        describe.plain(
          M.name ++ " in dir " ++ projectDir,
          ({describe}) => {
            let fileName = "./src/analyze_fixture_tests/" ++ M.name ++ ".txt";
            let expected = Files.readFileExn(fileName);
            let sections = TestParser.collectSections(expected->Utils.splitLines);
            sections->Belt.List.forEach(section => {
              describe.plain(section.heading, ({it}) => {
                section.children
                ->Belt.List.forEach(item => {
                    it(
                      item.name,
                      ({expect, ctx: compilerVersion}) => {
                        let expected =
                          item.versionedResults
                          ->Belt.List.getBy(((v, _)) => v == compilerVersion);
                        let expected =
                          switch (expected) {
                          | None => item.result
                          | Some((_, results)) => results
                          };
                        expect.string(
                          M.getOutput(
                            ~projectDir=Filename.concat(baseDir, projectDir),
                            item.otherFiles,
                            item.mainContent,
                          )
                          ->String.trim,
                        ).
                          toEqual(
                          String.concat("\n", expected)->String.trim,
                        );
                      },
                    )
                  })
              })
            });
          },
        );
      })
    })
  })
});