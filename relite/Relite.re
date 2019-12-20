Printexc.record_backtrace(true);

open Types;

module Suite = Suite;
module Types = Types;

type relite = {
  root: suite(unit, unit, unit),
  describe: describeWithOptions(unit),
  run: unit => Runner.suiteResult,
};

let init = () => {
  let root = Suite.root();
  {
    root,
    describe: Suite.describe(root),
    run: () => {
      let result = Runner.run(~report=Reporter.report, Expect.default, root);
      let summary = Runner.summarizeSuite(result);
      if (summary.Runner.failed > 0 || summary.errors > 0) {
        exit(1)
      } else {
        exit(0)
      }
    }
  }
};

let {describe, run} = init();

// let showTrail = trail => List.rev(trail) |> String.concat(":");

// let showEvent = fun
//   | Runner.SuiteStart(name, trail) => ">> [" ++ name ++ "] " ++ showTrail(trail)
//   | SuiteEnd(name, trail, result) => "<< " ++ name ++ "] " ++ showTrail(trail)
//   | TestStart(name, trail) => "#> [" ++ name ++ "] " ++ showTrail(trail)
//   | TestEnd(name, trail, testResult) => "#< [" ++ name ++ "] " ++ showTrail(trail)

// let report = x => {
//   print_endline(showEvent(x))
// };
// runSuite(report, [], LockedSuite(root), {
//   v: (),
//   beforeEach: () => {
//     print_endline("root before each")
//   },
//   afterEach: () => {
//     print_endline("root after each")
//   }
// });