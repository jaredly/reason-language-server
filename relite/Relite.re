Printexc.record_backtrace(true);

open Types;

module Suite = Suite;
module Types = Types;

type relite = {
  root: suite(unit, unit, unit),
  describe: describeWithOptions(unit),
  run: unit => Runner.suiteResult,
};

type options = {
  filter: option(Runner.filter),
}
let defaultOptions = {filter: None};

let failureFile = ".test-failures";

let sep = ":::"
let split = (string, sep) => Str.split(Str.regexp_string(sep), string)
let parseFailureFile = text => text->split("\n")->Belt.List.map(line => line->split(sep)->List.rev);
let makeFailureFile = paths => paths->Belt.List.map(path => String.concat(sep, path->List.rev)) |> String.concat("\n");
let loadFailureFile = () => {
  switch (Files.readFile(failureFile)) {
    | None => failwith("Cannot retest, " ++ failureFile ++ " doesn't exist")
    | Some(text) => Runner.Exactly(parseFailureFile(text))
  }
}
let saveFailureFile = paths => Files.writeFile(failureFile, makeFailureFile(paths));

let rec parseArgs = (options, args) => switch args {
  | [] => options
  | ["--retest", ...rest] => {
    parseArgs({...options, filter: Some(loadFailureFile())}, rest)
  }
  | [arg, ...rest] => parseArgs({...options, filter: Some(Runner.Substr(arg))}, rest)
};

let init = () => {
  let options = parseArgs(defaultOptions, Sys.argv->Belt.List.fromArray->List.tl);
  let root = Suite.root();
  {
    root,
    describe: Suite.describe(root),
    run: () => {
      let filtered = switch (options.filter) {
        | None => root
        | Some(filter) => Runner.filterSuite(root, filter)
      };
      let result = Runner.run(~report=Reporter.report, Expect.default, filtered);
      let summary = Runner.summarizeSuite(result);
      let failingPaths = Runner.suiteFailingPaths(result);
      if (failingPaths != []) {
        saveFailureFile(failingPaths)->ignore;
        print_endline("Savign");
      };
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