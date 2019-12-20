
let indent = n => {
  let buf = Buffer.create(n);
  for (_ in 1 to n) {
    buf->Buffer.add_char(' ')
  }
  buf->Buffer.contents
};

let clear = () => {
  ANSITerminal.move_bol();
  ANSITerminal.erase(ANSITerminal.Eol)
}

/*

A
- a1
- B
  - b1
  - b2


*/

let showSummary = ({Runner.succeeded, failed, skipped, errors}) => {
  let total = failed + skipped + succeeded;
  [
    failed > 0
    ? Some(string_of_int(failed) ++ "/" ++ string_of_int(total) ++ " failed")
    : Some("All " ++ string_of_int(total) ++ " succeeded"),
    skipped > 0 ? Some(string_of_int(skipped) ++ " skipped") : None,
    errors > 0 ? Some(string_of_int(errors) ++ " errors") : None
  ]->Belt.List.keepMap(x => x) |> String.concat(", ")
};

let suiteResult = fun
  | Runner.BeforeError(err, count) => "BeforeAll failed: " ++ err ++ " (skipped " ++ string_of_int(count) ++ " tests)"
  | SuiteSkipped(count) => "Suite skipped (" ++ string_of_int(count) ++ " tests)"
  | Results({
    tests,
    afterErr,
  }) => showSummary(Runner.summarize(tests)) ++ (switch afterErr { | None => "" | Some(e) => "  afterAll failed: " ++ e})

let report = fun
  | Runner.SuiteStart(name, trail) => {
    print_string("\n" ++ indent(List.length(trail) * 2) ++ name);
    flush(stdout)
  }
  | Start((tests, suites)) => {
    Printf.printf("Collected %d suites and %d tests", suites, tests);
    flush(stdout)
  }
  | SuiteEnd(name, trail, result, time) => {
    print_string("\n" ++ indent(List.length(trail) * 2) ++ name ++ " -- " ++ suiteResult(result) ++ Printf.sprintf(" took %0.2fs", time))
    flush(stdout)
  }
  | TestStart(name, trail) => {
    print_string("\n" ++ indent(List.length(trail) * 2) ++ name)
    flush(stdout)
  }
  | TestEnd(name, trail, result) => {
    clear()
    print_string(indent(List.length(trail) * 2) ++ name ++ " - " ++ (Runner.success(result) ? "PASS" : "FAIL"))
    switch result {
      | TestResult({err: Some(err)}) => print_string("  - " ++ err)
      | BeforeEachError(err) => print_string("  - beforeEach failed: " ++ err)
      | _ => ()
    }
    flush(stdout)
  };

