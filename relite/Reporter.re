
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
  [
    Some(string_of_int(failed) ++ "/" ++ string_of_int(failed + succeeded + skipped) ++ " failed"),
    skipped > 0 ? Some(string_of_int(skipped) ++ " skipped") : None,
    errors > 0 ? Some(string_of_int(errors) ++ " errors") : None
  ]->Belt.List.keepMap(x => x) |> String.concat(", ")
};

let suiteResult = fun
  | Runner.BeforeError(_) => "BeforeAll failed"
  | Results({
    tests
  }) => showSummary(Runner.summarize(tests))

let report = fun
  | Runner.SuiteStart(name, trail) => {
    print_string("\n" ++ indent(List.length(trail) * 2) ++ name);
    flush(stdout)
  }
  | SuiteEnd(name, trail, result) => {
    print_string("\n" ++ indent(List.length(trail) * 2) ++ name ++ " -- " ++ suiteResult(result))
    flush(stdout)
  }
  | TestStart(name, trail) => {
    print_string("\n" ++ indent(List.length(trail) * 2) ++ name)
    flush(stdout)
  }
  | TestEnd(name, trail, result) => {
    clear()
    print_string(indent(List.length(trail) * 2) ++ name ++ " - " ++ (Runner.success(result) ? "PASS" : "FAIL"))
    flush(stdout)
  };

