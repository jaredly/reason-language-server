let getOutput = (_files, text) =>
  try (
    "PASS: "
    ++ (
      JbuildFile.parse(text)
      |> List.map(JbuildFile.atomToString)
      |> String.concat("\n")
    )
  ) {
  | _ => "FAIL"
  };

let name = "TestJbuildFile";
