
let sectionHeader = line => {
  if (Utils.startsWith(line, "### ")) {
    Some(`Header(Utils.sliceToEnd(line, 4)))
  } else if (Utils.startsWith(line, "=== ")) {
    Some(`Test(Utils.sliceToEnd(line, 4)))
  } else if (Utils.startsWith(line, "-->")) {
    Some(`Results)
  } else if (Utils.startsWith(line, "---")) {
    Some(`File(Utils.sliceToEnd(line, 4)))
  } else {
    None
  }
};

let peekSection = lines => switch lines {
  | [] => (None, [])
  | [line, ...rest] => switch (sectionHeader(line)) {
    | None => (None, lines)
    | Some(header) => (Some(header), rest)
  }
};

let rec getUntilNextSection = lines => switch lines {
  | [] => ([], [])
  | [line, ...rest] => switch (sectionHeader(line)) {
    | None => {
      let (lines, after) = getUntilNextSection(rest);
      ([line, ...lines], after)
    }
    | Some(_) => ([], lines)
  }
};

let parseTest = (lines) => {
  let (lines, after) = getUntilNextSection(lines);
  let rec collectFiles = lines => {
    let (header, after) = peekSection(lines);
    switch header {
      | Some(`File(name)) => {
        let (lines, after) = getUntilNextSection(after);
        let (files, after) = collectFiles(after);
        ([(name, String.concat("\n", lines)), ...files], after)
      }
      | _ => ([], lines)
    }
  };
  let (files, after) = collectFiles(after);
  let files = switch lines {
    | [] => files
    | lines => [("Test.re", String.concat("\n", lines)), ...files]
  };
  let (header, final) = peekSection(after);
  switch (header) {
    | Some(`Results) => {
      let (lines, after) = getUntilNextSection(final);
      (files, lines, after)
    }
    | _ => (files, [], after)
  }
};

let rec splitFiles = files => {
  switch files {
    | [] => (None, [])
    | [("Test.re" | "", contents), ...rest] => (Some(contents), rest)
    | [file, ...rest] => {
      let (testFile, rest) = splitFiles(rest);
      (testFile, [file, ...rest])
    }
  }
};

let rec parseSections = lines => switch lines {
  | [] => []
  | [line, ...rest] => {
    switch (sectionHeader(line)) {
      | None => parseSections(rest)
      | Some(header) => switch header {
        | `Header(name) => [`Header(name), ...parseSections(rest)]
        | `Test(name) => {
          let (files, result, rest) = parseTest(rest);
          let (mainFile, otherFiles) = splitFiles(files);
          switch mainFile {
            | None => {
              print_endline("No Test.re file");
              parseSections(rest)
            }
            | Some(mainContent) => {
              [`Test(name, mainContent, otherFiles, result), ...parseSections(rest)]
            }
          }
        }
        | _ => parseSections(rest)
      }
    }
  }
};

type test = {name: string, mainContent: string, otherFiles: list((string, string)), result: list(string)};
type section = {heading: string, children: list(test)}

let addChild = (section, child) => {...section, children: [child, ...section.children]}

let collectSections = lines => {
  let parts = parseSections(lines);
  let rec loop = (current, previous, parts) => switch parts {
    | [`Header(heading), ...rest] => loop({heading, children: []}, [current, ...previous], rest)
    | [`Test(name, mainContent, otherFiles, result), ...rest] => loop(current->addChild({name, mainContent, otherFiles, result}), previous, rest)
    | [] => [current, ...previous] |> List.rev |> List.map(({heading, children}) => {heading, children: List.rev(children)})
  };
  switch parts {
    | [`Header(heading), ...rest] => loop({heading, children: []}, [], rest)
    | rest => loop({heading: "Default", children: []}, [], rest)
  }
}

let printFiles = (mainFile, files) => {
  switch files {
    | [] => String.trim(mainFile)
    | files => files |> List.map(((name, content)) => {
      "--- " ++ name ++ "\n" ++ String.trim(content)
    }) |> String.concat("\n") |> files => files ++ "\n---\n" ++ mainFile
  }
};