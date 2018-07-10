
open Infix;

let ensure = (v, m) => {
  if (!v) {
    print_endline("Error: " ++ m);
  }
};

let getOutput = (files, text) => {
  let state = TestUtils.getState();
  let package = TestUtils.getPackage([
    ("Test", ("/tmp/.lsp-test/Test.cmt", "/path/to/Test.re")),
    ...(files |> List.map(((name, _)) => (
      Filename.chop_extension(name),
      ("/tmp/.lsp-test/" ++ Filename.chop_extension(name) ++ ".cmt", "/path/to/" ++ name)
    )))
  ]);

  files |> List.iter(((name, contents)) => {
    let moduleName = Filename.chop_extension(name);
    /* print_endline("Compiling " ++ moduleName); */
    let%try_force result = AsYouType.process(
      ~moduleName,
      contents,
      ~cacheLocation=TestUtils.tmp,
      "./node_modules/.bin/bsc",
      "./node_modules/bs-platform/lib/refmt.exe",
      [TestUtils.tmp],
      ""
    );
    let (cmt, moduleData) = switch result {
      | AsYouType.Success(warnings, cmt, moduleData) => {
        /* print_endline("Good"); */
        (cmt, moduleData)
      }
      | TypeError(message, cmt, moduleData) => {
        print_endline("Failed to compile supporting file " ++ name ++ message);
        (cmt, moduleData)
      }
    };
    /* let cmtFile = ("/path/to/" ++ Filename.chop_extension(name) ++ ".cmt", "/path/to/" ++ name); */
    /* Hashtbl.replace(state.cmtCache, cmtFile, cmt); */
    let uri = "file://path/to/" ++ name;
    Hashtbl.replace(state.compiledDocuments, uri, result);
    /* let%opt_consume (_, data) = AsYouType.getResult(result); */
    Hashtbl.replace(state.lastDefinitions, uri, moduleData)
  });

  let (text, offset, pos) = TestUtils.extractPosition(text);

  let%try_force result = AsYouType.process(
    ~moduleName="Test",
    text,
    ~cacheLocation=TestUtils.tmp,
    "./node_modules/.bin/bsc",
    "./node_modules/bs-platform/lib/refmt.exe",
    [TestUtils.tmp],
    ""
  );

  let moduleData = switch result {
    | AsYouType.Success(warnings, cmt, moduleData) => {
      /* print_endline("Good"); */
      moduleData
    }
    | TypeError(message, cmt, moduleData) => {
      /* print_endline("Failed " ++ message); */
      moduleData
    }
  };

  let completions = switch (PartialParser.findCompletable(text, offset)) {
  | Nothing => {
    print_endline("Nothing completable found :/");
    print_endline(text);
    print_endline(string_of_int(offset));
    print_endline(string_of_int(String.length(text)));
    []
  }
  | Labeled(string) => {
    Log.log("don't yet support completion for argument labels, but I hope to soon!");
    []
  }
  | Lident(string) => {
    print_endline("Complete: " ++ string);
    let parts = Str.split(Str.regexp_string("."), string);
    let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
    let opens = PartialParser.findOpens(text, offset);
    let useMarkdown = !state.settings.clientNeedsPlainText;
    Completions.get(~currentPath="/path/to/Test.re", "Test", [], parts, state, Some(moduleData), pos, ~package);
  }
  };

  completions |> List.map((item: Completions.item) => {
    item.label
    ++ fold(item.path, "", uri => "\n- uri: " ++ uri)
    ++ fold(item.detail, "", detail => "\n> " ++ (Str.split(Str.regexp_string("\n"), detail) |> String.concat("\n> ")))
  }) |> String.concat("\n");
};

let process = lines => {
  let sections = TestParser.parseSections(lines);
  let hasOnly = Belt.List.some(sections, s => switch s {
    | `Test(name, _, _, _) => Utils.startsWith(name, "*")
    | _ => false
  });
  sections |> List.map(section => switch section {
    | `Header(name) => "### " ++ name ++ "\n"
    | `Test(name, mainFile, files, result) => {
      if (hasOnly && !Utils.startsWith(name, "*")) {
        "=== " ++ name ++ "\n" ++ TestParser.printFiles(mainFile, files) ++ "\n"
        ++ "-->\n" ++ String.concat("\n", result)
      } else {
        /* print_endline("Running " ++ name); */
        /* files |> List.iter(((name, _)) => print_endline("File: " ++ name)); */
        let newResult = getOutput(files, mainFile);
        "=== " ++ name ++ "\n" ++ TestParser.printFiles(mainFile, files) ++ "\n"
        ++ "-->\n" ++ newResult ++ "\n"
      }
    }
  })
};

let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

print_endline(logDest);
let testFile = "./tests/Completion.txt";
let raw = Files.readFileExn(testFile) |> Utils.splitLines;
let newRaw = process(raw) |> String.concat("\n");
/* print_endline(newRaw); */
Files.writeFileExn(testFile, newRaw);

/* let cases =  */
/* cases |> List.iter(test) */