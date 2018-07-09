
open Infix;

let ensure = (v, m) => {
  if (!v) {
    print_endline("Error: " ++ m);
  }
};

let getOutput = ((files, text)) => {
  let state = TestUtils.getState();
  let package = TestUtils.getPackage([
    ("Test.re", ("/path/to/Test.cmt", "/path/to/Test.re")),
    ...(files |> List.map(((name, _)) => (
      name,
      ("/path/to/" ++ Filename.chop_extension(name) ++ ".cmt", "/path/to/" ++ name)
    )))
  ]);

  files |> List.iter(((name, contents)) => {
    let moduleName = Filename.chop_extension(name);
    let%try_force result = AsYouType.process(
      ~moduleName,
      contents,
      ~cacheLocation=TestUtils.tmp,
      "./node_modules/.bin/bsc",
      "./node_modules/bs-platform/lib/refmt.exe",
      [TestUtils.tmp],
      ""
    );
    let uri = "file://path/to/" ++ name;
    Hashtbl.replace(state.compiledDocuments, uri, result);
    let%opt_consume (_, data) = AsYouType.getResult(result);
    Hashtbl.replace(state.lastDefinitions, uri, data)
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
    []
  }
  | Labeled(string) => {
    Log.log("don't yet support completion for argument labels, but I hope to soon!");
    []
  }
  | Lident(string) => {
    let parts = Str.split(Str.regexp_string("."), string);
    let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
    let opens = PartialParser.findOpens(text, offset);
    let useMarkdown = !state.settings.clientNeedsPlainText;
    Completions.get("Test", [], parts, state, Some(moduleData), pos, ~package);
  }
  };

  completions |> List.map((item: Completions.item) => {
    item.label
    ++ fold(item.uri, "", uri => "- uri: " ++ uri)
    ++ fold(item.detail, "", detail => "> " ++ (Str.split(Str.regexp_string("\n"), detail) |> String.concat("\n> ")))
  }) |> String.concat("\n");
};

let raw = Files.readFileExn("./tests/Completion.txt") |> Utils.splitLines;
/* let cases =  */
/* cases |> List.iter(test) */