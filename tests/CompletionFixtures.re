
open Infix;

let getOutput = (files, text) => {
  let (text, offset, pos) = TestUtils.extractPosition(text);
  let (state, package, moduleData) = TestUtils.setUp(files, text)
  let completions = switch (PartialParser.findCompletable(text, offset)) {
  | Nothing => failwith("Nothing completable found")
  | Labeled(string) => failwith("Can't do labeled completions yet")
  | Lident(string) =>
    print_endline("Complete: " ++ string);
    let parts = Str.split(Str.regexp_string("."), string);
    let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
    let opens = PartialParser.findOpens(text, offset);
    let useMarkdown = !state.settings.clientNeedsPlainText;
    Completions.get(~currentPath="/path/to/Test.re", "Test", [], parts, state, Some(moduleData), pos, ~package);
  };

  completions |> List.map((item: Completions.item) => {
    item.label
    ++ fold(item.path, "", path => "\n- path: " ++ path)
    ++ fold(item.detail, "", detail => "\n> " ++ (Str.split(Str.regexp_string("\n"), detail) |> String.concat("\n> ")))
  }) |> String.concat("\n");
};

let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

let testFile = "./tests/Completion.txt";
let output = Files.readFileExn(testFile) |> Utils.splitLines |. TestUtils.process(getOutput) |> String.concat("\n");
Files.writeFileExn(testFile, output);

/* let cases =  */
/* cases |> List.iter(test) */