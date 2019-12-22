
open Infix;
open SharedTypes;

/* Log.spamError := true; */

let getOutput = (~projectDir, files, text) => {
  let (text, offset, pos) = TestUtils.extractPosition(text);
  let (state, package, cmt, full) = TestUtils.setUp(~projectDir, files, text)
  let completions = switch (PartialParser.findCompletable(text, offset)) {
  | Nothing => failwith("Nothing completable found")
  | Labeled(string) => failwith("Can't do labeled completions yet")
  | Lident(string) =>
    Log.log("Complete: " ++ string);
    let parts = Str.split(Str.regexp_string("."), string);
    let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
    let rawOpens = PartialParser.findOpens(text, offset);
    // let useMarkdown = !state.settings.clientNeedsPlainText;
    let allModules = package.localModules;
    Log.log(showExtra(full.extra));
    NewCompletions.get(
      ~full,
      ~rawOpens,
      ~getModule=name => {
        Log.log("Getting module " ++ name);
        State.fileForModule(state, ~package, name) |> logIfAbsent("Unable to find module " ++ name);
      },
      ~allModules,
      ~package,
      pos,
      parts,
      );
  };

  completions |> List.map(((uri, item)) => {
    item.name.txt
    ++ "\n- path: " ++ uri
    ++ "\n> " ++ (Str.split(Str.regexp_string("\n"), NewCompletions.detail(item.name.txt, item.contents)) |> String.concat("\n> "))
  }) |> String.concat("\n");
};

let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

// let testFile = "./tests/TestCompletions.txt";
// let output = Files.readFileExn(testFile) |> Utils.splitLines |. TestUtils.process(getOutput(~projectDir=".")) |> String.concat("\n");
// Files.writeFileExn(testFile, output);

/* let cases = 
cases |> List.iter(test) */
