
open Infix;

let ensure = (v, m) => {
  if (!v) {
    print_endline("Error: " ++ m);
  }
};

let test = ((text, expected)) => {
  let state = TestUtils.getState();
  let package = TestUtils.getPackage([("Test.re", ("/path/to/Test.cmti", "/path/to/Test.re"))]);

  let (text, offset, pos) = {
    let clean = Str.substitute_first(Str.regexp_string("<*>"), x => "", text);
    let char = Str.match_beginning();
    let pre = String.sub(text, 0, char);
    let lines = Str.split(Str.regexp_string("\n"), pre);
    (clean, char, (List.length(lines), String.length(List.hd(List.rev(lines)))))
  };

  let%try_force result = AsYouType.process(
    text,
    ~cacheLocation=TestUtils.tmp,
    "./node_modules/.bin/bsc",
    "./node_modules/bs-platform/lib/refmt.exe",
    [],
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

  if (List.length(expected) != List.length(completions)) {
    Printf.printf("Different lengths. %d expected vs %d found\n", List.length(expected), List.length(completions))
  } else {
    List.combine(expected, completions) |> List.iter((((name, detail), item: Completions.item)) => {
      ensure(name == item.label, "Wrong label");
      ensure(detail == item.detail, "Wrong detail " ++ (item.detail |? ""));
    });
  }
};

let cases = [
  (
    {|
let awesome = 10;
let x = awe<*>
|},
    [("awesome", Some("let awesome: int"))],
  ),
  (
    {|
module M = {
  let awesome = 10;
};
let x = M.awe<*>
|},
    [("awesome", Some("let awesome: int"))],
  ),
  ({|
let x = awe<*>
module M = {
  let awesome = 10;
};
|}, []),
  (
    {|
type t = {name: int, nose: float};
let m = {name: 2, nose: 1.4};
m.n<*>
|},
    [
      ("name", Some("name: int\n\ntype name = {name: int, nose: float}")),
      ("nose", Some("nose: float\n\ntype nose = {name: int, nose: float}")),
    ],
  ),
];

let failing = [
  (
    {|
type t = {name: int, nose: float};
let m = {name: 2, nose: 1.4};
m.<*>
|},
    [
      ("name", Some("name: int\n\ntype name = {name: int, nose: float}")),
      ("nose", Some("nose: float\n\ntype nose = {name: int, nose: float}")),
    ],
  ),
];

cases |> List.iter(test)