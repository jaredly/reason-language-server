
let tmp = "/tmp/.lsp-test";
Files.mkdirp(tmp);
let test = ((text, expected)) => {

  let (text, pos) = {
    let clean = Str.substitute_first(Str.regexp_string("<*>"), x => "", text);
    let char = Str.match_beginning();
    let pre = String.sub(text, 0, char);
    let lines = Str.split(Str.regexp_string("\n"), pre);
    (clean, (List.length(lines), String.length(List.hd(List.rev(lines)))))
  };

  let%try_force result = AsYouType.process(
    text,
    ~cacheLocation=tmp,
    "./node_modules/.bin/bsc",
    "./node_modules/bs-platform/lib/refmt.exe",
    [],
    ""
  );
  switch result {
    | AsYouType.Success(warnings, cmt, moduleData) => {
      print_endline("Good");
    }
    | TypeError(message, cmt, moduleData) => {
      print_endline("Failed " ++ message);
    }
  };
  print_endline("Ok");
};

let cases = [
  ({|
let awesome = 10;
let x = awe<*>
|}, ["awesome"]),
  ({|
module M = {
  let awesome = 10;
};
let x = M.awe<*>
|}, ["awesome"]),
  ({|
module M = {
  let awesome = 10;
};
let x = awe<*>
|}, []),
];

cases |> List.iter(test)