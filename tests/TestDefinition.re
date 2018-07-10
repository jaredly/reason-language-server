/* 
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
}; */

let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

Log.spamError := true;
Printexc.record_backtrace(true);

let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let testFile = "./tests/Definition.txt";
let lines = Files.readFileExn(testFile) |> Utils.splitLines;
let output = TestUtils.process(lines, (files, mainFile) => {
  let (text, waypoints) = TestUtils.extractWaypoints(mainFile);
  let waypoints = waypoints |> List.map(((tag, offset, pos)) => (tag, (TestUtils.uriForName("Test.re"), offset, pos)));
  let (files, otherWaypoints) = TestUtils.extractAllWaypoints(files);
  let (state, package, moduleData) = TestUtils.setUp(files, text);
  let waypoints = waypoints @ otherWaypoints;
  let num = List.length(waypoints) / 2;
  let process = i => {
      let (curi, cursor, cpos) = List.assoc("c" ++ string_of_int(i), waypoints);
      let (turi, target, tpos) = List.assoc("t" ++ string_of_int(i), waypoints);
      let%opt_force (_, moduleData) = Hashtbl.find(state.compiledDocuments, curi) |> AsYouType.getResult;
      string_of_int(i) ++ ": " ++ switch (State.definitionForPos(
        curi,
        cpos, moduleData, state, ~package)
      ) {
        | None => "No definition! at " ++ showPos(cpos)
        | Some((_, None, _, _)) => "No location"
        | Some((_, _, _, None)) => "No uri"
        | Some((_, Some({loc_start: {pos_cnum}, loc_end: {pos_cnum: cend}}), _, Some(uri))) => {
          if (uri != turi) {
            "FAIL wrong uri " ++ uri ++ " expected " ++ turi
          } else if (pos_cnum != target) {
            Printf.sprintf("FAIL wrong position %d-%d expected %d", pos_cnum, cend, target)
          } else {
            "PASS"
          }
        }
      }
  };
  let rec loop = i => i > num ? [] : {
    let res = process(i);
    [res, ...loop(i + 1)]
  };
  num == 0 ? "NOPE" : loop(1) |> String.concat("\n")
}) |> String.concat("\n");
Files.writeFileExn(testFile, output);