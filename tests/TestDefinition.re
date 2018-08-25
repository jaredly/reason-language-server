
let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

Log.spamError := true;
Printexc.record_backtrace(true);

let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let testFile = "./tests/TestDefinition.txt";
let lines = Files.readFileExn(testFile) |> Utils.splitLines;
let output = TestUtils.process(lines, (files, mainFile) => {
  let (files, text, waypoints) = TestUtils.combinedWaypoints(files, mainFile);
  let (state, package, _, _) = TestUtils.setUp(files, text);
  let num = List.length(waypoints) / 2;
  package.localModules |. Belt.List.forEach(((a, (src, ex))) => {
      let%try_force {SharedTypes.file, extra} = State.getDefinitionData(Utils.toUri(ex), state, ~package);
      Log.log(SharedTypes.showExtra(extra));
  });
  let process = i => {
      let (curi, cursor, cpos) = List.assoc("c" ++ string_of_int(i), waypoints);
      Log.log("Curi " ++ curi);
      let (turi, target, tpos) = List.assoc("t" ++ string_of_int(i), waypoints);
      /* let%opt_force (_, moduleData) = Hashtbl.find(state.compiledDocuments, curi) |> AsYouType.getResult; */
      let%try_force {SharedTypes.file, extra} = State.getCompilationResult(curi, state, ~package) |> State.tryExtra;
      string_of_int(i) ++ ": " ++ switch (
        References.definitionForPos(
          ~package,
          ~file=file,
          ~extra=extra,
          ~getModule=State.fileForModule(state, ~package),
          cpos,
        )
      ) {
        | None => "Couldn't find a definition for " ++ showPos(cpos) ++ " " ++ curi
        | Some((uri, {loc_start: {pos_cnum}, loc_end: {pos_cnum: cend}})) => {
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