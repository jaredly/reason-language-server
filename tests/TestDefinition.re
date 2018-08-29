

let name = "TestDefinition";

let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let getOutput = (files, mainFile) => {
  let (files, text, waypoints) = TestUtils.combinedWaypoints(files, mainFile);
  let (state, package, _, _) = TestUtils.setUp(files, text);
  let num = List.length(waypoints) / 2;
  package.localModules |. Belt.List.forEach((modname) => {
    let%opt_consume paths = Utils.maybeHash(package.pathsForModule, modname);
    let%opt_consume src = TopTypes.getSrc(paths);
    let%try_force result = State.getCompilationResult(Utils.toUri(src), state, ~package);
    switch result {
      | Success(_, {extra}) => Log.log(SharedTypes.showExtra(extra));
      | TypeError(text, _) => {
        print_endline(text);
        failwith("Local module failed to compile")
      }
      | SyntaxError(text, _, _) =>
        print_endline(text);
        failwith("Local module syntax error")
    }
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
          ~getUri=State.fileForUri(state, ~package),
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
};
