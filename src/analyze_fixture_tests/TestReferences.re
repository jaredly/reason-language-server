
let name = "TestReferences";
let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let getOutput = (files, mainFile) => {
  let (files, text, waypoints) = TestUtils.combinedWaypoints(files, mainFile);
  let (state, package, _, _) = TestUtils.setUp(files, text);

  let fileNames = files |. Belt.List.map(fst);
  let fileNames = ["Test.re", ...fileNames];
  let fileData = fileNames |. Belt.List.reverse |. Belt.List.map((name) => {
    /* print_endline("Check " ++ name); */
    let moduleName = Filename.chop_extension(name) |. String.capitalize;
    let uri = TestUtils.uriForName(name);
    let%try_force result = State.getCompilationResult(uri, state, ~package);
    switch result {
      | Success(_, {file, extra}) => {
        Log.log(uri);
        Log.log(SharedTypes.showExtra(extra));

        (moduleName, (uri, file, extra))
      }
      | TypeError(text, _) => {
        print_endline(text);
        failwith("Local module failed to compile")
      }
      | SyntaxError(text, _, _) =>
        print_endline(text);
        failwith("Local module syntax error")
    }

  });

  let allModules = fileData |. Belt.List.map(((name, _)) => name);
  let getModule = moduleName => {
    let%opt (_, file, _) = Belt.List.getAssoc(fileData, moduleName, (==));
    Some(file)
  };
  let getExtra = moduleName => {
    let%opt (_, _, extra) = Belt.List.getAssoc(fileData, moduleName, (==));
    Some(extra)
  };

  /* let%opt_force file = ProcessCmt.forCmt("file://hello.re", x => x, cmt);
  let%opt_force extra = ProcessExtra.forCmt(~file, cmt); */

  let num = List.length(waypoints) / 2;
  let process = (i, curi, cursor, cpos) => {
      /* let (curi, cursor, cpos) = List.assoc("c" ++ string_of_int(i), waypoints); */
      let targets = List.filter(((name, contents)) => name == "t" ++ string_of_int(i), waypoints);
      /* let (turi, target, tpos) = List.assoc("t" ++ string_of_int(i), waypoints); */

      let {SharedTypes.file, extra} = Hashtbl.find(state.compiledDocuments, curi) |> AsYouType.getResult;

      /* let (line, char) = cpos; */

      switch (References.locForPos(~extra, cpos)) {
        | None => "  " ++ string_of_int(i) ++ " - no location at pos " ++ showPos(cpos)
        | Some((_, loc)) => {
          "  " ++ string_of_int(i) ++ ": " ++ switch (References.allReferencesForLoc(
            ~pathsForModule=package.pathsForModule,
            ~file,
            ~extra,
            ~allModules,
            ~getUri=State.fileForUri(state, ~package),
            ~getModule,
            ~getExtra,
            loc
          )) {
            | Error(e) => "No definition! at " ++ showPos(cpos) ++ " message " ++ e
            | Ok(allReferences) =>
              let targetValues = targets |> List.map(((a, b)) => b);
              let found = allReferences |> List.map(((uri, refs)) => {
                refs |> List.map((({Location.loc_start: {pos_cnum, pos_lnum, pos_bol}})) => (
                  uri, pos_cnum, (pos_lnum, pos_cnum - pos_bol)
                ))
              }) |> List.concat;
              let extra = targetValues |> List.filter(t => !List.mem(t, found));
              let unexpected = found |> List.filter(((uri, off, _) as t) => !List.mem(t, targetValues) && (uri != curi || off != cursor));
              if (extra == [] && unexpected == []) {
                "✅ PASS"
              } else {
                "🔴 FAIL\n" ++ (
                  extra |> List.map(((uri, off, (l, c))) => Printf.sprintf(
                    "    missing reference - %s %d (%d, %d)\n" , uri, off, l, c
                  )) |> String.concat("")
                ) ++ (
                  unexpected |> List.map(((uri, off, (l, c))) => Printf.sprintf(
                    "    extra ref - %s %d (%d, %d)\n" , uri, off, l, c
                  )) |> String.concat("")
                )
              }
          }

        }
      }
  };
  let rec loop = i => {
    switch (List.assoc("c" ++ string_of_int(i), waypoints)) {
      | exception Not_found => []
      | (curi, cursor, cpos) => {
        let res = process(i, curi, cursor, cpos);
        [res, ...loop(i + 1)]
      }
    }
  };
  num == 0 ? "NOPE" : loop(1) |> String.concat("\n")
};
