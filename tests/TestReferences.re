
let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

Log.spamError := true;
Printexc.record_backtrace(true);

let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let testFile = "./tests/TestReferences.txt";
let lines = Files.readFileExn(testFile) |> Utils.splitLines;
let output = TestUtils.process(lines, (files, mainFile) => {
  let (files, text, waypoints) = TestUtils.combinedWaypoints(files, mainFile);
  let (state, package, cmt, _) = TestUtils.setUp(files, text);

  let fileNames = files |. Belt.List.map(fst);
  let fileNames = ["Test.re", ...fileNames];
  let fileData = fileNames |. Belt.List.map((name) => {
    let moduleName = Filename.chop_extension(name) |. String.capitalize;
    let uri = TestUtils.uriForName(name);
    open Infix;
    let%opt_force (cmt, _) = State.getCompilationResult(uri, state, ~package) |> AsYouType.getResult;
    let%opt_force file = ProcessCmt.forCmt(uri, x => x, cmt);
    let%opt_force extra = ProcessExtra.forCmt(~file, cmt);

    print_newline();
    Log.log(uri);
    Log.log(SharedTypes.showExtra(extra));

    (moduleName, (uri, cmt, file, extra))
  });

  let allModules = fileData |. Belt.List.map(((name, _)) => name);
  let getModule = moduleName => {
    let%opt (_, _, file, _) = Belt.List.getAssoc(fileData, moduleName, (==));
    Some(file)
  };
  let getExtra = moduleName => {
    let%opt (_, _, _, extra) = Belt.List.getAssoc(fileData, moduleName, (==));
    Some(extra)
  };

  /* let%opt_force file = ProcessCmt.forCmt("file://hello.re", x => x, cmt);
  let%opt_force extra = ProcessExtra.forCmt(~file, cmt); */

  let num = List.length(waypoints) / 2;
  let process = (i, curi, cursor, cpos) => {
      /* let (curi, cursor, cpos) = List.assoc("c" ++ string_of_int(i), waypoints); */
      let targets = List.filter(((name, contents)) => name == "t" ++ string_of_int(i), waypoints);
      /* let (turi, target, tpos) = List.assoc("t" ++ string_of_int(i), waypoints); */

      let%opt_force (cmt, moduleData) = Hashtbl.find(state.compiledDocuments, curi) |> AsYouType.getResult;
      let%opt_force file = ProcessCmt.forCmt(curi, x => x, cmt);
      let%opt_force extra = ProcessExtra.forCmt(~file, cmt);

      /* let (line, char) = cpos; */

      switch (References.locForPos(~extra, cpos)) {
        | None => "  " ++ string_of_int(i) ++ " - no location at pos " ++ showPos(cpos)
        | Some(loc) => {
          "  " ++ string_of_int(i) ++ ": " ++ switch (References.forLoc(
            ~file,
            ~extra,
            ~allModules,
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
}) |> String.concat("\n");
Files.writeFileExn(testFile, output);