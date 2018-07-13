
let logDest = Filename.concat(Filename.get_temp_dir_name(), "lsp-test.log");
Log.setLocation(logDest);

Log.spamError := true;
Printexc.record_backtrace(true);

let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let testFile = "./tests/TestReferences.txt";
let lines = Files.readFileExn(testFile) |> Utils.splitLines;
let output = TestUtils.process(lines, (files, mainFile) => {
  let (files, text, waypoints) = TestUtils.combinedWaypoints(files, mainFile);
  let (state, _package, cmt, _) = TestUtils.setUp(files, text);

  let%opt_force file = ProcessCmt.forCmt("file://hello.re", x => x, cmt);
  let%opt_force extra = ProcessExtra.forCmt(~file, cmt);

  Log.log(SharedTypes.showExtra(extra));

  let num = List.length(waypoints) / 2;
  let process = (i, curi, cursor, cpos) => {
      /* let (curi, cursor, cpos) = List.assoc("c" ++ string_of_int(i), waypoints); */
      let targets = List.filter(((name, contents)) => name == "t" ++ string_of_int(i), waypoints);
      /* let (turi, target, tpos) = List.assoc("t" ++ string_of_int(i), waypoints); */
      let%opt_force (cmt, moduleData) = Hashtbl.find(state.compiledDocuments, curi) |> AsYouType.getResult;

      let%opt_force file = ProcessCmt.forCmt("file://hello.re", x => x, cmt);
      let%opt_force extra = ProcessExtra.forCmt(~file, cmt);

      let (line, char) = cpos;

      "  " ++ string_of_int(i) ++ ": " ++ switch (References.forPos(
        ~extra,
        ~getModule=0,
        (line + 1, char))) {
        | None => "No definition! at " ++ showPos(cpos)
        | Some(allReferences) =>
          let targetValues = targets |> List.map(((a, b)) => b);
          let found = allReferences |> List.map(((uri, refs)) => {
            refs |> List.map((({Location.loc_start: {pos_cnum, pos_lnum, pos_bol}})) => (
              uri, pos_cnum, (pos_lnum - 1, pos_cnum - pos_bol)
            ))
          }) |> List.concat;
          let extra = targetValues |> List.filter(t => !List.mem(t, found));
          let unexpected = found |> List.filter(((uri, off, _) as t) => !List.mem(t, targetValues) && (uri != curi || off != cursor));
          if (extra == [] && unexpected == []) {
            "PASS"
          } else {
            "FAIL\n" ++ (
              extra |> List.map(((uri, off, (l, c))) => Printf.sprintf(
                "    not found - %s %d (%d, %d)\n" , uri, off, l, c
              )) |> String.concat("")
            ) ++ (
              unexpected |> List.map(((uri, off, (l, c))) => Printf.sprintf(
                "    extra ref - %s %d (%d, %d)\n" , uri, off, l, c
              )) |> String.concat("")
            )
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