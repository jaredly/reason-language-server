
open Infix;
open SharedTypes;

let name = "TestHover";

let showPos = ((l, c)) => string_of_int(l) ++ ", " ++ string_of_int(c);

let getOutput = (files, mainFile) => {
  let (files, text, waypoints) = TestUtils.combinedWaypoints(files, mainFile);
  let (state, package, _, _) = TestUtils.setUp(files, text);
  let num = List.length(waypoints);

  /* TODO refactor this bunch out */
  package.localModules |. Belt.List.forEach((modname) => {
    let%opt_force paths = Utils.maybeHash(package.pathsForModule, modname);
    let%opt_force src = SharedTypes.getSrc(paths);
    let%try_force result = State.getCompilationResult(Utils.toUri(src), state, ~package);
    switch paths {
      | IntfAndImpl(_, _, _, Some(impl)) =>
        let%try_force res = State.getCompilationResult(Utils.toUri(impl), state, ~package);
        switch (res) {
          | TypeError(text, _) =>
            print_endline("Module: " ++ modname);
            print_endline(text);
            failwith("Local module failed to compile")
          | SyntaxError(text, _, _) =>
            print_endline(text);
            failwith("Local module syntax error")
          | _ => ()
        }
      | _ => ()
    };
    switch result {
      | Success(_, {extra}) => Log.log(SharedTypes.showExtra(extra));
      | TypeError(text, _) => {
        print_endline("Module: " ++ modname);
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
      /* Log.log("Curi " ++ curi);
      let (turi, target, tpos) = List.assoc("t" ++ string_of_int(i), waypoints); */
      /* let%opt_force (_, moduleData) = Hashtbl.find(state.compiledDocuments, curi) |> AsYouType.getResult; */
      let%try_force {SharedTypes.file, extra} = State.getCompilationResult(curi, state, ~package) |> State.tryExtra;
      string_of_int(i) ++ ": " ++ switch (References.locForPos(~extra, cpos)) {
        | None => "No loc recorded at " ++ showPos(cpos)
        | Some((_, loc)) => switch (
          Hover.newHover(
            ~rootUri=state.rootUri,
            ~file,
            ~extra,
            ~getModule=State.fileForModule(state, ~package),
            ~markdown=false,
            ~showPath=true,
            loc
          )
        /* References.definitionForLoc(
          ~pathsForModule=package.pathsForModule,
          ~file=file,
          ~getUri=State.fileForUri(state, ~package),
          ~getModule=State.fileForModule(state, ~package),
          loc,
        ) */
      ) {
        | None => "Couldn't find a hover for the loc at " ++ showPos(cpos) ++ " " ++ curi
        | Some(string) => {
          Str.global_replace(Str.regexp_string("\n"), "\n   ", string)
        }
      }}
  };
  let rec loop = i => i > num ? [] : {
    let res = process(i);
    [res, ...loop(i + 1)]
  };
  num == 0 ? "NOPE" : loop(1) |> String.concat("\n")




  /* let (text, offset, pos) = TestUtils.extractPosition(text);

  let (state, package, cmt, full) = TestUtils.setUp(files, text)
  let completions = switch (PartialParser.findCompletable(text, offset)) {
  | Nothing => failwith("Nothing completable found")
  | Labeled(string) => failwith("Can't do labeled completions yet")
  | Lident(string) =>
    Log.log("Complete: " ++ string);
    let parts = Str.split(Str.regexp_string("."), string);
    let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
    let rawOpens = PartialParser.findOpens(text, offset);
    let useMarkdown = !state.settings.clientNeedsPlainText;
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

  if (completions == []) {
    "🛑  no completions found"
  } else {
    completions |> List.map(((uri, item)) => {
      item.name.txt
      ++ "\n- path: " ++ uri
      ++ "\n> " ++ (Str.split(Str.regexp_string("\n"), NewCompletions.detail(item.name.txt, item.contents)) |> String.concat("\n> "))
    }) |> String.concat("\n");
  } */
};

