let tmp = "/tmp/.lsp-test";
Files.mkdirp(tmp);

let getPackage = (localModules) => {
  {
    State.basePath: tmp,
    localModules,
    dependencyModules: [],
    pathsForModule: State.makePathsForModule(localModules, []),
    buildSystem: BuildSystem.Bsb("3.2.0"),
    opens: [],
    tmpPath: tmp,
    compilationFlags: "",
    includeDirectories: [],
    compilerPath: "./node_modules/.bin/bsc",
    refmtPath: "./node_modules/bs-platform/lib/refmt.exe",
  };
  
};

let offsetToPos = (text, offset) => {
  let pre = String.sub(text, 0, offset);
  if (pre == "") {
    (0, 0)
  } else {

    let lines = Str.split(Str.regexp_string("\n"), pre);
    let last = List.hd(List.rev(lines));
    (List.length(lines) - 1, String.length(last))
  }
};

let extractPosition = text => {
  let clean = Str.substitute_first(Str.regexp_string("<*>"), x => "", text);
  let char = Str.match_beginning();
  (clean, char, offsetToPos(text, char))
};

let extractWaypoints = text => {
  let waypoints = ref([]);
  let globalOffset = ref(0);
  let clean = Str.global_substitute(Str.regexp("<[^>]+>"), text => {
    let match = Str.matched_string(text);
    let offset = Str.match_beginning() - globalOffset^;
    waypoints := [(match |> String.sub(_, 1, String.length(match) - 2), offset), ...waypoints^];
    globalOffset := globalOffset^ + String.length(match);
    ""
  }, text);
  (clean, waypoints^ |> List.map(((name, offset)) => (name, offset, offsetToPos(clean, offset))))
};

let mainUri = "file:///path/to/Test.re";
let mainPath = "/path/to/Test.re";

let uriForName = name => "file:///path/to/" ++ name;

let extractAllWaypoints = files => {
  files |> List.fold_left(((files, otherpoints), (name, contents)) => {
    let (clean, waypoints) = extractWaypoints(contents);
    ([(name, clean), ...files], List.map(((tag, offset, pos)) => (tag, (uriForName(name), offset, pos)), waypoints) @ otherpoints)
  }, ([], []))
};

let combinedWaypoints = (files, mainFile) => {
  let (text, waypoints) = extractWaypoints(mainFile);
  let waypoints = waypoints |> List.map(((tag, offset, pos)) => (tag, (uriForName("Test.re"), offset, pos)));
  let (files, otherWaypoints) = extractAllWaypoints(files);
  let waypoints = waypoints @ otherWaypoints;
  (files, text, waypoints)
};

let getState = () => {
  let packagesByRoot = Hashtbl.create(1);

  State.{
    rootPath: tmp,
    rootUri: "file://" ++ tmp,
    documentText: Hashtbl.create(5),
    documentTimers: Hashtbl.create(10),
    packagesByRoot,
    rootForUri: Hashtbl.create(30),
    cmtCache: Hashtbl.create(30),
    cmiCache: Hashtbl.create(30),
    compiledDocuments: Hashtbl.create(10),
    lastDefinitions: Hashtbl.create(10),
    settings: {
      perValueCodelens: false,
      opensCodelens: true,
      dependenciesCodelens: true,
      clientNeedsPlainText: false,
    },
  };
};

let setUp = (files, text) => {
  let state = getState();
  let package = getPackage([
    ("Test", ("/tmp/.lsp-test/Test.cmt", "/path/to/Test.re")),
    ...(files |> List.map(((name, _)) => (
      Filename.chop_extension(name),
      ("/tmp/.lsp-test/" ++ Filename.chop_extension(name) ++ ".cmt", "/path/to/" ++ name)
    )))
  ]);

  files |> List.iter(((name, contents)) => {
    let moduleName = Filename.chop_extension(name);
    /* print_endline("Compiling " ++ moduleName); */
    let%try_force result = AsYouType.process(
      ~moduleName,
      contents,
      ~cacheLocation=tmp,
      "./node_modules/.bin/bsc",
      "./node_modules/bs-platform/lib/refmt.exe",
      [tmp],
      ""
    );
    let (cmt, moduleData) = switch result {
      | AsYouType.Success(warnings, cmt, moduleData) => {
        (cmt, moduleData)
      }
      | TypeError(message, cmt, moduleData) => {
        print_endline("Failed to compile supporting file " ++ name ++ message);
        (cmt, moduleData)
      }
    };
    let uri = uriForName(name);
    Hashtbl.replace(state.compiledDocuments, uri, result);
    Hashtbl.replace(state.lastDefinitions, uri, moduleData)
  });

  let%try_force result = AsYouType.process(
    ~moduleName="Test",
    text,
    ~cacheLocation=tmp,
    "./node_modules/.bin/bsc",
    "./node_modules/bs-platform/lib/refmt.exe",
    [tmp],
    ""
  );

  let (cmt, moduleData) = switch result {
    | AsYouType.Success(warnings, cmt, moduleData) => {
      /* print_endline("Good"); */
      (cmt, moduleData)
    }
    | TypeError(message, cmt, moduleData) => {
      /* print_endline("Failed " ++ message); */
      (cmt, moduleData)
    }
  };
  Hashtbl.replace(state.compiledDocuments, mainUri, result);
  Hashtbl.replace(state.lastDefinitions, mainUri, moduleData);

  (state, package, cmt, moduleData)
};

let iterTests = (lines, iter) => {
  let sections = TestParser.parseSections(lines);
  let hasOnly = Belt.List.some(sections, s => switch s {
    | `Test(name, _, _, _) => Utils.startsWith(name, "*")
    | _ => false
  });
  sections |> List.iter(section => switch section {
    | `Test(name, mainFile, files, result) => {
      if (!hasOnly || Utils.startsWith(name, "*")) {
        iter(name, mainFile, files, result)
      }
    }
    | _ => ()
  })
};

let process = (lines, getResult) => {
  let sections = TestParser.parseSections(lines);
  let hasOnly = Belt.List.some(sections, s => switch s {
    | `Test(name, _, _, _) => Utils.startsWith(name, "*")
    | _ => false
  });
  sections |> List.map(section => switch section {
    | `Header(name) => "### " ++ name ++ "\n"
    | `Test(name, mainFile, files, result) => {
      if (hasOnly && !Utils.startsWith(name, "*")) {
        "=== " ++ name ++ "\n" ++ TestParser.printFiles(mainFile, files) ++ "\n"
        ++ (result == [] ? "" : "-->\n" ++ String.concat("\n", result))
      } else {
        print_endline("-----[ " ++ name ++ " ]-----");
        /* print_endline("Running " ++ name); */
        /* files |> List.iter(((name, _)) => print_endline("File: " ++ name)); */
        let newResult = getResult(files, mainFile);
        "=== " ++ name ++ "\n" ++ TestParser.printFiles(mainFile, files) ++ "\n"
        ++ (newResult == "" ? "" : "-->\n" ++ newResult ++ "\n")
      }
    }
  })
};