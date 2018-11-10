open Analyze;
let tmp = "/tmp/.lsp-test";
Files.mkdirp(tmp);

let getPackage = (localModules) => {
  let buildSystem = BuildSystem.Dune(Esy("0.3.4"));
  let%try refmtPath = BuildSystem.getRefmt(".", buildSystem);
  let%try_wrap compilerPath = BuildSystem.getCompiler(".", buildSystem);
  let (pathsForModule, nameForPath) = State.makePathsForModule(localModules, []);
  {
    TopTypes.basePath: tmp,
    localModules: localModules |. Belt.List.map(fst),
    dependencyModules: [],
    pathsForModule,
    nameForPath,
    namespace: None,
    buildSystem,
    buildCommand: None,
    interModuleDependencies: Hashtbl.create(0),
    opens: [],
    tmpPath: tmp,
    compilationFlags: "",
    compilerVersion: BuildSystem.V406,
    rebuildTimer: 0.,
    includeDirectories: [],
    compilerPath,
    refmtPath: Some(refmtPath),
    lispRefmtPath: None,
  };
};

let offsetToPos = (text, offset) => {
  let pre = String.sub(text, 0, offset);
  if (pre == "") {
    (1, 0)
  } else {

    let lines = Str.split(Str.regexp_string("\n"), pre);
    let last = List.hd(List.rev(lines));
    (List.length(lines), String.length(last))
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
  (files |> List.rev, text, waypoints)
};

let getState = () => {
  let packagesByRoot = Hashtbl.create(1);

  TopTypes.{
    rootPath: tmp,
    rootUri: "file:///path/to",
    documentText: Hashtbl.create(5),
    documentTimers: Hashtbl.create(10),
    packagesByRoot,
    rootForUri: Hashtbl.create(30),
    cmtCache: Hashtbl.create(30),
    cmiCache: Hashtbl.create(30),
    compiledDocuments: Hashtbl.create(10),
    lastDefinitions: Hashtbl.create(10),
    settings: {
      crossFileAsYouType: false,
      refmtLocation: None,
      lispRefmtLocation: None,
      formatWidth: None,
      perValueCodelens: false,
      opensCodelens: true,
      dependenciesCodelens: true,
      clientNeedsPlainText: false,
      showModulePathOnHover: false,
      recordAllLocations: false,
      autoRebuild: false,
    },
  };
};

let makeFilesList = files => {
  open Belt;
  let interfaces = Hashtbl.create(10);
  files |. List.forEach(((name, _)) => {
    let mname = Filename.chop_extension(name);
    if (Filename.check_suffix(name, "i")) {
      Hashtbl.replace(interfaces, mname, name)
    }
  });
  let srcBase = "/path/to/";
  let cmtBase = "/tmp/.lsp-test/";
  let normals = files |. List.keepMap(((name, _)) => {
    let mname = Filename.chop_extension(name);
    if (!Filename.check_suffix(name, "i")) {
      let intf = Utils.maybeHash(interfaces, mname);
      Hashtbl.remove(interfaces, mname);
      Some((mname, switch intf {
        | None => SharedTypes.Impl(cmtBase ++ mname ++ ".cmt", Some(srcBase ++ name))
        | Some(iname) =>
        SharedTypes.IntfAndImpl(cmtBase ++ mname ++ ".cmti", Some(srcBase ++ iname), cmtBase ++ mname ++ ".cmt", Some(srcBase ++ name))
      }))
    } else {
      None
    }
  });
  normals |. List.concat(Hashtbl.fold((mname, iname, res) =>  {
  [(mname, SharedTypes.Intf(cmtBase ++ mname ++ ".cmti", Some(srcBase ++ iname))), ...res]
}, interfaces, []))
};

let setUp = (files, text) => {
  let state = getState();
  let%try_force package = getPackage(
    /* (files |> List.map(((name, _)) => (
      Filename.chop_extension(name),
      TopTypes.Impl("/tmp/.lsp-test/" ++ Filename.chop_extension(name) ++ ".cmt", Some("/path/to/" ++ name))
    ))) */
    makeFilesList(files)
    @ [("Test", Impl("/tmp/.lsp-test/Test.cmt", Some("/path/to/Test.re")))]
  );

  files |> List.iter(((name, contents)) => {
    /* print_endline("Compiling " ++ name); */
    let moduleName = Filename.chop_extension(name);
    let uri = uriForName(name);
    let%try_force result = AsYouType.process(
      ~uri,
      ~moduleName,
      ~basePath=".",
      ~reasonFormat=false,
      ~allLocations=false,
      ~compilerVersion=package.compilerVersion,
      contents,
      ~cacheLocation=tmp,
      package.compilerPath,
      package.refmtPath,
      [tmp],
      ""
    );
    switch result {
      | AsYouType.SyntaxError(text, _, _) => failwith("Syntax error " ++ text)
      | AsYouType.TypeError(text, _) => failwith("Type error " ++ text)
      | _ => ()
    };
    let moduleData = AsYouType.getResult(result);
    Hashtbl.replace(state.compiledDocuments, uri, result);
    Hashtbl.replace(state.lastDefinitions, uri, moduleData);

    /* switch result {
      | AsYouType.SyntaxError(syntaxError, _, full) => Log.log("Syntax error! " ++ syntaxError)
      | TypeError(errorText, full) => Log.log("Type Error: " ++ errorText)
      | _ => ()
    }; */
  });

  let%try_force result = AsYouType.process(
    ~uri=mainUri,
    ~moduleName="Test",
    ~basePath=".",
    ~reasonFormat=false,
    ~compilerVersion=package.compilerVersion,
    ~allLocations=false,
    text,
    ~cacheLocation=tmp,
    package.compilerPath,
    package.refmtPath,
    [tmp],
    ""
  );
  /* switch result {
    | AsYouType.SyntaxError(syntaxError, _, full) => Log.log("Syntax error! " ++ syntaxError)
    | TypeError(errorText, full) => Log.log("Type Error: " ++ errorText)
    | _ => ()
  }; */

  let moduleData = AsYouType.getResult(result);
  Hashtbl.replace(state.compiledDocuments, mainUri, result);
  Hashtbl.replace(state.lastDefinitions, mainUri, moduleData);

  (state, package, (), moduleData)
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
