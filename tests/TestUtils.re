let tmp = "/tmp/.lsp-test";
Files.mkdirp(tmp);

let getPackage = (localModules) => {
  {
    State.basePath: tmp,
    localModules: [],
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

let extractPosition = text => {
  let clean = Str.substitute_first(Str.regexp_string("<*>"), x => "", text);
  let char = Str.match_beginning();
  let pre = String.sub(text, 0, char);
  let lines = Str.split(Str.regexp_string("\n"), pre);
  (clean, char, (List.length(lines), String.length(List.hd(List.rev(lines)))))
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
