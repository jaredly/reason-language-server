
open Analyze;

let checkExampleProject = (rootPath, sourcePaths) => {
  let state = {
    ...TopTypes.empty(),
    rootPath,
    rootUri: Util.Utils.toUri(rootPath),
  };
  print_endline("Looking in " ++ rootPath);
  let files = sourcePaths->Belt.List.map(path => {
    print_endline("Source " ++ path);
    Files.collect(path, FindFiles.isSourceFile)
  })->Belt.List.toArray->Belt.List.concatMany;
  files->Belt.List.forEach(path => {
    let uri = Utils.toUri(path);
    switch (State.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
      | Error(message) => print_endline("Unable to get package: " ++ uri)
        print_endline(message)
      | Ok(package) => switch (State.getCompilationResult(uri, state, ~package)) {
        | Error(message) => print_endline("Invalid compilation result: " ++ message)
        | Ok(result) => print_endline("Good: " ++ uri)
      }
    }
  })
};

let projects = [
  ("example-project", ["src"]),
  ("example-es6-imports", ["src"]),
  ("example-react", ["src"]),
  ("bs-3.1.5", ["src"]),
  ("example-esy-dune-project", ["lib", "bin"]),
];


let main = (baseDir) => {
  print_endline("Checking examples");

  projects->Belt.List.forEach(((root, sourcePaths)) => {
    print_endline("Ok " ++ root);
    let root = Filename.concat(baseDir, Filename.concat("examples", root));
    let sourcePaths = sourcePaths->Belt.List.map(Filename.concat(root));
    checkExampleProject(root, sourcePaths)
  })
}
