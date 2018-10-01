
open Analyze;

let checkExampleProject = (rootPath, sourcePaths) => {
  let state = {
    ...TopTypes.empty(),
    rootPath,
    rootUri: Util.Utils.toUri(rootPath),
  };
  print_endline("  Project directory in " ++ rootPath);
  let files = sourcePaths->Belt.List.map(path => {
    print_endline("  Source directory " ++ path);
    Files.collect(path, FindFiles.isSourceFile)
  })->Belt.List.toArray->Belt.List.concatMany;
  files->Belt.List.reduce([], (failures, path) => {
    let uri = Utils.toUri(path);
    switch (State.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
      | Error(message) =>
        print_endline("  Unable to get package: " ++ uri)
        print_endline(message);
        [`PackageFail(uri, message), ...failures]
      | Ok(package) => switch (State.getCompilationResult(uri, state, ~package)) {
        | Error(message) => 
          print_endline("  Invalid compilation result: " ++ message);
          [`FileFail(uri, message), ...failures]
        | Ok(result) =>
          print_endline("  Good: " ++ uri);
          failures
      }
    }
  })
};

let projects = [
  ("example-project", ["src"], "npm install"),
  ("example-es6-imports", ["src"], "npm install"),
  ("example-react", ["src", "__tests__"], "npm install"),
  ("name_with_underscore", ["src"], "npm install"),
  ("bs-3.1.5", ["src"], "npm install"),
  ("example-esy-dune-project", ["lib", "bin"], "esy"),
];


let main = (baseDir, verbose, args) => {
  print_endline("Checking each example project to make sure we can analyze each source file...");
  projects->Belt.List.reduce([], (failures, (root, sourcePaths, prepareCommand)) => {
    if (args == [] || List.mem(root, args)) {
      print_endline("\027[43;30m# Example " ++ root ++ "\027[0m");
      let root = Filename.concat(baseDir, Filename.concat("examples", root));
      print_endline("Running \027[32m" ++ prepareCommand ++ "\027[0m in " ++ root);
      let (stdout, stderr, success) = Util.Commands.execFull(~pwd=root, prepareCommand);
      if (!success) {
        [`Project(root, prepareCommand, stdout @ stderr), ...failures]
      } else {
        let sourcePaths = sourcePaths->Belt.List.map(Filename.concat(root));
        checkExampleProject(root, sourcePaths) @ failures
      }
    } else {
      failures
    }
  })
};

let (verbose, args) = Belt.List.reduce(Belt.List.fromArray(Sys.argv)->List.tl, (false, []), ((verbose, args), arg) => switch arg {
  | "-v" | "--verbose" => (true, args)
  | _ => (verbose, [arg, ...args])
});
if (verbose) {
  Util.Log.spamError := true;
};
let failures = main(Sys.getcwd(), verbose, List.rev(args));

if (failures == []) {
  print_endline("\n\027[42;30m## All clear! ##\027[0m");
  exit(0);
} else {
  print_endline("\n\027[41;30m## Tests failed! ##\027[0m");
  failures->Belt.List.forEach(failure => switch failure {
    | `Project(root, prepareCommand, output) =>
      print_endline("- Project prepare failed: " ++ root ++ " : " ++ prepareCommand);
      print_endline("    " ++ String.concat("\n    ", output));
    | `PackageFail(uri, message) => print_endline("- Failed to get package for " ++ uri ++ " : " ++ message)
    | `FileFail(uri, message) => print_endline("- Failed to get compilation info for " ++ uri ++ " : " ++ message)
  });
  exit(10)
}