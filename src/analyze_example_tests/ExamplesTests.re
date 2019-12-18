
open TestFramework;

open Analyze;

let checkExampleProject = (name, rootPath, sourcePaths, prepareCommand) => {

  // let {describe} =
  //   describeConfig
  //   |> withLifecycle(testLifecycle =>
  //       testLifecycle
  //       |> beforeAll(() => {
  //         let (stdout, stderr, success) = Util.Commands.execFull(~pwd=root, prepareCommand);
  //         print_endline("Ok did the " ++ prepareCommand ++ " in " ++ root);
  //         if (!success) {
  //           failwith("Unable to run " ++ prepareCommand ++ " in " ++ root)
  //         }
  //       })
  //     )
  //   |> build;

  let {describe} =
    describeConfig
    |> withLifecycle(testLifecycle =>
        testLifecycle
        |> beforeAll(() => {
          let (stdout, stderr, success) = Util.Commands.execFull(~pwd=rootPath, prepareCommand);
          // print_endline("Ok did the " ++ prepareCommand ++ " in " ++ rootPath);
          if (!success) {
            failwith("Unable to run " ++ prepareCommand ++ " in " ++ rootPath)
          }
          let state = {
            ...TopTypes.empty(),
            rootPath,
            rootUri: Util.Utils.toUri(rootPath),
          };
          state
        })
      )
    |> build;

  let files = sourcePaths->Belt.List.map(path => {
    // print_endline("  Source directory " ++ path);
    Files.collect(path, FindFiles.isSourceFile)
  })->Belt.List.toArray->Belt.List.concatMany;

  describe("Examples Test " ++ name, ({test}) => {
    files->Belt.List.forEach(path => {
      let uri = Utils.toUri(path);
      test(uri, ({expect, env: state}) => {
        switch (Packages.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
          | Error(message) =>
            expect.string("Unable to get package: " ++ message).toEqual("")
            // print_endline("  Unable to get package: " ++ uri)
            // print_endline(message);
            // [`PackageFail(uri, message), ...failures]
          | Ok(package) => switch (State.getCompilationResult(uri, state, ~package)) {
            | Error(message) =>
              expect.string("Invalid compilation result: " ++ message).toEqual("")
              // print_endline("  Invalid compilation result: " ++ message);
              // [`FileFail(uri, message), ...failures]
            | Ok(Success(_)) =>
              expect.bool(true).toBeTrue()
              // print_endline("  Good: " ++ uri);
              // failures
            | Ok(TypeError(message, _) | SyntaxError(message, _, _)) =>
              expect.string("Error compiling: " ++ message).toEqual("")
              // print_endline("  Error compiling: " ++ uri);
              // [`FileFail(uri, message)]
          }
        }
      })
    })
  })

  // print_endline("  Project directory in " ++ rootPath);
  // files->Belt.List.reduce([], (failures, path) => {
  // })
};

let esyProjects = [
  ("dune", ["src", "both"], "esy"),
  ("dune-complex", ["src", "both", "awesome"], "esy"),
  ("example-esy-dune-project", ["lib", "bin"], "esy"),
];

// Don't run the esy examples on windows, they're failing right now.
let projects = (Sys.os_type == "Unix" ? esyProjects : []) @ [
  ("example-project", ["src"], "npm install"),
  ("example-es6-imports", ["src"], "npm install"),
  ("example-react", ["src", "__tests__"], "npm install"),
  ("name_with_underscore", ["src"], "npm install"),
  ("bs-3.1.5", ["src"], "npm install"),
];

// let main = (baseDir, _verbose, args) => {
//   print_endline("Checking each example project to make sure we can analyze each source file...");
let baseDir = Sys.getcwd();
  projects->Belt.List.forEach(((rootName, sourcePaths, prepareCommand)) => {
    // if (args == [] || List.mem(root, args)) {
      describe("ExamplesTests " ++ rootName, ({test}) => {
        let root = Filename.concat(baseDir, Filename.concat("examples", rootName));

        // print_endline("\027[43;30m# Example " ++ root ++ "\027[0m");
        // print_endline("Running \027[32m" ++ prepareCommand ++ "\027[0m in " ++ root);
        // test("Setup Project", ({expect}) => {
        //   expect.bool(success).toBeTrue()
        // });
        // if (!success) {
        //   [`Project(root, prepareCommand, stdout @ stderr), ...failures]
        // } else {
          let sourcePaths = sourcePaths->Belt.List.map(Filename.concat(root));
          // sourcePaths->Belt.List.forEach(sourcePath => {
          //   describe(sourcePath, ({test}) => checkExampleProject(root, sourcePath))
          // })
          checkExampleProject(rootName, root, sourcePaths, prepareCommand)
        // }
      })
    // } else {
    //   failures
    // }
  })
// };

// let (verbose, args) = Belt.List.reduce(Belt.List.fromArray(Sys.argv)->List.tl, (false, []), ((verbose, args), arg) => switch arg {
//   | "-v" | "--verbose" => (true, args)
//   | _ => (verbose, [arg, ...args])
// });

// if (verbose) {
//   Util.Log.spamError := true;
// };
// let failures = main(Sys.getcwd(), verbose, List.rev(args));

// if (failures == []) {
//   print_endline("\n\027[42;30m## All clear! ##\027[0m");
//   exit(0);
// } else {
//   print_endline("\n\027[41;30m## Tests failed! ##\027[0m");
//   failures->Belt.List.forEach(failure => switch failure {
//     | `Project(root, prepareCommand, output) =>
//       print_endline("- Project prepare failed: " ++ root ++ " : " ++ prepareCommand);
//       print_endline("    " ++ String.concat("\n    ", output));
//     | `PackageFail(uri, message) => print_endline("- Failed to get package for " ++ uri ++ " : " ++ message)
//     | `FileFail(uri, message) => print_endline("- Failed to get compilation info for " ++ uri ++ " : " ++ message)
//   });
//   exit(10)
// }