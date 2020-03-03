
open TestFramework;
open Relite.Types;

open Analyze;
open TestFramework;

let checkExampleProject = (describe, name, rootPath, sourcePaths, prepareCommand) => {
  let files =
    sourcePaths
    ->Belt.List.map(path => {Files.collect(path, FindFiles.isSourceFile)})
    ->Belt.List.toArray
    ->Belt.List.concatMany;

  describe.withLifecycle(
    "Examples Test " ++ name,
    Relite.Suite.beforeAll(() => {
      let (stdout, stderr, success) = Util.Commands.execFull(~pwd=rootPath, prepareCommand);
      // print_endline("Ok did the " ++ prepareCommand ++ " in " ++ rootPath);
      if (!success) {
        failwith("Unable to run " ++ prepareCommand ++ " in " ++ rootPath);
      };
      let state = {...TopTypes.empty(), rootPath, rootUri: Util.Utils.toUri(rootPath)};
      state;
    }),
    ({it}) => {
    files->Belt.List.forEach(path => {
      let uri = Utils.toUri(path);
      it(uri, ({expect, ctx: state}) => {
        switch (Packages.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
        | Error(message) => failwith("Unable to get package: " ++ message)
        // print_endline("  Unable to get package: " ++ uri)
        // print_endline(message);
        // [`PackageFail(uri, message), ...failures]
        | Ok(package) =>
          switch (State.getCompilationResult(uri, state, ~package)) {
          | Error(message) => failwith("Invalid compilation result: " ++ message)
          // print_endline("  Invalid compilation result: " ++ message);
          // [`FileFail(uri, message), ...failures]
          | Ok(Success(_)) => ()
          // print_endline("  Good: " ++ uri);
          // failures
          | Ok(TypeError(message, _) | SyntaxError(message, _, _)) =>
            failwith("Error compiling: " ++ message)
          // print_endline("  Error compiling: " ++ uri);
          // [`FileFail(uri, message)]
          }
        }
      });
    })
  });
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
  ("bs-7.2", ["src"], "npm install"),
];

// let main = (baseDir, _verbose, args) => {
//   print_endline("Checking each example project to make sure we can analyze each source file...");
let baseDir = Sys.getcwd();
  projects->Belt.List.forEach(((rootName, sourcePaths, prepareCommand)) => {
    // if (args == [] || List.mem(root, args)) {
      describe.plain("ExamplesTests " ++ rootName, ({describe}) => {
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
          checkExampleProject(describe, rootName, root, sourcePaths, prepareCommand)
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