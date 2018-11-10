
module Helpers = Helpers;

let run = (~rootPath, ~filterPath, modify) => {
  let root = Utils.startsWith(rootPath, ".") ? Filename.concat(Sys.getcwd(), rootPath) : rootPath;
  let state = Analyze.TopTypes.empty();
  let state = {...state, settings: {...state.settings,
    recordAllLocations: true,
    autoRebuild: false,
  }};
  print_endline("Setting up a package");
  let%try_force package = Analyze.State.newPackageForRoot(~reportDiagnostics=(_, _) => (), state, root);
  let%opt_force (buildCommand, _) = package.buildCommand;
  print_endline("Running build command (for freshness) " ++ buildCommand);
  let (stdout, stderr, success) = Commands.execFull(~pwd=root, buildCommand);
  if (!success) {
    print_endline(Utils.joinLines(stdout));
    print_endline(Utils.joinLines(stderr));
    failwith("Build command " ++ buildCommand ++ " failed")
  };
  if (Utils.joinLines(stderr) |> String.trim != "") {
    print_endline(Utils.joinLines(stderr));
    failwith("Build command had stderror " ++ buildCommand)
  };

  let fullForCmt = (switch (package.compilerVersion) {
    | Analyze.BuildSystem.V402 => Process_402.fullForCmt
    | V406 => Process_406.fullForCmt
    | V407 => Process_407.fullForCmt
  })(~allLocations=true);

  let module Convert = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_404, Migrate_parsetree.OCaml_407);
  let module ConvertBack = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_407, Migrate_parsetree.OCaml_404);

  package.Analyze.TopTypes.localModules->Belt.List.forEach(moduleName => {
    let%opt_force paths = Utils.maybeHash(package.pathsForModule, moduleName);
    let%opt_consume (cmt, src) = SharedTypes.getImpl(paths);
    print_endline(src);

    if (filterPath(src, moduleName)) {
      let%try_force full = fullForCmt(~moduleName, cmt, src, x => x);
      let ctx = {Helpers.state, package, full};

      let file_chan = open_in(src);
      seek_in(file_chan, 0);
      let lexbuf = Lexing.from_channel(file_chan);
      let (structure, comments) = Reason_toolchain.RE.implementation_with_comments(lexbuf);
      close_in(file_chan);
      let structure = Convert.copy_structure(structure);

      /* let%try_force structure = Process_406.parseTreeForCmt(cmt); */
      let newStructure = modify(ctx, structure);

      if (newStructure != structure) {
        print_endline("Modified " ++ src);
        let structure = ConvertBack.copy_structure(newStructure);
        /* Pprintast.structure(Format.str_formatter, structure); */
        Reason_toolchain.RE.print_implementation_with_comments(Format.str_formatter, (structure, comments));

        Files.writeFileExn(src, Format.flush_str_formatter());
      }
    };
  });
};

