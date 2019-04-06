
open Util;
open Infix;

let rootPath = Unix.getcwd();
let sourceDir = rootPath /+ "src/lsp";
let files = Files.readDirectory(sourceDir)->Belt.List.keep(x => Filename.check_suffix(x, ".re"))
->Belt.List.map(name => sourceDir /+ name);

// let rootPath = rootPath == "." ? Unix.getcwd() : maybeConcat(Unix.getcwd(), rootPath);
let state = {
  ...Analyze.TopTypes.empty(),
  rootPath,
  rootUri: Util.Utils.toUri(rootPath)
};
files->Belt.List.forEach(fileName => {
  let uri = Utils.toUri(fileName);
  let missing = ref(0);
  switch (Packages.getPackage(~reportDiagnostics=(_, _) => (), uri, state)) {
    | Error(message) =>
      print_endline("  Unable to get package: " ++ uri)
      print_endline(message);
    | Ok(package) => switch (State.getCompilationResult(uri, state, ~package)) {
      | Error(message) =>
        print_endline("  Invalid compilation result: " ++ message);
      | Ok(Success(_text, {file, extra})) =>
        extra.locations->Belt.List.forEach(((location, loc)) => {
          switch loc {
            // | Constant(_) | Typed(_, Definition(_, _)) | Open
            // | TopLevelModule
            // | Explanation(_) => () // these don't need definitions
            | Typed(_, (LocalReference(_, _) | GlobalReference(_, _, _)) as t)
            when !location.loc_ghost
            =>
              switch (References.definitionForLoc(
                ~pathsForModule=package.pathsForModule,
                ~file=file,
                ~getUri=State.fileForUri(state, ~package),
                ~getModule=State.fileForModule(state, ~package),
                loc,
              )) {
                | None =>
                missing := 1 + missing^;
                Printf.printf(" !! No definition for \"%s\", line %d, column %d : %s\n",
                fileName,
                location.loc_start.pos_lnum,
                location.loc_start.pos_cnum - location.loc_start.pos_bol + 1,
                SharedTypes.Loc.typedToString(t)
                )
                | Some(_defn) => ()
              }
            | _ => ()
          }
        })
        print_endline("  Good: " ++ uri);
        print_endline("  > " ++ string_of_int(missing^) ++ " missing")
      | Ok(TypeError(message, _) | SyntaxError(message, _, _)) =>
        print_endline("  Error compiling: " ++ uri);
    };
    // print_endline(Analyze.State.Show.state(state, package));
  }
});