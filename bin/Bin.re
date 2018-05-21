
/* open State; */

/* let main = () => {
  let input = CliToInput.parse(Sys.argv);
  print_endline("<<< Converting input to model!");
  let package = InputToModel.package(~canBundle=input.Input.packageInput.canBundle, ~namespaced=input.Input.packageInput.namespaced, input.Input.packageInput);
  print_endline("<<< Compiling!");
  let compilationResults = CompilePackage.compilePackage(~debug=input.Input.env.debug, package);
  print_endline("<<< Compiled!");
  /* outputPackage(package, allCodeBlocks, input.Input.target); */
  ModelToOutput.package(package, compilationResults, input.Input.target, input.Input.env);
}; */
Printexc.record_backtrace(true);

let () = Main.main();
