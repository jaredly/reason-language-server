
type target =
  | Js
  | Bytecode
  | Native;

type t =
  | Dune
  | Bsb(string)
  /* The bool is "Is Bytecode" */
  | BsbNative(string, target);

let isNative = config => Json.get("entries", config) != None || Json.get("allowed-build-kinds", config) != None;

let getLine = (cmd, ~pwd) => {
  switch (Commands.execFull(~pwd, cmd)) {
    | ([line], _, true) => Result.Ok(line)
    | (out, err, _) => Error("Invalid response for " ++ cmd ++ "\n\n" ++ String.concat("\n", out @ err))
  }
};

open Infix;
let detect = (rootPath, bsconfig) => {
  let%try_wrap bsbVersion = {
    let (output, success) = Commands.execSync(rootPath /+ "node_modules/.bin/bsb -version");
    success ? switch output {
    | [line] => Ok(String.trim(line))
    | _ => Error("Unable to determine bsb version")
    } : Error("Could not run bsb");
  };

  /* TODO add a config option to specify native vs bytecode vs js backend */
  isNative(bsconfig) ? BsbNative(bsbVersion, Native) : Bsb(bsbVersion);
};

let getCompiledBase = (root, buildSystem) =>
  Files.ifExists(
    switch (buildSystem) {
    | Bsb("3.2.0") => root /+ "lib" /+ "bs" /+ "js"
    | Bsb("3.1.1") => root /+ "lib" /+ "ocaml"
    | Bsb(_) => root /+ "lib" /+ "bs"
    | BsbNative(_, Js) => root /+ "lib" /+ "bs" /+ "js"
    | BsbNative(_, Native) => root /+ "lib" /+ "bs" /+ "native"
    | BsbNative(_, Bytecode) => root /+ "lib" /+ "bs" /+ "bytecode"
    | Dune => root /+ "_build" /* TODO */
    },
  );

let getStdlib = (base, buildSystem) =>
  switch (buildSystem) {
  | BsbNative(_, Js)
  | Bsb(_) => [base /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "ocaml"]
  | BsbNative("3.2.0", Native) =>
    [base /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "ocaml" /+ "native",
    base /+ "node_modules" /+ "bs-platform" /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative("3.2.0", Bytecode) =>
    [base /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "ocaml" /+ "bytecode",
    base /+ "node_modules" /+ "bs-platform" /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative(_, Bytecode | Native) =>
    [base
    /+ "node_modules"
    /+ "bs-platform"
    /+ "vendor"
    /+ "ocaml"
    /+ "lib"
    /+ "ocaml"]
  | Dune => failwith("Don't know how to find the dune stdlib")
  };

let getCompiler = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | BsbNative(_, Js)
    | Bsb(_) => rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "bsc.exe"
    | BsbNative(_, Native) => rootPath /+ "node_modules" /+ "bs-platform" /+ "vendor" /+ "ocaml" /+ "ocamlopt.opt -c"
    | BsbNative(_, Bytecode) => rootPath /+ "node_modules" /+ "bs-platform" /+ "vendor" /+ "ocaml" /+ "ocamlc.opt -c"
    | Dune => {
      let%try_force ocamlopt = getLine("esy which ocamlopt.opt", ~pwd=rootPath);
      ocamlopt ++ " -c"
    }
  };
};

let getRefmt = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | BsbNative("3.2.0", _) => rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "refmt.exe"
    | Bsb(version) when version > "2.2.0" => rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "refmt.exe"
    | Bsb(_) | BsbNative(_, _) => rootPath /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "refmt3.exe"
    | Dune => {
      let%try_force refmt = getLine("esy which refmt", ~pwd=rootPath);
      refmt
    }
  };
};

let hiddenLocation = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | Bsb(_)
    | BsbNative(_, _) => rootPath /+ "node_modules" /+ ".lsp"
    | Dune => rootPath /+ "_build" /+ ".lsp"
  };
};