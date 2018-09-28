
type target =
  | Js
  | Bytecode
  | Native;

let targetName = t => switch t {
  | Js => "js"
  | Bytecode => "bytecode"
  | Native => "native"
};

type compilerVersion =
  | V406
  | V402;

type t =
  | Dune
  | Bsb(string)
  | BsbNative(string, target);

let isNative = config => Json.get("entries", config) != None || Json.get("allowed-build-kinds", config) != None;

let getLine = (cmd, ~pwd) => {
  switch (Commands.execFull(~pwd, cmd)) {
    | ([line], _, true) => RResult.Ok(line)
    | (out, err, _) => Error("Invalid response for " ++ cmd ++ "\n\n" ++ String.concat("\n", out @ err))
  }
};

open Infix;

let getBsPlatformDir = rootPath => {
  let result =
    ModuleResolution.resolveNodeModulePath(
      ~startPath=rootPath,
      "bs-platform",
    );
  switch (result) {
  | Some(path) =>
    RResult.Ok(path);
  | None =>
    let message = "bs-platform could not be found";
    Log.log(message);
    RResult.Error(message);
  };
};

/* One dir up, then into .bin.
    Is .bin always in the modules directory?
   */
let getBsbExecutable = rootPath =>
  RResult.InfixResult.(
    getBsPlatformDir(rootPath)
    |?>> Filename.dirname
    |?>> (path => path /+ ".bin" /+ "bsb")
  );

let getCompilerVersion = executable => {
  let cmd = executable ++ " -version";
  let (output, success) = Commands.execSync(cmd);
  success ? switch output {
  | [line] => switch (Utils.split_on_char('.', String.trim(line))) {
    | ["4", "02", _] => Ok(V402)
    | ["4", "06", _] => Ok(V406)
    | version => Error("Unsupported OCaml version: " ++ line)
  }
  | _ => Error("Unable to determine compiler version (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output))
  } : Error("Could not run compiler (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output));
};

let detect = (rootPath, bsconfig) => {
  let%try bsbExecutable = getBsbExecutable(rootPath);
  let%try_wrap bsbVersion = {
    let cmd = bsbExecutable ++ " -version";
    let (output, success) = Commands.execSync(cmd);
    success ? switch output {
    | [line] => Ok(String.trim(line))
    | _ => Error("Unable to determine bsb version (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output))
    } : Error("Could not run bsb (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output));
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

let getStdlib = (base, buildSystem) => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(base);
  switch (buildSystem) {
  | BsbNative(_, Js)
  | Bsb(_) => [bsPlatformDir /+ "lib" /+ "ocaml"]
  | BsbNative("3.2.0", Native) =>
    [bsPlatformDir /+ "lib" /+ "ocaml" /+ "native",
    bsPlatformDir /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative("3.2.0", Bytecode) =>
    [bsPlatformDir /+ "lib" /+ "ocaml" /+ "bytecode",
    bsPlatformDir /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative(_, Bytecode | Native) =>
    [bsPlatformDir
    /+ "vendor"
    /+ "ocaml"
    /+ "lib"
    /+ "ocaml"]
  | Dune => failwith("Don't know how to find the dune stdlib")
  };
};

let getCompiler = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | BsbNative(_, Js)
    | Bsb(_) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "lib" /+ "bsc.exe"
    | BsbNative(_, Native) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "vendor" /+ "ocaml" /+ "ocamlopt.opt"
    | BsbNative(_, Bytecode) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "vendor" /+ "ocaml" /+ "ocamlc.opt"
    | Dune => {
      let%try_wrap ocamlopt = getLine("esy which ocamlopt.opt", ~pwd=rootPath);
      ocamlopt
    }
  };
};

let getRefmt = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | BsbNative("3.2.0", _) => 
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "lib" /+ "refmt.exe"
    | Bsb(version) when version > "2.2.0" =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "lib" /+ "refmt.exe"
    | Bsb(_) | BsbNative(_, _) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "lib" /+ "refmt3.exe"
    | Dune => {
      let%try_wrap refmt = getLine("esy which refmt", ~pwd=rootPath);
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