
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

let getBsPlatformDir = rootPath => {
  let result =
    ModuleResolution.resolveNodeModulePath(
      ~startPath=rootPath,
      "bs-platform",
    );
  switch (result) {
  | Some(path) =>
    Log.log("Found bs-platform at " ++ path);
    Result.Ok(path);
  | None =>
    let message = "bs-platform could not be found";
    Log.log(message);
    Result.Error(message);
  };
};

/* One dir up, then into .bin.
    Is .bin always in the modules directory?
   */
let getBsbExecutable = rootPath =>
  Result.InfixResult.(
    getBsPlatformDir(rootPath)
    |?>> Filename.dirname
    |?>> (path => path /+ ".bin" /+ "bsb")
  );

let detect = (rootPath, bsconfig) => {
  let%try bsbExecutable = getBsbExecutable(rootPath);
  let%try_wrap bsbVersion = {
    let (output, success) = Commands.execSync(bsbExecutable ++ " -version");
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
  let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
  switch (buildSystem) {
    | BsbNative(_, Js)
    | Bsb(_) => bsPlatformDir /+ "lib" /+ "bsc.exe"
    | BsbNative(_, Native) => bsPlatformDir /+ "vendor" /+ "ocaml" /+ "ocamlopt.opt -c"
    | BsbNative(_, Bytecode) => bsPlatformDir /+ "vendor" /+ "ocaml" /+ "ocamlc.opt -c"
    | Dune => {
      let%try_force ocamlopt = getLine("esy which ocamlopt.opt", ~pwd=rootPath);
      ocamlopt ++ " -c"
    }
  };
};

let getRefmt = (rootPath, buildSystem) => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
  switch (buildSystem) {
    | BsbNative("3.2.0", _) => bsPlatformDir /+ "lib" /+ "refmt.exe"
    | Bsb(version) when version > "2.2.0" => bsPlatformDir /+ "lib" /+ "refmt.exe"
    | Bsb(_) | BsbNative(_, _) => bsPlatformDir /+ "lib" /+ "refmt3.exe"
    | Dune => {
      let%try_force refmt = getLine("esy which refmt", ~pwd=rootPath);
      refmt
    }
  };
};

let hiddenLocation = (rootPath, buildSystem) => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
  switch (buildSystem) {
    | Bsb(_)
    | BsbNative(_, _) => Filename.dirname(bsPlatformDir) /+ ".lsp"
    | Dune => rootPath /+ "_build" /+ ".lsp"
  };
};