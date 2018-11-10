
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
  | V407
  | V406
  | V402;

type packageManager =
  /* Absolute path to the Opam switch prefix */
  | Opam(string)
  | Esy(string);

type t =
  | Dune(packageManager)
  | Bsb(string)
  | BsbNative(string, target);

let isNative = config => Json.get("entries", config) != None || Json.get("allowed-build-kinds", config) != None;

let getLine = (cmd, ~pwd) => {
  switch (Commands.execFull(~pwd, cmd)) {
    | ([line], _, true) => RResult.Ok(line)
    | (out, err, _) => Error("Invalid response for " ++ cmd ++ "\n\n" ++ String.concat("\n", out @ err))
  }
};

let bucklescriptNamespacedName = (namespace, name) => switch namespace {
  | None => name
  | Some(namespace) => name ++ "-" ++ namespace
};

let duneNamespacedName = (namespace, name) => switch namespace {
  | None => name
  | Some(namespace) => namespace ++ "__" ++ name
};

let namespacedName = (buildSystem, namespace, name) => switch buildSystem {
  | Dune(_) => duneNamespacedName(namespace, name)
  | _ => bucklescriptNamespacedName(namespace, name)
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
    let resultSecondary =
      ModuleResolution.resolveNodeModulePath(
        ~startPath=rootPath,
        "bsb-native",
      );
    switch (resultSecondary) {
    | Some(path) => RResult.Ok(path)
    | None =>
      let message = "bs-platform could not be found";
      Log.log(message);
      RResult.Error(message);
    }
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
    | ["4", "07", _] => Ok(V407)
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

  isNative(bsconfig) ? BsbNative(bsbVersion, {
    switch {
      let%try backendString = MerlinFile.getBackend(rootPath);
      switch (backendString) {
      | "js" => Ok(Js)
      | "bytecode" => Ok(Bytecode)
      | "native" => Ok(Native)
      | s => Error("Found unsupported backend: " ++ s);
      };
    } {
      | Ok(backend) => backend
      | _ => Native
    };
  }) : Bsb(bsbVersion);
};

let getEsyCompiledBase = (root, esyVersion) =>
  switch (String.split_on_char('.', esyVersion)) {
  | [_, minor, ..._] =>
    let env = Unix.environment () |. Array.to_list;
    try {
      let root = List.find(var => Utils.startsWith(var, "cur__original_root="), env);
      let var = List.find(var => Utils.startsWith(var, "cur__target_dir="), env);
      let splitOnEquals = Utils.split_on_char('=');
      switch((splitOnEquals(root), splitOnEquals(var))) {
        | ([_, projectRoot], [_, targetDir]) => Ok(Files.relpath(projectRoot, targetDir))
        | _ => Error("Couldn't find Esy target directory (env missing)")
      }
    } {
    | _ =>
      switch(Commands.execResult("esy command-env --json")) {
      | Ok(commandEnv) =>
        switch (Json.parse(commandEnv)) {
          | exception Failure(message) =>
            Log.log("Json response");
            Log.log(commandEnv);
            Error("Couldn't find Esy target directory (invalid json response: parse fail): " ++ message)
          | exception exn =>
            Log.log(commandEnv);
            Error("Couldn't find Esy target directory (invalid json response) " ++ Printexc.to_string(exn))
          | json =>
            open Json.Infix;
            switch(Json.get("cur__original_root", json) |?> Json.string,
                  Json.get("cur__target_dir", json) |?> Json.string) {
              | (Some(projectRoot), Some(targetDir)) =>
                Ok(Files.relpath(projectRoot, targetDir))
              | _ => Error("Couldn't find Esy target directory (missing json entries)")
            }
        }
      | err => err
      }
    }
  | _ => Error("Couldn't find Esy target directory (bad esy version)")
  }

let getCompiledBase = (root, buildSystem) => {
  let compiledBase = switch (buildSystem) {
  | Bsb("3.2.0") => Ok(root /+ "lib" /+ "bs" /+ "js")
  | Bsb("3.1.1") => Ok(root /+ "lib" /+ "ocaml")
  | Bsb(_) => Ok(root /+ "lib" /+ "bs")
  | BsbNative(_, Js) => Ok(root /+ "lib" /+ "bs" /+ "js")
  | BsbNative(_, Native) => Ok(root /+ "lib" /+ "bs" /+ "native")
  | BsbNative(_, Bytecode) => Ok(root /+ "lib" /+ "bs" /+ "bytecode")
  | Dune(Opam(_)) => Ok(root /+ "_build") /* TODO maybe check DUNE_BUILD_DIR */
  | Dune(Esy(esyVersion)) =>
    let%try_wrap esyTargetDir = getEsyCompiledBase(root, esyVersion);
    root /+ esyTargetDir
  };

  switch compiledBase {
  | Ok(compiledBase) => Files.ifExists(compiledBase);
  | err => None
  };
};

let getOpamLibOrBinPath = (root, opamSwitchPrefix, path) => {
  let maybeLibOrBinPath = opamSwitchPrefix /+ path;
  if (Files.exists(maybeLibOrBinPath)) {
    Ok(maybeLibOrBinPath);
  } else {
    /* in local switches that share the sys-ocaml-version, the ocaml library and
     * binaries are apparently in the global (system) switch ¯\_(ツ)_/¯.
     */
    let%try sysOCamlVersion = getLine("opam config var sys-ocaml-version", ~pwd=root);
    let%try opamRoot = getLine("opam config var root", ~pwd=root);
    Ok(opamRoot /+ sysOCamlVersion /+ path);
  };
};

let getStdlib = (base, buildSystem) => {
  switch (buildSystem) {
  | BsbNative(_, Js)
  | Bsb(_) =>
    let%try_wrap bsPlatformDir = getBsPlatformDir(base);
    [bsPlatformDir /+ "lib" /+ "ocaml"]
  | BsbNative("3.2.0", Native) =>
    let%try_wrap bsPlatformDir = getBsPlatformDir(base);
    [bsPlatformDir /+ "lib" /+ "ocaml" /+ "native",
    bsPlatformDir /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative("3.2.0", Bytecode) =>
    let%try_wrap bsPlatformDir = getBsPlatformDir(base);
    [bsPlatformDir /+ "lib" /+ "ocaml" /+ "bytecode",
    bsPlatformDir /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative(_, Bytecode | Native) =>
    let%try_wrap bsPlatformDir = getBsPlatformDir(base);
    [bsPlatformDir
    /+ "vendor"
    /+ "ocaml"
    /+ "lib"
    /+ "ocaml"]
  | Dune(Esy(_)) =>
    let%try_wrap esy_ocamllib = getLine("esy -q sh -- -c 'echo $OCAMLLIB'", ~pwd=base);
    [esy_ocamllib]
  | Dune(Opam(switchPrefix)) =>
    let%try libPath = getOpamLibOrBinPath(base, switchPrefix, "lib" /+ "ocaml")
    Ok([libPath])
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
    | Dune(Esy(_)) =>
      let%try_wrap ocamlopt = getLine("esy which ocamlopt.opt", ~pwd=rootPath);
      ocamlopt
    | Dune(Opam(switchPrefix)) =>
      let%try_wrap binPath = getOpamLibOrBinPath(rootPath, switchPrefix, "bin" /+ "ocamlopt.opt")
      binPath
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
    | BsbNative(version, _) when version >= "4.0.6" =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "lib" /+ "refmt.exe"
    | Bsb(_) | BsbNative(_, _) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "lib" /+ "refmt3.exe"
    | Dune(Esy(_)) =>
      let%try_wrap refmt = getLine("esy which refmt", ~pwd=rootPath);
      refmt
    | Dune(Opam(switchPrefix)) =>
      Ok(switchPrefix /+ "bin" /+ "refmt")
  };
};

let hiddenLocation = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | Bsb(_)
    | BsbNative(_, _) => Ok(rootPath /+ "node_modules" /+ ".lsp")
    | Dune(Opam(_)) => Ok(rootPath /+ "_build" /+ ".lsp")
    | Dune(Esy(esyVersion)) =>
      let%try_wrap esyTargetDir = getEsyCompiledBase(rootPath, esyVersion);
      rootPath /+ esyTargetDir /+ ".lsp"
  };
};

let inferPackageManager = (projectRoot) => {
  let hasEsyDir = Files.exists(projectRoot /+ "_esy");

  switch(getLine("opam config var prefix", ~pwd=projectRoot)) {
    | Ok(prefix) when !hasEsyDir =>
      Log.log("Detected `opam` dependency manager for local use");
      Ok(Opam(prefix))
    | _ =>
      Log.log("Detected `esy` dependency manager for local use");
      let%try_wrap esyVersion = getLine("esy --version", ~pwd=projectRoot);
      Esy(esyVersion)
  };
};