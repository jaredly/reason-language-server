
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
  | V408
  | V407
  | V406
  | V402;

let showCompilerVersion = fun
  | V408 => "4.08"
  | V407 => "4.07"
  | V406 => "4.06"
  | V402 => "4.02";

type packageManager =
  /* Absolute path to the Opam switch prefix */
  | Opam(string)
  /* Esy version */
  | Esy(string);

type t =
  | Dune(packageManager)
  | Bsb(string)
  | BsbNative(string, target);

let usesStdlib = v => switch v {
  | V408 | V407 => true
  | V406 | V402 => false
};


let fromString = string => {
  switch (Util.Utils.split_on_char(':', string)) {
    | ["bsb", version] => Some(Bsb(version))
    | ["dune", "esy", version] => Some(Dune(Esy(version)))
    | ["dune", "opam", basedir] => Some(Dune(Opam(basedir)))
    | ["bsb-native", version, "js"] => Some(BsbNative(version, Js))
    | ["bsb-native", version, "native"] => Some(BsbNative(version, Native))
    | ["bsb-native", version, "bytecode"] => Some(BsbNative(version, Bytecode))
    | _ => None
  }
};

let show = t => switch t {
  | Dune(Esy(v)) => "dune & esy, esy version: " ++ v
  | Dune(Opam(loc)) => "dune & opam (switch at " ++ loc ++ ")"
  | Bsb(v) => "bsb version " ++ v
  | BsbNative(v, target) => "bsb-native version " ++ v ++ " targetting " ++ targetName(target)
}


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

let nodePlatform = 
    switch (Sys.os_type) {
      | "Unix" => switch (input_line (Unix.open_process_in ("uname -s"))) {
         | "Darwin"  => "darwin"
         | "Linux"   => "linux"
         | "FreeBSD" => "freebsd"
         | s => invalid_arg (s ++ ": unsupported os_type")
      }
      | "Win32" => "win32"
      | s => invalid_arg (s ++ ": unsupported os_type")
  };


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

let parseOCamlVersion = versionString =>switch (Utils.split_on_char('.', String.trim(versionString))) {
    | ["4", "02", ..._] => Ok(V402)
    | ["4", "06", ..._] => Ok(V406)
    | ["4", "07", ..._] => Ok(V407)
    | ["4", "08", ..._] => Ok(V408)
    | _ => Error("Unsupported OCaml version: " ++ versionString)
  }

let getCompilerVersion = executable => {
  let cmd = executable ++ " -version";
  let (output, success) = Commands.execSync(cmd);
  success ? switch output {
  | [line] when Str.string_match(Str.regexp_string("BuckleScript "), line, 0) =>
    switch (Str.split(Str.regexp("( *Using OCaml:?"), String.trim(line))) {
      | [_, version] => parseOCamlVersion(version)
      | xs => Error("Cannot detect OCaml version from BuckleScript version string: " ++ line ++ "[" ++ String.concat(" ;", xs) ++ "]")
    }
  | [line] => parseOCamlVersion(line)
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

let detectFull = projectDir => {
  let bsConfig = Filename.concat(projectDir, "bsconfig.json");
  if (Files.exists(bsConfig)) {
    let%try raw = Files.readFileResult(bsConfig);
    let config = Json.parse(raw);
    detect(projectDir, config);
  } else {
    let esy = getLine("esy --version", ~pwd=projectDir);
    switch (esy) {
    | Ok(v) => Ok(Dune(Esy(v)))
    | Error(err) => Error("Couldn't get esy version")
    };
  };
};

let getEsyCompiledBase = () => {
  let env = Unix.environment()->Array.to_list;

  switch(Utils.getEnvVar(~env, "cur__original_root"), Utils.getEnvVar(~env, "cur__target_dir")) {
  | (Some(projectRoot), Some(targetDir)) => Ok(Files.relpath(projectRoot, targetDir))
  | (_, _) =>
    switch (Commands.execResult("esy command-env --json")) {
    | Ok(commandEnv) =>
      switch (Json.parse(commandEnv)) {
      | exception (Failure(message)) =>
        Log.log("Json response");
        Log.log(commandEnv);
        Error("Couldn't find Esy target directory (invalid json response: parse fail): " ++ message);
      | exception exn =>
        Log.log(commandEnv);
        Error("Couldn't find Esy target directory (invalid json response) " ++ Printexc.to_string(exn));
      | json =>
        Json.Infix.(
          switch (
            Json.get("cur__original_root", json) |?> Json.string,
            Json.get("cur__target_dir", json) |?> Json.string,
          ) {
          | (Some(projectRoot), Some(targetDir)) => Ok(Files.relpath(projectRoot, targetDir))
          | _ => Error("Couldn't find Esy target directory (missing json entries)")
          }
        )
      }
    | err => err
    }
  }
};

let getCompiledBase = (root, buildSystem) => {
  let compiledBase = switch (buildSystem) {
  | Bsb("3.2.0") => Ok(root /+ "lib" /+ "bs" /+ "js")
  | Bsb("3.1.1") => Ok(root /+ "lib" /+ "ocaml")
  | Bsb(_) => Ok(root /+ "lib" /+ "bs")
  | BsbNative(_, Js) => Ok(root /+ "lib" /+ "bs" /+ "js")
  | BsbNative(_, Native) => Ok(root /+ "lib" /+ "bs" /+ "native")
  | BsbNative(_, Bytecode) => Ok(root /+ "lib" /+ "bs" /+ "bytecode")
  | Dune(Opam(_)) => Ok(root /+ "_build") /* TODO maybe check DUNE_BUILD_DIR */
  | Dune(Esy(_)) =>
    let%try_wrap esyTargetDir = getEsyCompiledBase();
    root /+ esyTargetDir
  };

  switch compiledBase {
  | Ok(compiledBase) => Files.ifExists(compiledBase);
  | _ => None
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
  | BsbNative(v, Native) when v >= "3.2.0" =>
    let%try_wrap bsPlatformDir = getBsPlatformDir(base);
    [bsPlatformDir /+ "lib" /+ "ocaml" /+ "native",
    bsPlatformDir /+ "vendor" /+ "ocaml" /+ "lib" /+ "ocaml"]
  | BsbNative(v, Bytecode) when v >= "3.2.0" =>
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
  | Dune(Esy(v)) =>
    let env = Unix.environment()->Array.to_list;
    switch (Utils.getEnvVar(~env, "OCAMLLIB")) {
    | Some(esy_ocamllib) => Ok([esy_ocamllib])
    | None =>
      let command = v < "0.5.6" ?
        "esy -q sh -- -c 'echo $OCAMLLIB'" : "esy -q sh -c 'echo $OCAMLLIB'";
      let%try_wrap esy_ocamllib = getLine(command, ~pwd=base);
      [esy_ocamllib];
    };
  | Dune(Opam(switchPrefix)) =>
    let%try libPath = getOpamLibOrBinPath(base, switchPrefix, "lib" /+ "ocaml")
    Ok([libPath])
  };
};

let isRunningInEsyNamedSandbox = () => {
  /* Check if we have `cur__target_dir` as a marker that we're inside an Esy context */
  Belt.Option.isSome(Utils.getEnvVar("cur__target_dir"))
};

let getExecutableInEsyPath = (exeName, ~pwd) => {
  let ret = if (isRunningInEsyNamedSandbox()) {
    getLine("which " ++ exeName, ~pwd)
  } else {
    getLine("esy which " ++ exeName, ~pwd)
  };
  if (Sys.win32) {
    switch ret {
      | RResult.Ok(ret) =>
        let ret = if (isRunningInEsyNamedSandbox()) {
          getLine("cygpath -w " ++ ret, ~pwd)
        } else {
          getLine("esy cygpath -w " ++ ret, ~pwd)
        };
        ret
      | Error(a) => Error(a)
    }
  } else {
    ret
  }
};

let getCompiler = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | BsbNative(_, Js)
    | Bsb(_) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      switch(Files.ifExists(bsPlatformDir /+ "lib" /+ "bsc.exe")){
        | Some (x) => x 
        | None => bsPlatformDir /+ nodePlatform /+ "bsc.exe"
      }
    | BsbNative(_, Native) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "vendor" /+ "ocaml" /+ "ocamlopt.opt"
    | BsbNative(_, Bytecode) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsPlatformDir /+ "vendor" /+ "ocaml" /+ "ocamlc.opt"
    | Dune(Esy(_)) => getExecutableInEsyPath("ocamlc.opt", ~pwd=rootPath)
    | Dune(Opam(switchPrefix)) =>
      let%try_wrap binPath = getOpamLibOrBinPath(rootPath, switchPrefix, "bin" /+ "ocamlopt.opt");
      binPath
  };
};

let getRefmt = (rootPath, buildSystem) => {
  let bsRefmt = (bsPlatformDir) =>
    switch (Files.ifExists(bsPlatformDir/+"lib"/+"refmt.exe")){
      | Some (x) => x
      | None => 
        switch(Files.ifExists(bsPlatformDir /+ nodePlatform /+ "refmt.exe")){
          | Some (x) => x 
          | None => bsPlatformDir /+ "lib" /+ "refmt3.exe"
        }
    }  
  switch (buildSystem) {
    | BsbNative("3.2.0", _) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsRefmt(bsPlatformDir)
    | Bsb(version) when version > "2.2.0" =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsRefmt(bsPlatformDir)
    | BsbNative(version, _) when version >= "4.0.6" =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsRefmt(bsPlatformDir)
    | Bsb(_) | BsbNative(_, _) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsRefmt(bsPlatformDir)
    | Dune(Esy(_)) => getExecutableInEsyPath("refmt",~pwd=rootPath)
    | Dune(Opam(switchPrefix)) =>
      Ok(switchPrefix /+ "bin" /+ "refmt")
  };
};

let hiddenLocation = (rootPath, buildSystem) => {
  switch (buildSystem) {
    | Bsb(_)
    | BsbNative(_, _) => Ok(rootPath /+ "node_modules" /+ ".lsp")
    | Dune(Opam(_)) => Ok(rootPath /+ "_build" /+ ".lsp")
    | Dune(Esy(_)) =>
      let%try_wrap esyTargetDir = getEsyCompiledBase();
      rootPath /+ esyTargetDir /+ ".lsp"
  };
};

let inferPackageManager = projectRoot => {
  let hasEsyDir = Files.exists(projectRoot /+ "_esy");

  if (hasEsyDir) {
    let esy = getLine("esy --version", ~pwd=projectRoot);
    switch (esy) {
    | Ok(v) =>
      Log.log("Detected `esy` dependency manager for local use");
      Ok(Esy(v));
    | Error(err) =>
      Log.log(err);
      Error("Couldn't get esy version")
    };
  } else {
    let opam = getLine("opam config var prefix", ~pwd=projectRoot);
    switch (opam) {
    | Ok(prefix) =>
      Log.log("Detected `opam` dependency manager for local use");
      Ok(Opam(prefix));
    | _ => Error("Couldn't get opam switch prefix")
    };
  };
};
