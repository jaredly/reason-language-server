open Infix;
open TopTypes;
// open BuildCommand;

let escapePreprocessingFlags = flag => {
  /* ppx escaping not supported on windows yet */
  if (Sys.os_type == "Win32") {
    flag
  } else {
    let parts = Utils.split_on_char(' ', flag);
    switch(parts) {
      | [("-ppx" | "-pp") as flag, ...rest] =>
        flag ++ " " ++ Utils.maybeQuoteFilename(String.concat(" ", rest))
      | _ => flag
    }
  }
};

/**
 * Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files)
 */
let makePathsForModule = (localModules: list((string, SharedTypes.paths)), dependencyModules: list((string, SharedTypes.paths))) => {
  let pathsForModule = Hashtbl.create(30);
  let nameForPath = Hashtbl.create(30);
  let add = (name, paths) => switch paths {
    | SharedTypes.Intf(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
    | SharedTypes.Impl(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
    | SharedTypes.IntfAndImpl(_, intf, _, impl) =>
        intf |?< path => Hashtbl.replace(nameForPath, path, name);
        impl |?< path => Hashtbl.replace(nameForPath, path, name);
    | _ => ()
  };

  dependencyModules |> List.iter(((modName, paths)) => {
    add(modName, paths);
    Hashtbl.replace(pathsForModule, modName, paths)
  });

  localModules |> List.iter(((modName, paths)) => {
    add(modName, paths);
    Hashtbl.replace(pathsForModule, modName, paths)
  });

  (pathsForModule, nameForPath)
};

let newBsPackage = (~overrideBuildSystem=?, ~reportDiagnostics, state, rootPath) => {
  let%try raw = Files.readFileResult(rootPath /+ "bsconfig.json");
  let config = Json.parse(raw);

  let%try bsPlatform = BuildSystem.getBsPlatformDir(rootPath);

  

  
  let bsb = switch (Files.ifExists(bsPlatform /+ "lib" /+ "bsb.exe")){
    | Some (x) => x
    | None =>
      switch (Files.ifExists(bsPlatform /+ BuildSystem.nodePlatform /+ "bsb.exe")){
        | Some (x) => x
        | None => failwith ("can not locate bsb.exe in " ++ bsPlatform)
      } 
   };
  
  let%try buildSystem = switch overrideBuildSystem {
    | None => BuildSystem.detect(rootPath, config)
    | Some(s) => Ok(s)
  };


  let buildCommand = switch buildSystem {
    | Bsb(_) => bsb ++ " -make-world"
    | BsbNative(_, target) => bsb ++ " -make-world -backend " ++ BuildSystem.targetName(target)
    | Dune(_) => assert(false)
  };

  Log.log({|📣 📣 NEW BSB PACKAGE 📣 📣|});
  /* failwith("Wat"); */
  Log.log("- location: " ++ rootPath);
  Log.log("- bsPlatform: " ++ bsPlatform);
  Log.log("- buildSystem: " ++ BuildSystem.show(buildSystem));
  Log.log("- build command: " ++ buildCommand);

  let%try () = if (state.settings.autoRebuild) {
    BuildCommand.runBuildCommand(~reportDiagnostics, state, rootPath, Some((buildCommand, rootPath)));
  } else { Ok() };

  let compiledBase = BuildSystem.getCompiledBase(rootPath, buildSystem);
  let%try stdLibDirectories = BuildSystem.getStdlib(rootPath, buildSystem);
  let%try compilerPath = BuildSystem.getCompiler(rootPath, buildSystem);
  let mlfmtPath = state.settings.mlfmtLocation;
  let%try refmtPath = BuildSystem.getRefmt(rootPath, buildSystem);
  let%try tmpPath = BuildSystem.hiddenLocation(rootPath, buildSystem);
  let%try (dependencyDirectories, dependencyModules) = FindFiles.findDependencyFiles(~debug=true, ~buildSystem, rootPath, config);
  let%try_wrap compiledBase = compiledBase |> RResult.orError("You need to run bsb first so that reason-language-server can access the compiled artifacts.\nOnce you've run bsb, restart the language server.");

  let supportsNamespaceRename = BuildSystem.(switch(buildSystem) {
    | Bsb(v) when v >= "5.0.0" => true
    | _ => false
    });
  let namespace = FindFiles.getNamespace(~supportsNamespaceRename, config);
  let localSourceDirs = FindFiles.getSourceDirectories(~includeDev=true, rootPath, config);
  Log.log("Got source directories " ++ String.concat(" - ", localSourceDirs));
  let localCompiledDirs = localSourceDirs |> List.map(Infix.fileConcat(compiledBase));
  let localCompiledDirs = namespace == None ? localCompiledDirs : [compiledBase, ...localCompiledDirs];

  let localModules =
    FindFiles.findProjectFiles(~debug=true, namespace, rootPath, localSourceDirs, compiledBase);
    /* |> List.map(((name, paths)) => (switch (namespace) {
      | None => name
      | Some(n) => name ++ "-" ++ n }, paths)); */
  Log.log("-- All local modules found: " ++ string_of_int(List.length(localModules)));
  localModules |> List.iter(((name, paths)) => {
    Log.log(name);
    switch paths {
      | SharedTypes.Impl(cmt, _) => Log.log("impl " ++ cmt)
      | Intf(cmi, _) => Log.log("intf " ++ cmi)
      | _ => Log.log("Both")
    }
  });

  let (pathsForModule, nameForPath) = makePathsForModule(localModules, dependencyModules);

  let opens = switch (namespace) {
    | None => []
    | Some(namespace) => {
      let cmt = compiledBase /+ namespace ++ ".cmt";
      Log.log("############ Namespaced as " ++ namespace ++ " at " ++ cmt);
      Hashtbl.add(pathsForModule, namespace, Impl(cmt, None));
      [FindFiles.nameSpaceToName(namespace)]
    }
  };
  Log.log("Dependency dirs " ++ String.concat(" ", dependencyDirectories));

  let (flags, opens) = switch buildSystem {
    /* Bsb-native's support for merlin is not dependable */
    /* So I have to reimplement the compiler flags here. */
    | BsbNative(_, _) =>
      let defaultFlags = [
        "-w -30-40+6+7+27+32..39+44+45+101",
      ];
      let flags = config |> Json.get("bsc-flags") |?> Json.array |?>> Utils.filterMap(Json.string) |? [];
      let flags = defaultFlags @ flags;
      let flags = switch namespace {
        | None => flags
        | Some(name) => flags @ ["-open " ++ FindFiles.nameSpaceToName(name)]
      };
      let flags = config |> Json.get("reason") |?> Json.get("react-jsx") != None
      ? flags @ ["-ppx " ++ bsPlatform /+ "lib" /+ "reactjs_jsx_ppx_2.exe"]
      : flags;
      let ppxs = config |> Json.get("ppx-flags") |?> Json.array |?>> Utils.filterMap(Json.string) |? [];
      Log.log("Getting hte ppxs yall");
      let flags = flags @ (Belt.List.map(ppxs, name => {
        MerlinFile.fixPpxBsNative("-ppx " ++ Filename.quote(name), rootPath)
      }));
      let flags = switch (config |> Json.get("warnings") |?> Json.get("number") |?> Json.string) {
        | None => flags
        | Some(w) => flags @ ["-w " ++ w]
      };
      (flags @ [
        "-ppx " ++ bsPlatform /+ "lib" /+ "bsppx.exe"
      ], opens)
    | _ => {
      let flags =
        MerlinFile.getFlags(rootPath)
        |> RResult.withDefault([""])
        |> List.map(escapePreprocessingFlags);
      let opens = List.fold_left((opens, item) => {
        let parts = Utils.split_on_char(' ', item);
        let rec loop = items => switch items {
          | ["-open", name, ...rest] => [name, ...loop(rest)]
          | [_, ...rest] => loop(rest)
          | [] => []
        };
        opens @ loop(parts)
      }, opens, flags);
      (flags, opens)
    }
  };

  let flags = switch buildSystem {
    | Bsb(version) | BsbNative(version, Js) => {

      let jsPackageMode = {
        let specs = config |> Json.get("package-specs");
        let spec = switch specs {
          | Some(Json.Array([item, ..._])) => Some(item)
          | Some(Json.Object(_)) => specs
          | _ => None
        };
        spec |?> Json.get("module") |?> Json.string
      } |? "commonjs";
      let flags = switch (jsPackageMode) {
        | "es6" as packageMode
        | "es6-global" as packageMode => [
            "-bs-package-name",
            config |> Json.get("name") |?> Json.string |? "unnamed",
            ...packageMode == "es6"
              ? ["-bs-package-output", "es6:node_modules/.lsp", ...flags]
              : flags
          ]
        | _ => flags;
      };
      /* flags */
      [ version > "7.1" ? "-bs-no-builtin-ppx" : "-bs-no-builtin-ppx-ml", ...flags];
    }
    | _ => flags
  };

  let interModuleDependencies = Hashtbl.create(List.length(localModules));

  module Semver = Util.Semver;
  let compilerVersion = switch buildSystem {
    | Bsb(version) when Semver.parse(version)->Semver.major >= "6" => BuildSystem.V406
    | _ => BuildSystem.V402
  };

  {
    basePath: rootPath,
    rebuildTimer: 0.,
    localModules: localModules |. Belt.List.map(fst),
    dependencyModules: dependencyModules |. Belt.List.map(fst),
    pathsForModule,
    nameForPath,
    buildSystem,
    buildCommand: state.settings.autoRebuild ? Some((buildCommand, rootPath)) : None,
    opens,
    tmpPath,
    namespace,
    /* Bucklescript is always 4.02.3 */
    compilerVersion,
    compilationFlags: flags |> String.concat(" "),
    interModuleDependencies,
    includeDirectories:
      localCompiledDirs @
      dependencyDirectories @
      stdLibDirectories
      ,
    compilerPath,
    mlfmtPath: mlfmtPath,
    refmtPath: Some(refmtPath),
    /** TODO detect this from node_modules */
    lispRefmtPath: None,
  };
};

let rec findJbuilderProjectRoot = (path) => {
  if (path == "/") {
    RResult.Error("Unable to find project root dir")
  } else if (Files.exists(path /+ "dune-project")) {
    Ok(path)
  } else if (Files.exists(path /+ "dune-workspace")) {
    Ok(path)
  } else if (Files.exists(path /+ "_build")) {
    Ok(path)
  } else if (Files.exists(path /+ "_esy")) {
    Ok(path)
  } else if (Files.exists(path /+ "_opam")) {
    Ok(path)
  } else {
    findJbuilderProjectRoot(Filename.dirname(path))
  }
};

let pathToPath = paths => switch paths {
  | `Intf(a, b) => SharedTypes.Intf(a, b)
  | `Impl(a, b) => SharedTypes.Impl(a, b)
  | `IntfAndImpl(a, b, c, d) => SharedTypes.IntfAndImpl(a, b, c, d)
};

let newJbuilderPackage = (~overrideBuildSystem=?, ~reportDiagnostics, state, rootPath) => {
  /* `Filename.dirname` strips the current path segment even if it's a
   * directory, which means it would disallow projects where the source files
   * are at the toplevel, i.e. co-located with the e.g. `dune-project` file.
   */
  let%try projectRoot = findJbuilderProjectRoot(
    Sys.is_directory(rootPath) ? rootPath : Filename.dirname(rootPath)
  );
  Log.log("=== Project root: " ++ projectRoot);

  let%try (pkgMgr, buildSystem) = switch overrideBuildSystem {
    | None =>
      let%try pkgMgr = BuildSystem.inferPackageManager(projectRoot);
      Ok((pkgMgr, BuildSystem.Dune(pkgMgr)));
    | Some(BuildSystem.Dune(mgr)) => Ok((mgr, Dune(mgr)))
    | Some(_) => failwith("Invalid build system override when creating a new dune package")
  };

  let%try merlinRaw = Files.readFileResult(rootPath /+ ".merlin");
  let (source, _build, flags) = MerlinFile.parseMerlin("", merlinRaw);

  let%try (jbuildPath, jbuildRaw) = JbuildFile.readFromDir(rootPath);
  let%try jbuildConfig = switch (JbuildFile.parse(jbuildRaw)) {
    | exception Failure(message) => Error("Unable to parse build file " ++ jbuildPath ++ " " ++ message)
    | x => Ok(x)
  };

  let packageName = JbuildFile.findName(jbuildConfig);

  let namespace = switch packageName {
    | `Library(name) => Some(name)
    | _ => None
  };

  let libraryName = switch packageName {
    | `Library(n) => Some(n)
    | _ => None
  };

  let%try hiddenLocation = BuildSystem.hiddenLocation(projectRoot, buildSystem);
  Files.mkdirp(hiddenLocation);

  Log.log("Get ocaml stdlib dirs");
  let%try stdlibs = BuildSystem.getStdlib(projectRoot, buildSystem);

  let%try (pathsForModule, nameForPath, localModules, depsModules, includeDirectories) = {
    Log.log("New dune process");
    let (pathsForModule_, nameForPath, localModules, depsModules, includeDirectories) = MerlinFile.getModulesFromMerlin(
      ~stdlibs,
      rootPath, merlinRaw);
    let pathsForModule = Hashtbl.create(Stdlib.Hashtbl.length(pathsForModule_));
    pathsForModule_ |> Hashtbl.iter((k, v) => pathsForModule->Hashtbl.replace(k, pathToPath(v)));
    // pathsForModule->Hasthbl.map
    Ok((
      pathsForModule,
      nameForPath,
      localModules->Belt.List.keepMap(name => Utils.maybeHash(pathsForModule, name) |?>> (x => (name, x))),
      depsModules,
      includeDirectories
    ))
  };

  let interModuleDependencies = Hashtbl.create(List.length(localModules));

  let buildCommand = switch (pkgMgr) {
    | Esy(_) =>
      Some(("esy", projectRoot))
    | Opam(switchPrefix) =>
      if (Files.exists(switchPrefix /+ "bin" /+ "dune")) {
        Some(("opam exec -- dune build @install --root .", projectRoot))
      } else if (Files.exists(projectRoot /+ "_opam" /+ "bin" /+ "jbuild")) {
        Some(("opam exec -- jbuild build @install --root .", projectRoot))
      } else {
        None
      }
  };

  let%try () = if (state.settings.autoRebuild) {
    BuildCommand.runBuildCommand(~reportDiagnostics, state, rootPath, buildCommand);
  } else { Ok() };

  let%try compilerPath = BuildSystem.getCompiler(projectRoot, buildSystem);
  let%try compilerVersion = BuildSystem.getCompilerVersion(compilerPath);
  let mlfmtPath = state.settings.mlfmtLocation;
  let refmtPath = BuildSystem.getRefmt(projectRoot, buildSystem) |> RResult.toOptionAndLog;
  Ok({
    basePath: rootPath,
    localModules: localModules |. Belt.List.map(fst),
    rebuildTimer: 0.,
    interModuleDependencies,
    dependencyModules: depsModules,
    pathsForModule,
    namespace,
    nameForPath,
    buildSystem,
    buildCommand,
    /* TODO check if there's a module called that */
    opens: fold(libraryName, [], libraryName => [String.capitalize_ascii(libraryName)]),
    tmpPath: hiddenLocation,
    compilationFlags: flags |> String.concat(" "),
    includeDirectories,
    compilerVersion,
    compilerPath,
    mlfmtPath,
    refmtPath,
    lispRefmtPath: None,
  });
};






let findRoot = (uri, packagesByRoot, overrides) => {
  let override = overrides->Belt.List.getBy(((prefix, _system)) => {
    Util.Utils.startsWith(uri, prefix)
  });
  switch override {
    | Some((root, system)) => Some(`Override(root, system))
    | None =>
      let%opt path = Utils.parseUri(uri);
      let rec loop = path => {
        if (path == "/") {
          None
        } else if (Hashtbl.mem(packagesByRoot, path)) {
          Some(`Root(path))
        } else if (Files.exists(path /+ "bsconfig.json")) {
          Some(`Bs(path))
        } else if (Files.exists(path /+ "dune")) {
          Log.log("Found a `dune` file at " ++ path);
          Some(`Jbuilder(path))
        } else if (Files.exists(path /+ ".merlin")) {
          Log.log("Found a `.merlin` file at " ++ path);
          Some(`Jbuilder(path))
        } else {
          loop(Filename.dirname(path))
        }
      };
      loop(Filename.dirname(path))
  }
};

let newPackageForRoot = (~reportDiagnostics, state, root) => {
  if (Files.exists(root /+ "bsconfig.json")) {
    let%try package = newBsPackage(~reportDiagnostics, state, root);
    Files.mkdirp(package.tmpPath);
    /* Hashtbl.replace(state.rootForUri, uri, package.basePath); */
    Hashtbl.replace(state.packagesByRoot, package.basePath, package);
    RResult.Ok(package)
  } else if (Files.exists(root /+ "dune-project")) {
    let%try package = newJbuilderPackage(~reportDiagnostics, state, root);
    Files.mkdirp(package.tmpPath);
    /* Hashtbl.replace(state.rootForUri, uri, package.basePath); */
    Hashtbl.replace(state.packagesByRoot, package.basePath, package);
    RResult.Ok(package)
  } else {
    RResult.Error("No bsconfig.json or dune-project found");
  }
};

let getPackage = (~reportDiagnostics, uri, state) => {
  if (Hashtbl.mem(state.rootForUri, uri)) {
    RResult.Ok(Hashtbl.find(state.packagesByRoot, Hashtbl.find(state.rootForUri, uri)))
  } else {
    let%try root = findRoot(uri, state.packagesByRoot, state.settings.buildSystemOverrideByRoot) |> RResult.orError("No root directory found");
    let%try package = switch root {
    | `Root(rootPath) =>
      Hashtbl.replace(state.rootForUri, uri, rootPath);
      RResult.Ok(Hashtbl.find(state.packagesByRoot, Hashtbl.find(state.rootForUri, uri)))
    | `Override(rootPath, (Bsb(_) | BsbNative(_)) as buildSystem) =>
      let%try package = newBsPackage(~overrideBuildSystem=buildSystem, ~reportDiagnostics, state, rootPath);
      Files.mkdirp(package.tmpPath);
      let package = {
        ...package,
        refmtPath: state.settings.refmtLocation |?? package.refmtPath,
        lispRefmtPath: state.settings.lispRefmtLocation |?? package.lispRefmtPath,
      };
      Hashtbl.replace(state.rootForUri, uri, package.basePath);
      Hashtbl.replace(state.packagesByRoot, package.basePath, package);
      RResult.Ok(package)
    | `Bs(rootPath) =>
      let%try package = newBsPackage(~reportDiagnostics, state, rootPath);
      Files.mkdirp(package.tmpPath);
      let package = {
        ...package,
        refmtPath: state.settings.refmtLocation |?? package.refmtPath,
        lispRefmtPath: state.settings.lispRefmtLocation |?? package.lispRefmtPath,
      };
      Hashtbl.replace(state.rootForUri, uri, package.basePath);
      Hashtbl.replace(state.packagesByRoot, package.basePath, package);
      RResult.Ok(package)
    | `Override(path, (Dune(_)) as buildSystem) =>
      Log.log("]] Making a new jbuilder package at " ++ path);
      let%try package = newJbuilderPackage(~overrideBuildSystem=buildSystem, ~reportDiagnostics, state, path);
      Files.mkdirp(package.tmpPath);
      Hashtbl.replace(state.rootForUri, uri, package.basePath);
      Hashtbl.replace(state.packagesByRoot, package.basePath, package);
      RResult.Ok(package)
    | `Jbuilder(path) =>
      Log.log("]] Making a new jbuilder package at " ++ path);
      let%try package = newJbuilderPackage(~reportDiagnostics, state, path);
      Files.mkdirp(package.tmpPath);
      Hashtbl.replace(state.rootForUri, uri, package.basePath);
      Hashtbl.replace(state.packagesByRoot, package.basePath, package);
      RResult.Ok(package)
    };

    /* {
      let rootName = switch root {
        | `Bs(s) => Some("bs:" ++ s)
        | `Jbuilder(s) => Some("dune:" ++ s)
        | `Root(_) => None
      };
      let%opt_consume root = rootName;
      Log.log("[[[  New package : at root " ++ root ++ " ]]]");
      Log.log("# Local packages")
      package.localModules |. Belt.List.forEach(name => Log.log(" - " ++ name))
      Log.log("# All detected modules");
      package.pathsForModule |> Hashtbl.iter((name, paths) => Log.log(" - " ++ name ++ "\t\t\t" ++ switch paths {
        | SharedTypes.Impl(cmt, _) => "impl " ++ cmt
        | Intf(cmt, _) => "intf " ++ cmt
        | IntfAndImpl(cmt, _, cmpl, _) => "both " ++ cmt ++ " " ++ cmpl
      }));
    }; */

    Ok(package)
  }
};
