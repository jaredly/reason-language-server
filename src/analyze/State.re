open Infix;

open TopTypes;

module Show = {
  let state = ({rootPath}, {localModules, dependencyModules, pathsForModule}) => {
    "Root: " ++ rootPath ++
    "\nLocal\n"++
    (Belt.List.map(localModules, (name) => {
      let paths = Hashtbl.find(pathsForModule, name);
      Printf.sprintf("%s (%s : %s)", name, SharedTypes.getCmt(paths), SharedTypes.getSrc(paths) |? "(no src!)")
    }) |> String.concat("\n"))
    ++
    "\nDeps\n" ++
    (Belt.List.map(dependencyModules, (modname) => {
      let paths = Hashtbl.find(pathsForModule, modname);
      Printf.sprintf("%s (%s : %s)", modname, SharedTypes.getCmt(paths), SharedTypes.getSrc(paths) |? "")
    }) |> String.concat("\n"))
  };
};

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
    /* switch (paths) {
      | Impl(_, Some(src)) => Log.log("> Dep " ++ modName ++ " at " ++ src)
      | _ => ()
    }; */
    Hashtbl.replace(pathsForModule, modName, paths)
  });

  localModules |> List.iter(((modName, paths)) => {
    add(modName, paths);
    /* switch (paths) {
      | Impl(_, Some(src)) => Log.log("> Local " ++ modName ++ " at " ++ src)
      | _ => ()
    }; */
    Hashtbl.replace(pathsForModule, modName, paths)
  });

  (pathsForModule, nameForPath)
};

let rec getAffectedFiles = (root, lines) => switch lines {
  | [] => []
  | [one, ...rest] when Utils.startsWith(one, "ninja: error: ") =>
    [Utils.toUri(root /+ "bsconfig.json"), ...getAffectedFiles(root, rest)]
  | [one, ...rest] when Utils.startsWith(one, "File \"") =>
    switch (Utils.split_on_char('"', String.trim(one))) {
      | [_, name, ..._] => [(root /+ name) |> Utils.toUri, ...getAffectedFiles(root, rest)]
      | _ => {
        Log.log("Unable to parse file line " ++ one);
        getAffectedFiles(root, rest)
      }
    }
  | [one, two, ...rest] when Utils.startsWith(one, "  Warning number ") || Utils.startsWith(one, "  We've found a bug ") =>
    switch (Utils.split_on_char(' ', String.trim(two))) {
      | [one, ..._] => [one |> String.trim |> Utils.toUri, ...getAffectedFiles(root, rest)]
      | _ => getAffectedFiles(root, [two, ...rest])
    }
  | [_, ...rest] => {
    /* Log.log(
      "Not covered " ++ one
    ); */
    getAffectedFiles(root, rest)
  }
};

let runBuildCommand = (~reportDiagnostics, state, root, buildCommand) => {
  /** TODO check for a bsb.lock file & bail if it's there */
  /** TODO refactor so Dune projects don't get bsconfig.json handling below */
  let%opt_consume (buildCommand, commandDirectory) = buildCommand;
  Log.log(">> Build system running: " ++ buildCommand);
  let (stdout, stderr, _success) = Commands.execFull(~pwd=commandDirectory, buildCommand);
  Log.log(">>> stdout");
  Log.log(Utils.joinLines(stdout));
  Log.log(">>> stderr");
  Log.log(Utils.joinLines(stderr));
  let files = getAffectedFiles(commandDirectory, stdout @ stderr);
  Log.log("Affected files: " ++ String.concat(" ", files));
  let bsconfigJson = root /+ "bsconfig.json" |> Utils.toUri;
  let bsconfigClean = ref(true);
  files |. Belt.List.forEach(uri => {
    if (Utils.endsWith(uri, "bsconfig.json")) {
      bsconfigClean := false;
      Log.log("Bsconfig.json sending");
      reportDiagnostics(
        uri, `BuildFailed(stdout @ stderr)
        )
    };
    Hashtbl.remove(state.compiledDocuments, uri);
    Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() -. 0.01)
  });
  if (bsconfigClean^) {
    Log.log("Cleaning bsconfig.json");
    reportDiagnostics(bsconfigJson, `BuildSucceeded)
  }
  /* TODO report notifications here */
};

let escapePpxFlag = flag => {
  let parts = Utils.split_on_char(' ', flag);
  switch(parts) {
    | ["-ppx", ...ppx] => "-ppx " ++ (String.concat(" ", ppx) |> Filename.quote)
    | _ => flag
  }
};

let newBsPackage = (~overrideBuildSystem=?, ~reportDiagnostics, state, rootPath) => {
  let%try raw = Files.readFileResult(rootPath /+ "bsconfig.json");
  let config = Json.parse(raw);

  let%try bsPlatform = BuildSystem.getBsPlatformDir(rootPath);

  let%try buildSystem = switch overrideBuildSystem {
    | None => BuildSystem.detect(rootPath, config)
    | Some(s) => Ok(s)
  };

  let%try bsb = Files.ifExists(bsPlatform /+ "lib" /+ "bsb.exe") |> RResult.orError(bsPlatform /+ "lib" /+ "bsb.exe not found. aborting");

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

  if (state.settings.autoRebuild) {
    runBuildCommand(~reportDiagnostics, state, rootPath, Some((buildCommand, rootPath)));
  };

  let compiledBase = BuildSystem.getCompiledBase(rootPath, buildSystem);
  let%try stdLibDirectories = BuildSystem.getStdlib(rootPath, buildSystem);
  let%try compilerPath = BuildSystem.getCompiler(rootPath, buildSystem);
  let%try refmtPath = BuildSystem.getRefmt(rootPath, buildSystem);
  let%try tmpPath = BuildSystem.hiddenLocation(rootPath, buildSystem);
  let%try (dependencyDirectories, dependencyModules) = FindFiles.findDependencyFiles(~debug=true, ~buildSystem, rootPath, config);
  let%try_wrap compiledBase = compiledBase |> RResult.orError("You need to run bsb first so that reason-language-server can access the compiled artifacts.\nOnce you've run bsb, restart the language server.");

  let namespace = FindFiles.getNamespace(config);
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
        MerlinFile.fixPpx("-ppx " ++ name, rootPath) |> escapePpxFlag
      }));
      let flags = switch (config |> Json.get("warnings") |?> Json.get("number") |?> Json.string) {
        | None => flags
        | Some(w) => flags @ ["-w " ++ w]
      };
      (flags @ [
        "-ppx " ++ bsPlatform /+ "lib" /+ "bsppx.exe"
      ], opens)
    | _ => {
      let flags = MerlinFile.getFlags(rootPath) |> RResult.withDefault([""]) |> List.map(escapePpxFlag);
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
    | Bsb(_) | BsbNative(_, Js) => {

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
      ["-bs-no-builtin-ppx-ml", ...flags];
    }
    | _ => flags
  };

  let interModuleDependencies = Hashtbl.create(List.length(localModules));

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
    compilerVersion: BuildSystem.V402,
    compilationFlags: flags |> String.concat(" "),
    interModuleDependencies,
    includeDirectories:
      localCompiledDirs @
      dependencyDirectories @
      stdLibDirectories
      ,
    compilerPath,
    refmtPath: Some(refmtPath),
    /** TODO detect this from node_modules */
    lispRefmtPath: None,
  };
};

let newJbuilderPackage = (~overrideBuildSystem=?, ~reportDiagnostics, state, rootPath) => {
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

  let%try projectRoot = findJbuilderProjectRoot(Filename.dirname(rootPath));
  Log.log("=== Project root: " ++ projectRoot);

  let%try (pkgMgr, buildSystem) = switch overrideBuildSystem {
    | None =>
      let%try pkgMgr = BuildSystem.inferPackageManager(projectRoot);
      Ok((pkgMgr, BuildSystem.Dune(pkgMgr)));
    | Some(BuildSystem.Dune(mgr)) => Ok((mgr, Dune(mgr)))
    | Some(_) => failwith("Invalid build system override when creating a new dune package")
  };

  let%try buildDir =
    BuildSystem.getCompiledBase(projectRoot, buildSystem)
    |> RResult.resultOfOption(
      "Could not find local build dir",
    );
  Log.log("=== Build dir:    " ++ buildDir);

  let%try merlinRaw = Files.readFileResult(rootPath /+ ".merlin");
  let (source, _build, flags) = MerlinFile.parseMerlin("", merlinRaw);

  let%try (jbuildPath, jbuildRaw) = JbuildFile.readFromDir(rootPath);
  let%try jbuildConfig = switch (JbuildFile.parse(jbuildRaw)) {
    | exception Failure(message) => Error("Unable to parse build file " ++ jbuildPath ++ " " ++ message)
    | x => Ok(x)
  };
  let packageName = JbuildFile.findName(jbuildConfig);

  Log.log("Get ocaml stdlib dirs");
  let%try stdlibs = BuildSystem.getStdlib(projectRoot, buildSystem);

  /* TODO support binaries, and other directories */
  let includeSubdirs = JbuildFile.hasIncludeSubdirs(jbuildConfig);
  Log.log("Include subdirs? " ++ (includeSubdirs ? "YES" : "no :/"));

  let sourceFiles = includeSubdirs
    ? Files.collect(rootPath, FindFiles.isSourceFile)->Belt.List.map(path => Files.relpath(rootPath, path))
    : Files.readDirectory(rootPath) |> List.filter(FindFiles.isSourceFile);
  let rel = Files.relpath(projectRoot, rootPath);
  let compiledBase = switch packageName {
    | `NoName => buildDir /+ "default" /+ rel
    | `Library(libraryName) => buildDir /+ "default" /+ rel /+ "." ++ libraryName ++ ".objs"
    | `Executable(execName) => buildDir /+ "default" /+ rel /+ "." ++ execName ++ ".eobjs"
  };
  let libraryName = switch packageName {
    | `Library(n) => Some(n)
    | _ => None
  };

  let namespace = switch packageName {
    | `Library(name) => Some(name)
    | _ => None
  };

  /* print_endline("locals"); */
  Log.log("Got a compiled base " ++ compiledBase);
  let localModules = sourceFiles |> List.map(filename => {
    let name = FindFiles.getName(filename);
    let namespaced = switch packageName {
      | `NoName | `Executable(_) => name
      | `Library(libraryName) =>
        String.capitalize_ascii(libraryName) ++ "__" ++ String.capitalize_ascii(name)
    };
    Log.log("Local file: " ++ (rootPath /+ filename));
    let implCmtPath =
      compiledBase
        /+ (
          fold(libraryName, "", l => l ++ "__")
          ++ String.capitalize_ascii(Filename.chop_extension(filename))
          ++ ".cmt"
        );
    Log.log("Local .cmt file: " ++ implCmtPath);
    (
      namespaced,
      SharedTypes.Impl(implCmtPath, Some(rootPath /+ filename)),
    );
  });


  /* print_endline("Getting things"); */
  let (otherDirectories, otherFiles) = source |> List.filter(s => s != "." && s != "" && s.[0] != '/') |> optMap(name => {
    let otherPath = rootPath /+ name;
    let res = {
      let%try (jbuildPath, jbuildRaw) = JbuildFile.readFromDir(otherPath);
      let%try jbuildConfig = switch (JbuildFile.parse(jbuildRaw)) {
        | exception Failure(message) => Error("Unable to parse build file " ++ jbuildPath ++ " " ++ message)
        | x => Ok(x)
      };
      let%try libraryName = JbuildFile.findName(jbuildConfig) |> n => switch n {
        | `Library(name) => RResult.Ok(name)
        | _ => Error("Not a library")
      };
      let rel = Files.relpath(projectRoot, otherPath);
      let compiledBase = buildDir /+ "default" /+ rel /+ "." ++ libraryName ++ ".objs";
      Log.log("Found " ++ libraryName ++ " defined in " ++ jbuildPath);
      Log.log("Compiled base: " ++ compiledBase);
      Ok((compiledBase, FindFiles.collectFiles(~compiledTransform=modName => {
        if (modName == libraryName) {
          modName
        } else {
          libraryName ++ "__" ++ modName
        }
      }, ~sourceDirectory=otherPath, compiledBase)));
    };
    switch res {
      | Error(message) => {
        Log.log(message);
        None
      }
      | Ok(res) => Some(res)
    }
  }) |> List.split;

  let dependencyDirectories = (source |> List.filter(s => s != "" && s != "." && s.[0] == '/')) @ stdlibs;

  let%try hiddenLocation = BuildSystem.hiddenLocation(projectRoot, buildSystem);
  Files.mkdirp(hiddenLocation);

  let dependencyModules = dependencyDirectories
  |> List.map(path => {
    Log.log(">> Collecting deps for " ++ path);
    Files.readDirectory(Infix.maybeConcat(rootPath, path))
    |> List.filter(FindFiles.isCompiledFile)
    |> List.map(name => {
      let compiled = path /+ FindFiles.cmtName(~namespace=None, name);
      (Filename.chop_extension(name) |> String.capitalize_ascii, SharedTypes.Impl(compiled, Some(path /+ name)));
    })
  })
  |> List.concat;

  let dependencyModules = List.concat(otherFiles) @ dependencyModules;
  /* let pathsForModule = makePathsForModule(localModules, dependencyModules); */
  let (pathsForModule, nameForPath) = makePathsForModule(localModules, dependencyModules);
  Log.log("Depedency dirs " ++ String.concat(" ", dependencyDirectories));

  libraryName |?< libraryName => Hashtbl.replace(
    pathsForModule,
    String.capitalize_ascii(libraryName),
    Impl(compiledBase /+ libraryName ++ ".cmt", None)
  );

  let interModuleDependencies = Hashtbl.create(List.length(localModules));

  let buildCommand = switch (pkgMgr) {
    | Esy =>
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

  if (state.settings.autoRebuild) {
    runBuildCommand(~reportDiagnostics, state, rootPath, buildCommand);
  };

  let%try compilerPath = BuildSystem.getCompiler(projectRoot, buildSystem);
  let%try compilerVersion = BuildSystem.getCompilerVersion(compilerPath);
  let refmtPath = BuildSystem.getRefmt(projectRoot, buildSystem) |> RResult.toOptionAndLog;
  Ok({
    basePath: rootPath,
    localModules: localModules |. Belt.List.map(fst),
    rebuildTimer: 0.,
    interModuleDependencies,
    dependencyModules: dependencyModules |. Belt.List.map(fst),
    pathsForModule,
    namespace,
    nameForPath,
    buildSystem,
    buildCommand,
    /* TODO check if there's a module called that */
    opens: fold(libraryName, [], libraryName => [String.capitalize_ascii(libraryName)]),
    tmpPath: hiddenLocation,
    compilationFlags: flags |> String.concat(" "),
    includeDirectories: [compiledBase, ...otherDirectories] @ dependencyDirectories,
    compilerVersion,
    compilerPath,
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

let isMl = path =>
  Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".mli");

let odocToMd = text => MarkdownOfOCamldoc.convert(text);
let compose = (fn1, fn2, arg) => fn1(arg) |> fn2;

let converter = (src, usePlainText) => {
  let mlToOutput = compose(odocToMd, usePlainText ? Omd.to_text : Omd.to_markdown);
  fold(
    src,
    mlToOutput,
    src => isMl(src) ? mlToOutput : (usePlainText ? compose(Omd.of_string, Omd.to_text) : (x => x))
  );
};

let newDocsForCmt = (~compilerVersion, ~moduleName, cmtCache, changed, cmt, src, clientNeedsPlainText) => {
  let uri = Utils.toUri(src |? cmt);
  let%opt file = (switch compilerVersion {
    | BuildSystem.V402 => Process_402.fileForCmt
    | V406 => Process_406.fileForCmt
    | V407 => Process_407.fileForCmt
  })(~moduleName, cmt, uri, converter(src, clientNeedsPlainText)) |> RResult.toOptionAndLog;
  Hashtbl.replace(cmtCache, cmt, (changed, file));
  Some(file);
};

let newDocsForCmi = (~compilerVersion, ~moduleName, cmiCache, changed, cmi, src, clientNeedsPlainText) => {
  let%opt file = (switch compilerVersion {
    | BuildSystem.V402 => Process_402.fileForCmi
    | V406 => Process_406.fileForCmi
    | V407 => Process_407.fileForCmi
  })(~moduleName, cmi, Utils.toUri(src |? cmi), converter(src, clientNeedsPlainText));
  Hashtbl.replace(cmiCache, cmi, (changed, file));
  Some(file);
};

let hasProcessedCmt = (state, cmt) => Hashtbl.mem(state.cmtCache, cmt);

let docsForCmt = (~package, ~moduleName, cmt, src, state) =>
  if (Filename.check_suffix(cmt, ".cmi")) {
    if (Hashtbl.mem(state.cmiCache, cmt)) {
      let (mtime, docs) = Hashtbl.find(state.cmiCache, cmt);
      /* TODO I should really throttle this mtime checking to like every 50 ms or so */
      switch (Files.getMtime(cmt)) {
      | None =>
        Log.log("⚠️ cannot get docs for nonexistant cmi " ++ cmt);
        None;
      | Some(changed) =>
        if (changed > mtime) {
          newDocsForCmi(
            ~compilerVersion=package.compilerVersion,
            ~moduleName,
            state.cmiCache,
            changed,
            cmt,
            src,
            state.settings.clientNeedsPlainText,
          );
        } else {
          Some(docs);
        }
      };
    } else {
      switch (Files.getMtime(cmt)) {
      | None =>
        Log.log("⚠️ cannot get docs for nonexistant cmi " ++ cmt);
        None;
      | Some(changed) =>
        newDocsForCmi(
          ~compilerVersion=package.compilerVersion,
          ~moduleName,
          state.cmiCache,
          changed,
          cmt,
          src,
          state.settings.clientNeedsPlainText,
        )
      };
    };
  } else if (Hashtbl.mem(state.cmtCache, cmt)) {
    let (mtime, docs) = Hashtbl.find(state.cmtCache, cmt);
    /* TODO I should really throttle this mtime checking to like every 50 ms or so */
    switch (Files.getMtime(cmt)) {
    | None =>
      Log.log("⚠️ cannot get docs for nonexistant cmt " ++ cmt);
      None;
    | Some(changed) =>
      if (changed > mtime) {
        newDocsForCmt(
          ~compilerVersion=package.compilerVersion,
          ~moduleName,
          state.cmtCache,
          changed,
          cmt,
          src,
          state.settings.clientNeedsPlainText,
        );
      } else {
        Some(docs);
      }
    };
  } else {
    switch (Files.getMtime(cmt)) {
    | None =>
      Log.log("⚠️ cannot get docs for nonexistant cmt " ++ cmt);
      None;
    | Some(changed) =>
      newDocsForCmt(
          ~compilerVersion=package.compilerVersion,
          ~moduleName,
        state.cmtCache,
        changed,
        cmt,
        src,
        state.settings.clientNeedsPlainText,
      )
    };
  };

let updateContents = (uri, text, version, state) => {
  Hashtbl.remove(state.compiledDocuments, uri);
  Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), false));
  state
};

let getContents = (uri, state) => {
  let (text, _, _) = Hashtbl.find(state.documentText, uri);
  text
};

let refmtForUri = (uri, package) =>
  if (Filename.check_suffix(uri, ".ml")
      || Filename.check_suffix(uri, ".mli")) {
    RResult.Ok(None);
  } else if (Filename.check_suffix(uri, ".rel")
             || Filename.check_suffix(uri, ".reli")) {
    switch (package.lispRefmtPath) {
    | None =>
      Error("No lispRefmt path found, cannot process .rel or .reli files")
    | Some(x) => Ok(Some(x))
    };
  } else {
    switch (package.refmtPath) {
      | None => Error("No refmt found for dune project. Cannot process .re file")
      | Some(x) => Ok(Some(x))
    }
  };

open Infix;

let getInterfaceFile = (uri, state, ~package: TopTypes.package) => {
  let%try path = Utils.parseUri(uri) |> RResult.orError("Not a uri");
  let text = Hashtbl.mem(state.documentText, uri) ? {
    let (text, _, _) = Hashtbl.find(state.documentText, uri);
    text
  } : {
    let path = Utils.parseUri(uri) |! "not a uri: " ++ uri;
    Files.readFileExn(path)
  };
  let%try moduleName = switch (Utils.maybeHash(package.nameForPath, path)) {
    | None =>
      Hashtbl.iter((k, v) => Log.log("Path: " ++ k ++ "  " ++ v), package.nameForPath);
      Log.log("Can't find " ++ path);
      Error("Can't find module name for path " ++ path)
    | Some(x) => Ok(x)
  };
  let includes = state.settings.crossFileAsYouType
  ? [package.tmpPath, ...package.includeDirectories]
  : package.includeDirectories;
  let%try refmtPath = refmtForUri(uri, package);
  AsYouType.getInterface(
    ~moduleName,
    ~basePath=package.basePath,
    ~reasonFormat=switch (package.buildSystem) {
      | Bsb(_) | BsbNative(_, Js) => Utils.endsWith(uri, "re") || Utils.endsWith(uri, "rei")
      | _ => false
    },
    text,
    ~cacheLocation=package.tmpPath,
    package.compilerPath,
    refmtPath,
    includes,
    package.compilationFlags,
  );
};

let getCompilationResult = (uri, state, ~package: TopTypes.package) => {
  if (Hashtbl.mem(state.compiledDocuments, uri)) {
    Belt.Result.Ok(Hashtbl.find(state.compiledDocuments, uri))
  } else {
    let%try path = Utils.parseUri(uri) |> RResult.orError("Not a uri");
    let text = Hashtbl.mem(state.documentText, uri) ? {
      let (text, _, _) = Hashtbl.find(state.documentText, uri);
      text
    } : {
      let path = Utils.parseUri(uri) |! "not a uri: " ++ uri;
      Files.readFileExn(path)
    };
    let moduleName = BuildSystem.namespacedName(package.buildSystem, package.namespace, FindFiles.getName(path));
    /* let%try moduleName = switch (Utils.maybeHash(package.nameForPath, path)) {
      | None =>
        Hashtbl.iter((k, v) => Log.log("Path: " ++ k ++ "  " ++ v), package.nameForPath);
        Log.log("Can't find " ++ path ++ " in package " ++ package.basePath);
        Error("Can't find module name for path " ++ path)
      | Some(x) => Ok(x)
    }; */
    let includes = state.settings.crossFileAsYouType
    ? [package.tmpPath, ...package.includeDirectories]
    : package.includeDirectories;
    let%try refmtPath = refmtForUri(uri, package);
    let%try result = AsYouType.process(
      ~compilerVersion=package.compilerVersion,
      ~uri,
      ~moduleName,
      ~allLocations=state.settings.recordAllLocations,
      ~basePath=package.basePath,
      ~reasonFormat=switch (package.buildSystem) {
        | Bsb(_) | BsbNative(_, Js) => Utils.endsWith(uri, "re") || Utils.endsWith(uri, "rei")
        | _ => false
      },
      text,
      ~cacheLocation=package.tmpPath,
      package.compilerPath,
      refmtPath,
      includes,
      package.compilationFlags,
    );
    Hashtbl.replace(state.compiledDocuments, uri, result);
    switch (result) {
    | AsYouType.SyntaxError(_) => ()
    | AsYouType.TypeError(_, full) => {
      if (!Hashtbl.mem(state.lastDefinitions, uri)) {
        Log.log("<< Making lastDefinitions with type error for " ++ uri);
        Hashtbl.replace(state.lastDefinitions, uri, full)
      }
    }
    | Success(_, full) => {
      Log.log("<< Replacing lastDefinitions for " ++ uri);

      Hashtbl.replace(state.lastDefinitions, uri, full);
      Hashtbl.replace(package.interModuleDependencies, moduleName, SharedTypes.hashList(full.extra.externalReferences) |. Belt.List.map(fst));

      if (state.settings.crossFileAsYouType) {
        /** Check dependencies */
        package.localModules |. Belt.List.forEach((mname) => {
          let%opt_consume paths = Utils.maybeHash(package.pathsForModule, mname);
          let%opt_consume src = SharedTypes.getSrc(paths);
          let otherUri = Utils.toUri(src);
          if (mname != moduleName
              && List.mem(
                   moduleName,
                   Query.hashFind(package.interModuleDependencies, mname) |? [],
                 )) {
            Hashtbl.remove(state.compiledDocuments, otherUri);
            Hashtbl.replace(
              state.documentTimers,
              otherUri,
              Unix.gettimeofday() +. 0.01,
            );
          };
        });

        package.localModules |. Belt.List.forEach((mname) => {
          let%opt_consume paths = Utils.maybeHash(package.pathsForModule, mname);
          let%opt_consume src = SharedTypes.getSrc(paths);
          let otherUri = Utils.toUri(src);
          switch (Hashtbl.find(state.compiledDocuments, otherUri)) {
            | exception Not_found => ()
            | TypeError(_, {extra}) | Success(_, {extra}) => {
              if (Hashtbl.mem(extra.externalReferences, moduleName)) {
                Hashtbl.remove(state.compiledDocuments, otherUri);
                Hashtbl.replace(state.documentTimers, otherUri, Unix.gettimeofday() +. 0.01);
              }
            }
            | _ => ()
          }
        });
      }
    }
    };
    Ok(result)
  }
};

let getLastDefinitions = (uri, state) => switch (Hashtbl.find(state.lastDefinitions, uri)) {
| exception Not_found => None
| data =>
  Some(data)
};

let tryExtra = p => {
  let%try p = p;
  Ok(AsYouType.getResult(p))
};

/* If there's a previous "good" version, use that, otherwise use the current version */
let getBestDefinitions = (uri, state, ~package) => {
  if (Hashtbl.mem(state.lastDefinitions, uri)) {
    Belt.Result.Ok(Hashtbl.find(state.lastDefinitions, uri))
  } else {
    tryExtra(getCompilationResult(uri, state, ~package))
  }
};

let getDefinitionData = (uri, state, ~package) => {
  getCompilationResult(uri, state, ~package) |> tryExtra
};

let docsForModule = (modname, state, ~package) =>
    if (Hashtbl.mem(package.pathsForModule, modname)) {
      let paths = Hashtbl.find(package.pathsForModule, modname);
      /* TODO do better */
      let cmt = SharedTypes.getCmt(paths);
      let src = SharedTypes.getSrc(paths);
      Log.log("FINDING " ++ cmt ++ " src " ++ (src |? ""));
      let%opt_wrap docs = docsForCmt(~package, ~moduleName=modname, cmt, src, state);
      (docs, src)
    } else {
      Log.log("No path for module " ++ modname);
      None;
    };

let fileForUri = (state,  ~package, uri) => {
  let%try moduleData = getCompilationResult(uri, state, ~package) |> tryExtra;
  Ok((moduleData.file, moduleData.extra))
};

let fileForModule = (state,  ~package, modname) => {
  let file = state.settings.crossFileAsYouType ? {
    /* Log.log("✅ Gilr got mofilr " ++ modname); */
    Log.log(package.localModules |> String.concat(" "));
    let%opt paths = Utils.maybeHash(package.pathsForModule, modname);
    /* TODO do better? */
    let%opt src = SharedTypes.getSrc(paths);
    let uri = Utils.toUri(src);
    if (Hashtbl.mem(state.documentText, uri)) {
      let%opt {SharedTypes.file} = tryExtra(getCompilationResult(uri, state, ~package)) |> RResult.toOptionAndLog;
      Some(file)
    } else {
      None
    }
  } : None;
  switch file {
    | Some(_) => file
    | None =>
      let%opt (file, _) = docsForModule(modname, state, ~package);
      Some(file)
  }
};

let extraForModule = (state, ~package, modname) => {
  if (Hashtbl.mem(package.pathsForModule, modname)) {
    let paths = Hashtbl.find(package.pathsForModule, modname);
    /* TODO do better? */
    let%opt src = SharedTypes.getSrc(paths);
    let%opt {extra} = tryExtra(getCompilationResult(Utils.toUri(src), state, ~package)) |> RResult.toOptionAndLog;
    Some(extra)
  } else {
    None;
  }
};

let maybeFound = (fn, a) =>
  switch (fn(a)) {
  | exception Not_found => None
  | x => Some(x)
  };

let topLocation = uri => {
  Location.loc_ghost: false,
  loc_start: {
    Lexing.pos_fname: uri,
    pos_lnum: 1,
    pos_cnum: 1,
    pos_bol: 1,
  },
  loc_end: {
    Lexing.pos_fname: uri,
    pos_lnum: 1,
    pos_cnum: 1,
    pos_bol: 1,
  },
};
