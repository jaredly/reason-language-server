open Infix;

open TopTypes;

module Show = {
  let state = ({rootPath}, {localModules, compilerPath, dependencyModules}) => {
    "Root: " ++ rootPath ++
    "\nLocal\n"++
    (Belt.List.map(localModules, ((name, (cmt, src))) => Printf.sprintf("%s (%s : %s)", name, cmt, src)) |> String.concat("\n"))
    ++
    "\nDeps\n" ++ 
    (Belt.List.map(dependencyModules, ((modpath, (cmt, src))) => Printf.sprintf("%s (%s : %s)", modpath, cmt, src |? "")) |> String.concat("\n"))
  };
};

let makePathsForModule = (localModules, dependencyModules) => {
  let pathsForModule = Hashtbl.create(30);
  dependencyModules |> List.iter(((modName, (cmt, source))) => {
    Log.log("Dependency " ++ cmt ++ " - " ++ Infix.(source |? ""));
    Hashtbl.replace(pathsForModule, modName, (cmt, source))
  });

  localModules |> List.iter(((modName, (cmt, source))) => {
    Log.log("> Local " ++ modName ++ " at " ++ cmt ++ " - " ++ source);
    Hashtbl.replace(pathsForModule, modName, (cmt, Some(source)))
  });

  pathsForModule

};

let newBsPackage = (rootPath) => {
  let%try raw = Files.readFileResult(rootPath /+ "bsconfig.json");
  let config = Json.parse(raw);

  let%try buildSystem = BuildSystem.detect(rootPath, config);

  let compiledBase = BuildSystem.getCompiledBase(rootPath, buildSystem);
  let%try stdLibDirectories = BuildSystem.getStdlib(rootPath, buildSystem);
  let%try compilerPath = BuildSystem.getCompiler(rootPath, buildSystem);
  let%try refmtPath = BuildSystem.getRefmt(rootPath, buildSystem);
  let%try tmpPath = BuildSystem.hiddenLocation(rootPath, buildSystem);
  let%try (dependencyDirectories, dependencyModules) = FindFiles.findDependencyFiles(~debug=true, ~buildSystem, rootPath, config);
  let%try_wrap compiledBase = compiledBase |> Result.orError("You need to run bsb first so that reason-language-server can access the compiled artifacts.\nOnce you've run bsb, restart the language server.");

  let namespace = FindFiles.getNamespace(config);
  let localSourceDirs = FindFiles.getSourceDirectories(~includeDev=true, rootPath, config);
  Log.log("Got source directories " ++ String.concat(" - ", localSourceDirs));
  let localCompiledDirs = localSourceDirs |> List.map(Infix.fileConcat(compiledBase));
  let localCompiledDirs = namespace == None ? localCompiledDirs : [compiledBase, ...localCompiledDirs];

  let localModules = FindFiles.findProjectFiles(~debug=true, namespace, rootPath, localSourceDirs, compiledBase) |> List.map(((full, rel)) => (FindFiles.namespacedName(~namespace, rel), (full, rel)));
  
  let pathsForModule = makePathsForModule(localModules, dependencyModules);

  let opens = switch (namespace) {
    | None => []
    | Some(namespace) => {
      let cmt = compiledBase /+ namespace ++ ".cmt";
      /* Log.log("Namespaced as " ++ namespace ++ " at " ++ cmt); */
      Hashtbl.add(pathsForModule, namespace, (cmt, None));
      [FindFiles.nameSpaceToName(namespace)]
    }
  };
  Log.log("Depedency dirs " ++ String.concat(" ", dependencyDirectories));

  let flags = MerlinFile.getFlags(rootPath) |> Result.withDefault([""]);
  let flags = switch buildSystem {
    | Bsb(_) | BsbNative(_, Js) => {
      let jsPackageMode = config
      |> Json.get("package-specs")
      |?> Json.nth(0)
      |?> Json.get("module")
      |?> Json.string
      |? "commonjs";
      let flags = jsPackageMode == "es6" ? [
        "-bs-package-name",
        config |> Json.get("name") |?> Json.string |? "unnamed",
        "-bs-package-output",
        "es6:src",
        ...flags] : flags;
      ["-bs-no-builtin-ppx-ml", ...flags];
    }
    | _ => flags
  };

  let interModuleDependencies = Hashtbl.create(List.length(localModules));
  /* localModules |. Belt.List.forEach(((name, _)) => {
    Hashtbl.add(interModuleDependencies, name, Hashtbl.create(10))
  }); */

  {
    basePath: rootPath,
    localModules,
    dependencyModules,
    pathsForModule,
    buildSystem,
    opens,
    tmpPath,
    compilationFlags: flags |> String.concat(" "),
    interModuleDependencies,
    includeDirectories: 
      stdLibDirectories @ 
      dependencyDirectories @ localCompiledDirs,
    compilerPath,
    refmtPath,
  };
};

let newJbuilderPackage = (rootPath) => {
  let rec findJbuilderProjectRoot = path => {
    if (path == "/") {
      Result.Error("Unable to find _build directory")
    } else if (Files.exists(path /+ "_build")) {
      Ok(path)
    } else {
      findJbuilderProjectRoot(Filename.dirname(path))
    }
  };
  let%try projectRoot = findJbuilderProjectRoot(Filename.dirname(rootPath));
  let buildDir = projectRoot /+ "_build";
  let%try merlinRaw = Files.readFileResult(rootPath /+ ".merlin");
  let (source, build, flags) = MerlinFile.parseMerlin("", merlinRaw);

  let buildSystem = BuildSystem.Dune;

  let%try jbuildRaw = Files.readFileResult(rootPath /+ "jbuild");
  let jbuildConfig = JbuildFile.parse(jbuildRaw);
  let packageName = JbuildFile.findName(jbuildConfig);

  let%try ocamllib = BuildSystem.getLine("esy sh -c 'echo $OCAMLLIB'", buildDir);

  /* TODO support binaries, and other directories */

  let sourceFiles = Files.readDirectory(rootPath) |> List.filter(FindFiles.isSourceFile);
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

  let localModules = sourceFiles |> List.map(filename => {
    let name = FindFiles.getName(filename) |> String.capitalize;
    let namespaced = switch packageName {
      | `NoName | `Executable(_) => name
      | `Library(libraryName) => libraryName ++ "__" ++ name
    };
    (
      namespaced,
      (
        compiledBase
        /+ (
          fold(libraryName, "", l => l ++ "__")
          ++ Filename.chop_extension(filename)
          ++ ".cmt"
        ),
        rootPath /+ filename,
      ),
    );
  });

  let (otherDirectories, otherFiles) = source |> List.filter(s => s != "." && s != "" && s.[0] == '.') |> optMap(name => {
    let otherPath = rootPath /+ name;
    let res = {
      let%try jbuildRaw = Files.readFileResult(otherPath /+ "jbuild");
      let jbuildConfig = JbuildFile.parse(jbuildRaw);
      let%try libraryName = JbuildFile.findName(jbuildConfig) |> n => switch n {
        | `Library(name) => Result.Ok(name)
        | _ => Error("Not a library")
      };
      let rel = Files.relpath(projectRoot, otherPath);
      let compiledBase = buildDir /+ "default" /+ rel /+ "." ++ libraryName ++ ".objs";
      Log.log("Other directory: " ++ compiledBase);
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

  let dependencyDirectories = [ocamllib, ...(source |> List.filter(s => s != "" && s.[0] != '.'))];

  let%try hiddenLocation = BuildSystem.hiddenLocation(projectRoot, buildSystem);
  Files.mkdirp(hiddenLocation);

  let dependencyModules = dependencyDirectories
  |> List.map(path => {
    Log.log(">> Collecting deps for " ++ path);
    Files.readDirectory(Infix.maybeConcat(rootPath, path))
    |> List.filter(FindFiles.isSourceFile)
    |> List.map(name => {
      let compiled = path /+ FindFiles.cmtName(~namespace=None, name);
      (Filename.chop_extension(name) |> String.capitalize, (compiled, Some(path /+ name)));
    })
  })
  |> List.concat;

  let dependencyModules = List.concat(otherFiles) @ dependencyModules;
  let pathsForModule = makePathsForModule(localModules, dependencyModules);
  Log.log("Depedency dirs " ++ String.concat(" ", dependencyDirectories));

  libraryName |?< libraryName => Hashtbl.replace(pathsForModule, libraryName ++ "__", (compiledBase /+ libraryName ++ "__.cmt", None));

  let interModuleDependencies = Hashtbl.create(List.length(localModules));
  /* localModules |. Belt.List.forEach(((name, _)) => {
    Hashtbl.add(interModuleDependencies, name, Hashtbl.create(10))
  }); */

  let%try compilerPath = BuildSystem.getCompiler(projectRoot, buildSystem);
  let%try refmtPath = BuildSystem.getRefmt(projectRoot, buildSystem);
  Ok({
    basePath: rootPath,
    localModules,
    interModuleDependencies,
    dependencyModules,
    pathsForModule,
    buildSystem,
    /* TODO check if there's a module called that */
    opens: fold(libraryName, [], libraryName => [libraryName ++ "__"]),
    tmpPath: hiddenLocation,
    compilationFlags: flags |> String.concat(" "),
    includeDirectories: [compiledBase, ...otherDirectories] @ dependencyDirectories,
    compilerPath,
    refmtPath,
  });
};






let findRoot = (uri, packagesByRoot) => {
  let%opt path = Utils.parseUri(uri);
  let rec loop = path => {
    if (path == "/") {
      None
    } else if (Hashtbl.mem(packagesByRoot, path)) {
      Some(`Root(path))
    } else if (Files.exists(path /+ "bsconfig.json")) {
      Some(`Bs(path))
      /* jbuilder */
    } else if (Files.exists(path /+ ".merlin")) {
      Some(`Jbuilder(path))
    } else {
      loop(Filename.dirname(path))
    }
  };
  loop(Filename.dirname(path))
};

let getPackage = (uri, state) => {
  if (Hashtbl.mem(state.rootForUri, uri)) {
    Result.Ok(Hashtbl.find(state.packagesByRoot, Hashtbl.find(state.rootForUri, uri)))
  } else {
    let%try root = findRoot(uri, state.packagesByRoot) |> Result.orError("No root directory found");
    switch root {
    | `Root(rootPath) =>
      Hashtbl.replace(state.rootForUri, uri, rootPath);
      Result.Ok(Hashtbl.find(state.packagesByRoot, Hashtbl.find(state.rootForUri, uri)))
    | `Bs(rootPath) =>
      let%try package = newBsPackage(rootPath);
      let package = {...package, refmtPath: state.settings.refmtLocation |? package.refmtPath};
      Hashtbl.replace(state.rootForUri, uri, package.basePath);
      Hashtbl.replace(state.packagesByRoot, package.basePath, package);
      Result.Ok(package)
    | `Jbuilder(path) =>
      let%try package = newJbuilderPackage(path);
      Hashtbl.replace(state.rootForUri, uri, package.basePath);
      Hashtbl.replace(state.packagesByRoot, package.basePath, package);
      Result.Ok(package)
    }
  }
};

let isMl = path =>
  Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".mli");

let odocToMd = text => MarkdownOfOCamldoc.convert(0, text);
let compose = (fn1, fn2, arg) => fn1(arg) |> fn2;

let converter = (src, usePlainText) => {
  let mlToOutput = compose(odocToMd, usePlainText ? Omd.to_text : Omd.to_markdown);
  fold(
    src,
    mlToOutput,
    src => isMl(src) ? mlToOutput : (usePlainText ? compose(Omd.of_string, Omd.to_text) : (x => x))
  );
};

let newDocsForCmt = (cmtCache, changed, cmt, src, clientNeedsPlainText) => {
  let infos = Cmt_format.read_cmt(cmt);
  /* let%opt src = src; */
  let uri = Utils.toUri(src |? cmt);
  let%opt file = ProcessCmt.forCmt(uri, converter(src, clientNeedsPlainText), infos) |> Result.toOptionAndLog;
  Hashtbl.replace(cmtCache, cmt, (changed, file));
  Some(file);
};

let newDocsForCmi = (cmiCache, changed, cmi, src, clientNeedsPlainText) => {
  let infos = Cmi_format.read_cmi(cmi);
  /* switch (Docs.forCmi(converter(src, clientNeedsPlainText), infos)) {
  | None => {Log.log("Docs.forCmi gave me nothing " ++ cmi);None}
  | Some((docstring, items)) => */
    let%opt file = ProcessCmt.forCmi(Utils.toUri(src |? cmi), converter(src, clientNeedsPlainText), infos);
    /* let docs = Docs.moduleDocs(docstring, items, file); */
    Hashtbl.replace(cmiCache, cmi, (changed, file));
    Some(file);
  /* }; */
};

let hasProcessedCmt = (state, cmt) => Hashtbl.mem(state.cmtCache, cmt);

let docsForCmt = (cmt, src, state) =>
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

open Infix;
let getCompilationResult = (uri, state, ~package) => {
  if (Hashtbl.mem(state.compiledDocuments, uri)) {
    Belt.Result.Ok(Hashtbl.find(state.compiledDocuments, uri))
  } else {
    let text = Hashtbl.mem(state.documentText, uri) ? {
      let (text, _, _) = Hashtbl.find(state.documentText, uri);
      text
    } : {
      let path = Utils.parseUri(uri) |! "not a uri: " ++ uri;
      Files.readFileExn(path)
    };
    let moduleName = Utils.parseUri(uri) |! "not a uri" |> FindFiles.getName;
    let includes = state.settings.crossFileAsYouType
    ? [package.tmpPath, ...package.includeDirectories]
    : package.includeDirectories;
    let%try result = AsYouType.process(
      ~uri,
      ~moduleName,
      text,
      ~cacheLocation=package.tmpPath,
      package.compilerPath,
      package.refmtPath,
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
        package.localModules |. Belt.List.forEach(((mname, (cmt, src))) => {
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


        package.localModules |. Belt.List.forEach(((mname, (cmt, src))) => {
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
      let (cmt, src) = Hashtbl.find(package.pathsForModule, modname);
      Log.log("FINDING " ++ cmt ++ " src " ++ (src |? ""));
      let%opt_wrap docs = docsForCmt(cmt, src, state);
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
    Log.log(package.localModules |> List.map(fst) |> String.concat(" "));
    let%opt (cmt, src) = Belt.List.getAssoc(package.localModules, modname, (==));
    /* Log.log("Found it " ++ src); */
    let uri = Utils.toUri(src);
    if (Hashtbl.mem(state.documentText, uri)) {
      let%opt {SharedTypes.file} = tryExtra(getCompilationResult(uri, state, ~package)) |> Result.toOptionAndLog;
      Some(file)
    } else {
      None
    }
  } : None;
  switch file {
    | Some(f) => file
    | None =>
      let%opt (file, _) = docsForModule(modname, state, ~package);
      Some(file)
  }
};

let extraForModule = (state, ~package, modname) => {
  if (Hashtbl.mem(package.pathsForModule, modname)) {
    let (cmt, src) = Hashtbl.find(package.pathsForModule, modname);
    let%opt src = src;
    let%opt {file, extra} = tryExtra(getCompilationResult(Utils.toUri(src), state, ~package)) |> Result.toOptionAndLog;
    Some(extra)
  } else {
    None;
  }
};

let maybeFound = Definition.maybeFound;

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
