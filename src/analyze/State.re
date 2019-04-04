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
