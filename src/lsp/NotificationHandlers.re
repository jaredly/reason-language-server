
open Infix;
open Result;
open Log;
open TopTypes;

let recompileDebounceTime = 0.5; /* seconds */

let getTextDocument = doc => {
  let%opt uri = Json.get("uri", doc) |?> Json.string;
  let%opt version = Json.get("version", doc) |?> Json.number;
  let%opt text = Json.get("text", doc) |?> Json.string;
  Some((uri, version, text))
};

let checkPackageTimers = state => {
  Hashtbl.iter((_, package) => {
    if (package.rebuildTimer != 0. && package.rebuildTimer < Unix.gettimeofday()) {
      package.rebuildTimer = 0.;
      State.runBuildCommand(state, package.basePath, package.buildCommand);
    }
  }, state.packagesByRoot)
};

let setPackageTimer = package => {
  if (package.rebuildTimer == 0.) {
    package.rebuildTimer = Unix.gettimeofday() +. 0.01;
  }
};

let watchedFileContentsMap = Hashtbl.create(100);

let reloadAllState = state => {
  Log.log("RELOADING ALL STATE");
  Hashtbl.iter(
    (uri, _) => Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() +. recompileDebounceTime),
    state.documentText,
  );
  {
    ...TopTypes.empty(),
    documentText: state.documentText,
    documentTimers: state.documentTimers,
    settings: state.settings,
  };
};

let notificationHandlers: list((string, (state, Json.t) => result(state, string))) = [
  ("textDocument/didOpen", (state, params) => {
    let%try (uri, version, text) = Json.get("textDocument", params) |?> getTextDocument |> Result.orError("Invalid params");
    Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), true));
    Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() +. recompileDebounceTime);
    
    let%try path = Utils.parseUri(uri) |> Result.orError("Invalid uri");
    if (FindFiles.isSourceFile(path)) {
      let%try package = State.getPackage(uri, state);
      let name = FindFiles.getName(path);
      if (!Hashtbl.mem(package.nameForPath, name)) {
        Ok(reloadAllState(state))
        /* Hashtbl.add(package.nameForPath, path, name);
        Hashtbl.add(package.pathsForModule, name, Impl(path, Some(path)));
        Hashtbl.replace(state.packagesByRoot, package.basePath, {
          ...package,
          localModules: [name, ...package.localModules]
        });
        Ok(state) */
      } else {
        Ok(state)
      }
    } else {
      Ok(state)
    }
  }),
  ("workspace/didChangeConfiguration", (state, params) => {
    let nullIfEmpty = item => item == "" ? None : Some(item);
    let settings = params |> Json.get("settings") |?> Json.get("reason_language_server");
    let refmtLocation = (settings |?> Json.get("refmt") |?> Json.string) |?> nullIfEmpty;
    let lispRefmtLocation = (settings |?> Json.get("lispRefmt") |?> Json.string |?> nullIfEmpty);
    let perValueCodelens = (settings |?> Json.get("per_value_codelens") |?> Json.bool) |? false;
    let opensCodelens = (settings |?> Json.get("opens_codelens") |?> Json.bool) |? true;
    let dependenciesCodelens = (settings |?> Json.get("dependencies_codelens") |?> Json.bool) |? true;
    let formatWidth = (settings |?> Json.get("format_width") |?> Json.number) |?>> int_of_float;
    /* let crossFileAsYouType = (settings |?> Json.get("cross_file_as_you_type") |?> Json.bool) |? false; */
    /* Disabling this -- too finnicky :/ */
    let crossFileAsYouType = false;
    Ok({
      ...state,
      settings: {
        ...state.settings,
        perValueCodelens,
        refmtLocation,
        lispRefmtLocation,
        opensCodelens,
        formatWidth,
        dependenciesCodelens,
        crossFileAsYouType,
      },
    });
  }),
  ("textDocument/didSave", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> doc => RJson.get("uri", doc) |?> RJson.string;
    let%try package = State.getPackage(uri, state);
    setPackageTimer(package);
    let moduleName = FindFiles.getName(uri);

    package.localModules |. Belt.List.forEach((mname) => {
      let%opt_consume paths = Utils.maybeHash(package.pathsForModule, mname);
      let%opt_consume src = TopTypes.getSrc(paths);
      let otherUri = Utils.toUri(src);
      let refs = Query.hashFind(package.interModuleDependencies, mname);
      open Infix;
      if (mname != moduleName
          && (
            List.mem(
                moduleName,
                refs |? [],
              )
            || switch (Hashtbl.find(state.compiledDocuments, otherUri)) {
              | exception Not_found => true
              | Success(_) => false
              | SyntaxError(_) => false
              | TypeError(_) => true
            }
        )
      ) {
        Hashtbl.remove(state.compiledDocuments, otherUri);
        Hashtbl.replace(
          state.documentTimers,
          otherUri,
          Unix.gettimeofday() +. 0.015,
        );
      };
    });

    Ok(state)
  }),
  ("textDocument/didChange", (state, params) => {
    open InfixResult;
    let%try doc = params |> RJson.get("textDocument");
    let%try uri = RJson.get("uri", doc) |?> RJson.string;
    let%try version = RJson.get("version", doc) |?> RJson.number;
    let%try changes = RJson.get("contentChanges", params) |?> RJson.array;
    let%try text = List.nth(changes, List.length(changes) - 1) |> RJson.get("text") |?> RJson.string;
    /* Hmm how do I know if it's modified? */
    let state = State.updateContents(uri, text, version, state);
    Hashtbl.replace(state.documentTimers, uri, Unix.gettimeofday() +. recompileDebounceTime);
    Ok(state)
  }),
  ("workspace/didChangeWatchedFiles", (state, params) => {
    Log.log("Got a watched file change");
    let%try changes = RJson.get("changes", params);
    let%try changes = RJson.array(changes);
    open InfixResult;
    let shouldReload = Belt.List.some(changes, change => {
      let%try t = RJson.get("type", change) |?> RJson.number;
      let%try uri = RJson.get("uri", change) |?> RJson.string;
      let isRelevant =
        Utils.endsWith(uri, "/bsconfig.json") ||
        Utils.endsWith(uri, "/jbuild") ||
        Utils.endsWith(uri, "/dune");
      if (!isRelevant) {
        Ok(false)
      } else {
        let%try path = Utils.parseUri(uri) |> Result.orError("Cannot parse URI");
        let%try contents = Files.readFileResult(path);
        if (Hashtbl.mem(watchedFileContentsMap, uri) && Hashtbl.find(watchedFileContentsMap, uri) == contents) {
          Ok(false)
        } else {
          Hashtbl.replace(watchedFileContentsMap, uri, contents);
          Ok(true)
        }
      }
    } |? false);

    if (shouldReload) {
      Ok(reloadAllState(state))
    } else {
      Ok(state)
    }
  })
];
