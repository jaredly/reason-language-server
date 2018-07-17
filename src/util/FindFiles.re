open Infix;

let shouldExist = (message, v) =>
  Files.exists(v) ? v : failwith(message ++ ": " ++ v);

let oneShouldExist = (message, items) => {
  let rec loop = left =>
    switch (left) {
    | [] => failwith(message ++ " Looked at " ++ String.concat(", ", items))
    | [one, ...more] => Files.exists(one) ? one : loop(more)
    };
  loop(items);
};

let ifOneExists = items => {
  let rec loop = left =>
    switch (left) {
    | [] => None
    | [one, ...more] => Files.exists(one) ? Some(one) : loop(more)
    };
  loop(items);
};

let ifDebug = (debug, name, fn, v) => {
  if (debug) {
    Log.log(name ++ ": " ++ fn(v));
  };
  v;
};

/**
 * Returns a list of paths, relative to the provided `base`
 *
 */
let getSourceDirectories = (~includeDev=false, base, config) => {
  let rec handleItem = (current, item) =>
    switch (item) {
    | Json.Array(contents) =>
      List.map(handleItem(current), contents) |> List.concat
    | Json.String(text) => [current /+ text]
    | Json.Object(_) =>
      let dir =
        Json.get("dir", item) |?> Json.string |? "Must specify directory";
      let typ =
        includeDev ?
          "lib" : item |> Json.get("type") |?> Json.string |? "lib";
      if (typ == "dev") {
        [];
      } else {
        switch (item |> Json.get("subdirs")) {
        | None => [current /+ dir]
        | Some(Json.True) =>
          Files.collectDirs(base /+ current /+ dir)
          /* |> ifDebug(true, "Subdirs", String.concat(" - ")) */
          |> List.filter(name => name != Filename.current_dir_name)
          |> List.map(Files.relpath(base))
        | Some(item) => [
            current /+ dir,
            ...handleItem(current /+ dir, item),
          ]
        };
      };
    | _ => failwith("Invalid subdirs entry")
    };
  config |> Json.get("sources") |?>> handleItem("") |? [];
};

/**
 * Use this to get the directories you need to `-I` include to get things to compile.
 */
let getDependencyDirs = (base, config, ~buildSystem) => {
  let deps =
    config
    |> Json.get("bs-dependencies")
    |?> Json.array
    |? []
    |> optMap(Json.string);
  deps
  |> List.map(name => {
       let loc = base /+ "node_modules" /+ name;
       switch (Files.readFile(loc /+ "bsconfig.json")) {
       | Some(text) =>
         let inner = Json.parse(text);
         /* let allowedKinds = inner |> Json.get("allowed-build-kinds") |?> Json.array |?>> List.map(Json.string |.! "allowed-build-kinds must be strings") |? ["js"]; */
         let compiledBase =
           BuildSystem.getCompiledBase(loc, buildSystem)
           |! "Cannot find directory for compiled artifacts.";
         /* if (List.mem("js", allowedKinds)) { */
         [
           compiledBase,
           ...getSourceDirectories(loc, inner)
              |> List.map(name => compiledBase /+ name),
         ];
       /* } else {
            []
          } */
       | None =>
         Log.log("Skipping nonexistent dependency: " ++ name);
         [];
       };
     })
  |> List.concat;
};

let isCompiledFile = name =>
  Filename.check_suffix(name, ".cmt")
  || Filename.check_suffix(name, ".cmi")
  || Filename.check_suffix(name, ".cmti");

let isSourceFile = name =>
  Filename.check_suffix(name, ".re")
  || Filename.check_suffix(name, ".rei")
  || Filename.check_suffix(name, ".ml")
  || Filename.check_suffix(name, ".mli");

let compiledNameSpace = name =>
  Str.split(Str.regexp_string("-"), name)
  |> List.map(String.capitalize)
  |> String.concat("");

let compiledBase = (~namespace, name) =>
  Filename.chop_extension(name)
  ++ (
    switch (namespace) {
    | None => ""
    | Some(n) => "-" ++ compiledNameSpace(n)
    }
  );

let cmtName = (~namespace, name) =>
  compiledBase(~namespace, name)
  ++ (name.[String.length(name) - 1] == 'i' ? ".cmti" : ".cmt");

let cmiName = (~namespace, name) => compiledBase(~namespace, name) ++ ".cmi";

let getName = x => Filename.basename(x) |> Filename.chop_extension;

let filterDuplicates = cmts => {
  /* Remove .cmt's that have .cmti's */
  let intfs = Hashtbl.create(100);
  cmts
  |> List.iter(path =>
       if (Filename.check_suffix(path, ".rei")
           || Filename.check_suffix(path, ".mli")
           || Filename.check_suffix(path, ".cmti")) {
         Hashtbl.add(intfs, getName(path), true);
       }
     );
  cmts
  |> List.filter(path =>
       ! (
         (
           Filename.check_suffix(path, ".re")
           || Filename.check_suffix(path, ".ml")
           || Filename.check_suffix(path, ".cmt")
           || Filename.check_suffix(path, ".cmi")
         )
         && Hashtbl.mem(intfs, getName(path))
       )
     );
};

let getNamespace = config => {
  let isNamespaced = Json.get("namespace", config) |?> Json.bool |? false;
  isNamespaced ?
    config
    |> Json.get("name")
    |?> Json.string
    |! "name is required if namespace is true"
    |> String.capitalize
    |> (s => Some(s)) :
    None;
};

let collectFiles = (~compiledTransform=x => x, ~sourceDirectory=?, directory) => {
  let allFiles = Files.readDirectory(directory);
  let compileds = allFiles |> List.filter(isCompiledFile) |> filterDuplicates;
  let sources =
    fold(sourceDirectory, allFiles, Files.readDirectory)
    |> List.filter(isSourceFile)
    |> filterDuplicates;
  let sourceBase = sourceDirectory |? directory;
  compileds
  |> List.map(path => {
       let modName = getName(path);
       let moduleName = modName |> String.capitalize;
       let compiled = directory /+ path;
       let source =
         Utils.find(
           name =>
             compiledTransform(getName(name)) == modName ?
               Some(sourceBase /+ name) : None,
           sources,
         );
       (moduleName, (compiled, source));
     });
};

/**
 * returns a list of (absolute path to cmt(i), relative path from base to source file)
 */
let findProjectFiles =
    (~debug, namespace, root, sourceDirectories, compiledBase) =>
  sourceDirectories
  |> List.map(Infix.fileConcat(root))
  |> ifDebug(debug, "Source directories", String.concat(" - "))
  |> List.map(name => Files.collect(name, isSourceFile))
  |> List.concat
  |> ifDebug(debug, "Source files found", String.concat(" : "))
  |> filterDuplicates
  |> Utils.filterMap(path => {
       let rel = Files.relpath(root, path);
       ifOneExists([
         compiledBase /+ cmtName(~namespace, rel),
         compiledBase /+ cmiName(~namespace, rel),
       ])
       |?>> (cm => (cm, path));
     })
  |> ifDebug(debug, "With compiled base", items =>
       String.concat("\n", List.map(((a, b)) => a ++ " : " ++ b, items))
     )
  |> List.filter(((full, rel)) => Files.exists(full));

type modpath =
  | Namespaced(string, string)
  | Plain(string);

let loadStdlib = stdlib =>
  collectFiles(stdlib)
  |> List.filter(((_, (cmt, src))) => Files.exists(cmt))
  |> List.map(((name, s)) => (Plain(name), s));

let needsCompilerLibs = config =>
  config
  |> Json.get("ocaml-dependencies")
  |?> Json.array
  |? []
  |> optMap(Json.string)
  |> List.mem("compiler-libs");

let findDependencyFiles = (~debug, ~buildSystem, base, config) => {
  let deps =
    config
    |> Json.get("bs-dependencies")
    |?> Json.array
    |? []
    |> optMap(Json.string);
  Log.log("Deps " ++ String.concat(", ", deps));
  let depFiles =
    deps
    |> List.map(name => {
         let loc = base /+ "node_modules" /+ name;
         let innerPath = loc /+ "bsconfig.json";
         Log.log("Dep loc " ++ innerPath);
         switch (Files.readFile(innerPath)) {
         | Some(text) =>
           let inner = Json.parse(text);
           let namespace = getNamespace(inner);
           let directories =
             getSourceDirectories(~includeDev=false, loc, inner);
           let compiledBase =
             BuildSystem.getCompiledBase(loc, buildSystem)
             |! "No compiled base found";
           if (debug) {
             Log.log("Compiled base: " ++ compiledBase);
           };
           let compiledDirectories =
             directories |> List.map(Infix.fileConcat(compiledBase));
           let compiledDirectories =
             namespace == None ?
               compiledDirectories : [compiledBase, ...compiledDirectories];
           let files =
             findProjectFiles(
               ~debug,
               namespace,
               loc,
               directories,
               compiledBase,
             );
           let files =
             switch (namespace) {
             | None =>
               List.map(
                 ((full, rel)) => (
                   Plain(getName(rel) |> String.capitalize),
                   (full, Some(rel)),
                 ),
                 files,
               )
             | Some(name) =>
               files
               |> List.map(((full, rel)) =>
                    (Namespaced(name, getName(rel)), (full, Some(rel)))
                  )
             };
           (compiledDirectories, files);
         | None =>
           Log.log("Skipping nonexistent dependency: " ++ name);
           ([], []);
         };
       });
  let (directories, files) = List.split(depFiles);
  let files = List.concat(files);
  let%try stdlibDirectories = BuildSystem.getStdlib(base, buildSystem);
  let directories = stdlibDirectories @ List.concat(directories);
  let results = files @ List.concat(List.map(loadStdlib, stdlibDirectories));
  let%try bsPlatformDir = BuildSystem.getBsPlatformDir(base);
  Result.Ok((
    needsCompilerLibs(config) ?
      [
        bsPlatformDir
        /+ "vendor"
        /+ "ocaml"
        /+ "lib"
        /+ "ocaml"
        /+ "compiler-libs",
        ...directories,
      ] :
      directories,
    needsCompilerLibs(config) ?
      loadStdlib(
        bsPlatformDir
        /+ "vendor"
        /+ "ocaml"
        /+ "lib"
        /+ "ocaml"
        /+ "compiler-libs",
      )
      @ results :
      results,
  ));
};