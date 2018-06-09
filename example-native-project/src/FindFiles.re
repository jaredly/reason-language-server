open Infix;

let shouldExist = (message, v) => Files.exists(v) ? v : failwith(message ++ ": " ++ v);
let oneShouldExist = (message, items) => {
  let rec loop = left => switch left {
  | [] => failwith(message ++ " Looked at " ++ String.concat(", ", items))
  | [one, ...more] => Files.exists(one) ? one : loop(more)
  };
  loop(items)
};

let ifOneExists = (items) => {
  let rec loop = left => switch left {
  | [] => None
  | [one, ...more] => Files.exists(one) ? Some(one) : loop(more)
  };
  loop(items)
};


/**
 * Returns a list of paths, relative to the provided `base`
 *
 */
let getSourceDirectories = (~includeDev=false, base, config) => {
  let rec handleItem = (current, item) => {
    switch item {
    | Json.Array(contents) => List.map(handleItem(current), contents) |> List.concat
    | Json.String(text) => [current /+ text]
    | Json.Object(_) =>
      let dir = Json.get("dir", item) |?> Json.string |? "Must specify directory";
      let typ = includeDev ? "lib" : (item |> Json.get("type") |?> Json.string |? "lib");
      if (typ == "dev") {
        []
      } else {
        [current /+ dir, ...switch (item |> Json.get("subdirs")) {
        | None => []
        | Some(Json.True) => Files.collectDirs(base /+ current /+ dir) |> List.map(Files.relpath(base))
        | Some(item) => handleItem(current /+ dir, item)
        }]
      }
    | _ => failwith("Invalid subdirs entry")
    };
  };
  config |> Json.get("sources") |?>> handleItem("") |? []
};


let isNative = config => Json.get("entries", config) != None || Json.get("allowed-build-kinds", config) != None;

/**
 * Use this to get the directories you need to `-I` include to get things to compile.
 */
let getDependencyDirs = (base, config) => {
  let deps = config |> Json.get("bs-dependencies") |?> Json.array |? [] |> optMap(Json.string);
  deps |> List.map(name => {
    let loc = base /+ "node_modules" /+ name;
    switch (Files.readFile(loc /+ "bsconfig.json")) {
    | Some(text) =>
      let inner = Json.parse(text);
      /* let allowedKinds = inner |> Json.get("allowed-build-kinds") |?> Json.array |?>> List.map(Json.string |.! "allowed-build-kinds must be strings") |? ["js"]; */
      let isNative = isNative(inner);
      let compiledBase = oneShouldExist("Cannot find directory for compiled artifacts.",
        isNative
          ? [loc /+ "lib/bs/js", loc /+ "lib" /+ "bs" /+ "native"]
          : [loc /+ "lib/bs", loc /+ "lib" /+ "ocaml"]
      );
      /* if (List.mem("js", allowedKinds)) { */
        [compiledBase, ...(getSourceDirectories(loc, inner) |> List.map(name => compiledBase /+ name))];
      /* } else {
        []
      } */
    | None =>
      print_endline("Skipping nonexistent dependency: " ++ name);
      []
    }
  }) |> List.concat
};

let isCompiledFile = name =>
  Filename.check_suffix(name, ".cmt")
  || Filename.check_suffix(name, ".cmti");

let isSourceFile = name =>
  Filename.check_suffix(name, ".re")
  || Filename.check_suffix(name, ".rei")
  || Filename.check_suffix(name, ".ml")
  || Filename.check_suffix(name, ".mli");

let compiledNameSpace = name => Str.split(Str.regexp_string("-"), name) |> List.map(String.capitalize) |> String.concat("");

let compiledName = (~namespace, name) =>
  Filename.chop_extension(name)
  ++ (switch namespace { | None => "" | Some(n) => "-" ++ compiledNameSpace(n) })
  ++ (name.[String.length(name) - 1] == 'i' ? ".cmti" : ".cmt");

let getName = x => Filename.basename(x) |> Filename.chop_extension;

let filterDuplicates = cmts => {
  /* Remove .cmt's that have .cmti's */
  let intfs = Hashtbl.create(100);
  cmts |> List.iter(path => if (Filename.check_suffix(path, ".rei") || Filename.check_suffix(path, ".mli") || Filename.check_suffix(path, ".cmti")) {
    Hashtbl.add(intfs, getName(path), true)
  });
  cmts |> List.filter(path => {
    !((Filename.check_suffix(path, ".re") || Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".cmt")) && Hashtbl.mem(intfs, getName(path)))
  });
};

let ifDebug = (debug, name, fn, v) => {
  if (debug) {
    print_endline(name ++ ": " ++ fn(v))
  };
  v
};

let getNamespace = config => {
  let isNamespaced = Json.get("namespace", config) |?> Json.bool |? false;
  isNamespaced ? (config |> Json.get("name") |?> Json.string |! "name is required if namespace is true" |> String.capitalize |> s => Some(s)) : None;
};

let getCompiledBase = (root, config) => oneShouldExist("Cannot find directory for compiled artifacts.",
    isNative(config)
      ? [root /+ "lib/bs/native"]
      : [root /+ "lib/bs", root /+ "lib/ocaml"]
  );

/**
 * returns a list of (absolute path to cmt(i), relative path from base to source file)
 */
let findProjectFiles = (~debug, namespace, root, config, compiledBase) => {
  getSourceDirectories(~includeDev=true, root, config)
  |> ifDebug(debug, "Source directories from bsconfig", items => String.concat("\n", items))
  |> List.map(Infix.fileConcat(root))
  |> ifDebug(debug, "With root", items => String.concat("\n", items))
  |> List.map(name => Files.collect(name, isSourceFile))
  |> List.concat
  |> ifDebug(debug, "Source files found", String.concat(" : "))
  |> filterDuplicates
  |> List.map(path => {
    let rel = Files.relpath(root, path);
    (compiledBase /+ compiledName(~namespace, rel), path)
  })
  |> ifDebug(debug, "With compiled base", (items) => String.concat("\n", List.map(((a, b)) => a ++ " : " ++ b, items)))
  |> List.filter(((full, rel)) => Files.exists(full))
};

type modpath = Namespaced(string, string) | Plain(string);

let loadStdlib = stdlib => Files.readDirectory(stdlib)
  |> List.filter(isSourceFile)
  |> filterDuplicates
  |> List.map(path => (Plain(getName(path) |> String.capitalize), (stdlib /+ compiledName(~namespace=None, path), stdlib /+ path)))
  |> List.filter(((_, (cmt, src))) => Files.exists(cmt))
  ;

let needsCompilerLibs = config => {
  config |> Json.get("ocaml-dependencies") |?> Json.array |? [] |> optMap(Json.string) |> List.mem("compiler-libs")
};

let findDependencyFiles = (~debug, base, config) => {
  let deps = config |> Json.get("bs-dependencies") |?> Json.array |? [] |> optMap(Json.string);
  let depFiles = deps |> List.map(name => {
    let loc = base /+ "node_modules" /+ name;
    switch (Files.readFile(loc /+ "bsconfig.json")) {
    | Some(text) =>
      let inner = Json.parse(text);
      let namespace = getNamespace(config);
      let files = findProjectFiles(~debug, namespace, loc, inner, getCompiledBase(base, config));
      switch namespace {
      | None => List.map(((full, rel)) => (Plain(getName(rel) |> String.capitalize), (full, rel)), files)
      | Some(name) => files |> List.map(((full, rel)) => (Namespaced(name, getName(rel)), (full, rel)))
      }
    | None =>
      print_endline("Skipping nonexistent dependency: " ++ name);
      []
    }
  }) |> List.concat;
  let results = depFiles @ loadStdlib(base /+ "node_modules/bs-platform/lib/ocaml");
  needsCompilerLibs(config)
  ? loadStdlib(base /+ "node_modules/bs-platform/vendor/ocaml/lib/ocaml/compiler-libs")
  : results
};

