

/* TODO maybe track the loc's of these things */
type visibilityPath =
| File(string)
| NotVisible
| ExportedModule(string, visibilityPath)
| HiddenModule(string, visibilityPath)
| Expression(visibilityPath);

/* TODO maybe keep track of the "current module path" */
type declared('t) = {
  name: Location.loc(string),
  extentLoc: Location.t,
  scopeLoc: Location.t,
  stamp: int,
  deprecated: option(string),
  modulePath: visibilityPath,
  exported: bool,
  docstring: option(string),
  contents: 't,
  /* TODO maybe add a uri? */
  /* scopeType: scope, */
  /* scopeStart: (int, int), */
};

let emptyDeclared = (name) => {
  name: Location.mknoloc(name),
  extentLoc: Location.none,
  scopeLoc: Location.none,
  stamp: 0,
  deprecated: None,
  modulePath: NotVisible,
  exported: false,
  docstring: None,
  contents: ()
};

module Type = {
  module Attribute = {
    type t = {
      stamp: int,
      name: Location.loc(string),
      typ: Types.type_expr,
      typLoc: Location.t,
    };
  };

  module Constructor = {
    type t = {
      stamp: int,
      name: Location.loc(string),
      args: list((Types.type_expr, Location.t)),
      res: option(Types.type_expr),
    };
    open Infix;
    let show = ({name: {txt}, args, res}) => {
      txt ++ (args == []
        ? ""
        : "(" ++ String.concat(", ", args |. Belt.List.map(((typ, _)) => (
          PrintType.default.expr(PrintType.default, typ) |> PrintType.prettyString
        ))) ++ ")")
      ++ ((res |?>> typ => "\n" ++ PrintType.prettyString(PrintType.default.expr(PrintType.default, typ))) |? "")
    };
  };

  type kind =
  | Abstract
  | Open
  | Record(list(Attribute.t))
  | Variant(list(Constructor.t))
  ;
  type t = {
    kind,
    params: list((Types.type_expr, Location.t)),
    typ: Types.type_declaration,
  };
};

module Value = {
  type t = {
    typ: Types.type_expr,
    recursive: bool,
  };
};

/* type scope =
| File
| Switch
| Module
| Let
| LetRec; */

let isVisible = declared =>
  declared.exported && {
    let rec loop = v => switch v {
      | File(_) => true
      | NotVisible => false
      | ExportedModule(_, inner) => loop(inner)
      | HiddenModule(_, _) => false
      | Expression(_) => false
    };
    loop(declared.modulePath)
  };

type namedMap('t) = Hashtbl.t(string, 't);
type namedStampMap = namedMap(int);

module Module = {
  type exported = {
    types: namedStampMap,
    values: namedStampMap,
    modules: namedStampMap,
    moduleTypes: namedStampMap,
    /* constructors: namedStampMap, */
    /* classes: namedStampMap,
    classTypes: namedStampMap, */
  };
  let initExported = () => {
    types: Hashtbl.create(10),
    values: Hashtbl.create(10),
    modules: Hashtbl.create(10),
    moduleTypes: Hashtbl.create(10),
    /* constructors: Hashtbl.create(10), */
  };
  type item =
  | Value(Value.t)
  | Type(Type.t)
  | Module(kind)
  | ModuleType(kind)
  and contents = {
    exported,
    mutable topLevel: list(declared(item)),
  }
  and kind =
  | Ident(Path.t)
  /* , Location.loc(Longident.t)) */
  | Structure(contents);
  /* | JustType */
  /* and t = {
    typ: Types.module_type,
    kind,
  }; */
};

type stampMap('t) = Hashtbl.t(int, 't);

type stamps = {
  types: stampMap(declared(Type.t)),
  values: stampMap(declared(Value.t)),
  modules: stampMap(declared(Module.kind)),
  moduleTypes: stampMap(declared(Module.kind)),
  constructors: stampMap(declared(Type.Constructor.t)),
  /* moduleTypes: stampMap(declared(Module.kind)), */
};

let initStamps = () => {
  types: Hashtbl.create(10),
  values: Hashtbl.create(10),
  modules: Hashtbl.create(10),
  moduleTypes: Hashtbl.create(10),
  constructors: Hashtbl.create(10),
};

type file = {
  uri: string,
  docstring: option(string),
  stamps,
  moduleName: string,
  contents: Module.contents,
};

let emptyFile = (moduleName, uri) => {
  uri,
  docstring: None,
  stamps: initStamps(),
  moduleName,
  contents: {exported: Module.initExported(), topLevel: []}
};

type tip =
| Value
| Type
| Attribute(string)
| Constructor(string)
| Module
| ModuleType;

let tipToString = tip => switch tip {
| Value => "Value"
| Type => "Type"
| Attribute(a) => "Attribute(" ++ a ++ ")"
| Constructor(a) => "Constructor(" ++ a ++ ")"
| Module => "Module"
| ModuleType => "ModuleType"
};

type path =
| Tip(string)
| Nested(string, path);

let rec pathToString = path => switch path {
  | Tip(name) => name
  | Nested(name, inner) => name ++ "." ++ pathToString(inner)
};

let rec pathFromVisibility = (visibilityPath, current) => switch visibilityPath {
  | File(_) => Some(current)
  | ExportedModule(name, inner) => pathFromVisibility(inner, Nested(name, current))
  | NotVisible
  | HiddenModule(_, _)
  | Expression(_) => None
};

let pathFromVisibility = (visibilityPath, tipName) => pathFromVisibility(visibilityPath, Tip(tipName));

module Loc = {
  type typed =
  | LocalReference(int, tip)
  | GlobalReference(string, path, tip)
  | NotFound
  | Definition(int, tip);
  type t =
  | Typed(Types.type_expr, typed)
  | Constant(Asttypes.constant)
  | Module(typed)
  | TopLevelModule(string)
  | TypeDefinition(string, Types.type_declaration, int)
  | Explanation(string)
  | Open;
};

type openTracker = {
  path: Path.t,
  loc: Location.t,
  ident: Location.loc(Longident.t),
  extent: Location.t,
  mutable used: list((path, tip, Location.t)),
};

/** These are the bits of info that we need to make in-app stuff awesome */
type extra = {
  internalReferences: Hashtbl.t(int, list(Location.t)),
  externalReferences: Hashtbl.t(string, list((path, tip, Location.t))),
  mutable locations: list((Location.t, Loc.t)),
  /* This is the "open location", like the location...
     or maybe the >> location of the open ident maybe */
  /* OPTIMIZE: using a stack to come up with this would cut the computation time of this considerably. */
  opens: Hashtbl.t(Location.t, openTracker),
};

type full = {extra, file};

let initExtra = () => {
  internalReferences: Hashtbl.create(10),
  externalReferences: Hashtbl.create(10),
  locations: [],
  opens: Hashtbl.create(10),
};

let initFull = (moduleName, uri) => {file: emptyFile(moduleName, uri), extra: initExtra()};

let hashList = h => Hashtbl.fold((a, b, c) => [(a, b), ...c], h, []);

let showExtra = ({internalReferences, locations, externalReferences, opens}) => {
  let refs = hashList(internalReferences) |> List.map(((stamp, locs)) => {
    "Stamp: " ++ string_of_int(stamp) ++ "\n - "
    ++ String.concat("\n - ", List.map(Utils.showLocation, locs))
  }) |> String.concat("\n");
  let erefs = hashList(externalReferences) |> List.map(((moduleName, refs)) => {
    "External " ++ moduleName ++ ":\n - "
    ++ String.concat("\n - ",
    List.map(((path, tip, loc)) => 
      Utils.showLocation(loc)
      ++ " : " ++ pathToString(path) ++ " : " ++ tipToString(tip)
    , refs)
    )
  }) |> String.concat("\n");

  let opens = hashList(opens);
  Log.log("Opens " ++ string_of_int(List.length(opens)));
  let opens = opens |> List.map(((loc, tracker)) => {
    "Open at " ++ Utils.showLocation(loc) ++
    "\n  path: " ++ Path.name(tracker.path) ++
    "\n  ident: " ++ String.concat(".", Longident.flatten(tracker.ident.txt)) ++
    "\n  used:" ++ String.concat("", tracker.used |> List.map(((path, tip, loc)) => {
      "\n    " ++ pathToString(path) ++ " : " ++ tipToString(tip)
    }))

  }) |> String.concat("\n");

  "## Extra:\n\n" ++ refs ++ "\n" ++ erefs ++ "\n### Opens\n" ++ opens
};