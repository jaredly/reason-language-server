
type kinds = [ `Function | `Array | `Variable | `Object | `Null | `EnumMember | `Module | `Enum | `Interface | `TypeParameter | `ModuleType ];

type flexibleType = {
  toString: unit => string,
  variableKind: kinds,
  getConstructorPath: unit => option((Path.t, list(flexibleType))),
  getArguments: unit => (list((string, flexibleType)), flexibleType),
};

type flexibleDeclaration = {
  declToString: string => string,
  declarationKind: kinds,
};

type filePath = string
type paths =
| Impl(filePath, option(filePath))
| Intf(filePath, option(filePath))
| IntfAndImpl(filePath, option(filePath), filePath, option(filePath));

let getSrc = p => switch p {
  | Intf(_, s)
  | Impl(_, s)
  | IntfAndImpl(_, Some(_) as s, _, _)
  | IntfAndImpl(_, None, _, s) => s
};

let getCmt = p => switch p {
  | Impl(c, _) | Intf(c, _) | IntfAndImpl(c, _, _, _) => c
};

/* TODO maybe track the loc's of these things */
type visibilityPath =
| File(string)
| NotVisible
| ExportedModule(string, visibilityPath)
| HiddenModule(string, visibilityPath)
| Expression(visibilityPath);

let rec showVisibilityPath = path => switch path {
  | File(uri) => Some((uri, []))
  | NotVisible => None
  | ExportedModule(name, inner) => switch (showVisibilityPath(inner)) {
    | None => None
    | Some((uri, path)) => Some((uri, path @ [name]))
  }
  | HiddenModule(_) => None
  | Expression(inner) => None
};

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
      typ: flexibleType,
      typLoc: Location.t,
    };
  };

  module Constructor = {
    type t = {
      stamp: int,
      name: Location.loc(string),
      args: list((flexibleType, Location.t)),
      res: option(flexibleType),
    };
    open Infix;
  };

  type kind =
  | Abstract(option((Path.t, list(flexibleType))))
  | Open
  | Record(list(Attribute.t))
  | Variant(list(Constructor.t))
  ;
  type t = {
    kind,
    params: list((flexibleType, Location.t)),
    typ: flexibleDeclaration,
  };
};

module Value = {
  type t = {
    typ: flexibleType,
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
  | Structure(contents);
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
  | Typed(flexibleType, typed)
  | Constant(Asttypes.constant)
  | Module(typed)
  | TopLevelModule(string)
  | TypeDefinition(string, flexibleDeclaration, int)
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