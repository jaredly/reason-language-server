
/* type typeExtra =
| Constructors(list((int, Location.loc(string), Types.constructor_declaration)))
| Attributes(list((int, Location.loc(string), (Location.t, Types.type_expr))));

type item = {
  name: string,
  kind,
  stamp: int,
  loc: Location.t,
  docstring: option(string),
}

and kind =
  | Module(list(item))
  | ModuleAlias(Path.t)
  | Function(list(Types.type_expr), Types.type_expr)
  | Value(Types.type_expr)
  | Type(Types.type_declaration, option(typeExtra))
  | Constructor(Types.constructor_declaration, string, Types.type_declaration)
  | Attribute(Types.type_expr, string, Types.type_declaration); */

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
  stamp: int,
  deprecated: option(string),
  modulePath: visibilityPath,
  exported: bool,
  docstring: option(string),
  contents: 't,
  /* scopeType: scope, */
  /* scopeStart: (int, int), */
};

module Type = {
  module Attribute = {
    type t = {
      stamp: int,
      name: Location.loc(string),
      typ: Types.type_expr,
      typLoc: Location.t,
    }
  };
  module Constructor = {
    type t = {
      stamp: int,
      name: Location.loc(string),
      args: list((Types.type_expr, Location.t)),
      res: option(Types.type_expr),
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

/**

I want the top level to be

A (y) > B (n) > C (y) > y/n
building up, so

Exported(ExportedModule("C", NotExportedModule("B", ExportedModule("A", File("uri")))))

 */



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
    /* classes: namedStampMap,
    classTypes: namedStampMap, */
  };
  let initExported = () => {
    types: Hashtbl.create(10),
    values: Hashtbl.create(10),
    modules: Hashtbl.create(10),
    moduleTypes: Hashtbl.create(10),
  };
  type item =
  | Value(Value.t)
  | Type(Type.t)
  | Module(t)
  | ModuleType(t)
  and contents = {
    exported,
    mutable topLevel: list(declared(item)),
  }
  and kind =
  | Ident(Path.t, Location.loc(Longident.t))
  | Structure(contents)
  /* | JustType */
  and t = {
    typ: Types.module_type,
    kind,
  };
};

type stampMap('t) = Hashtbl.t(int, 't);

type stamps = {
  types: stampMap(declared(Type.t)),
  values: stampMap(declared(Value.t)),
  modules: stampMap(declared(Module.t)),
  moduleTypes: stampMap(declared(Module.t)),
};

let initStamps = () => {
  types: Hashtbl.create(10),
  values: Hashtbl.create(10),
  modules: Hashtbl.create(10),
  moduleTypes: Hashtbl.create(10),
};

type file = {
  uri: string,
  docstring: option(string),
  stamps,
  moduleName: string,
  contents: Module.contents,
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
/* | Tip(string, 'tip) */
| Nested(string, path);

let rec pathToString = path => switch path {
  | Tip(name) => name
  | Nested(name, inner) => name ++ " > " ++ pathToString(inner)
};

let rec pathFromVisibility = (visibilityPath, current) => switch visibilityPath {
  | File(_) => Some(current)
  | ExportedModule(name, inner) => pathFromVisibility(inner, Nested(name, current))
  | NotVisible
  | HiddenModule(_, _)
  | Expression(_) => None
};

let pathFromVisibility = (visibilityPath, tipName) => pathFromVisibility(visibilityPath, Tip(tipName));

/* type fullPath = ([`Local(int) | `Global(string, path)], tip); */

type openTracker = {
  path: Path.t,
  loc: Location.t,
  ident: Location.loc(Longident.t),
  extent: Location.t,
  used: Hashtbl.t((path, tip), Location.t),
  mutable useCount: int,
};

module Loc = {
  type typed =
  | LocalReference(int, tip)
  | GlobalReference(string, path, tip)
  | NotFound
  | Definition(int, tip);
  type t =
  | Typed(Types.type_expr, typed)
  | Explanation(string)
  | Open;
};

/** These are the bits of info that we need to make in-app stuff awesome */
type extra = {
  /* todo maybe I want a stampToDeclared map or something */
  internalReferences: Hashtbl.t(int, list(Location.t)),
  externalReferences: Hashtbl.t(string, list((path, tip, Location.t))),
  mutable locations: list((Location.t, Loc.t)),
  opens: Hashtbl.t(Location.t, openTracker),
};

let initExtra = () => {
  internalReferences: Hashtbl.create(10),
  externalReferences: Hashtbl.create(10),
  locations: [],
  opens: Hashtbl.create(10),
};

let hashList = h => Hashtbl.fold((a, b, c) => [(a, b), ...c], h, []);

let showExtra = ({internalReferences, locations, externalReferences}) => {
  let refs = hashList(internalReferences) |> List.map(((stamp, locs)) => {
    /* let {name} = Hashtbl.find() */
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
  "## Extra:\n\n" ++ refs ++ "\n" ++ erefs
};