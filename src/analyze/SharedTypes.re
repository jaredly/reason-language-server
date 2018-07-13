
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

type scope =
| File
| Switch
| Module
| Let
| LetRec;

/**

I want the top level to be

A (y) > B (n) > C (y) > y/n
building up, so

Exported(ExportedModule("C", NotExportedModule("B", ExportedModule("A", File("uri")))))

 */



/* TODO maybe track the loc's of these things */
type visibilityPath =
| File(string)
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
  docstring: option(string),
  stamps,
  contents: Module.contents,
};

type tip =
| Value
| Type
| Attribute(string)
| Constructor(string)
| Module
| ModuleType;

type path('tip) =
| Tip(string, 'tip)
| Nested(string, path('tip));

type openTracker = {
  path: Path.t,
  loc: Location.loc(Longident.t),
  extent: Location.t,
  used: Hashtbl.t((string, path(tip)), Location.t),
  mutable useCount: int,
};

module Loc = {
  type typed =
  | Value(Path.t)
  | Type(Path.t)
  | Module(Path.t)
  | Constructor(Path.t, string)
  | Attribute(Path.t, string)
  | ConstructorDefinition(int, string)
  | AttributeDefinition(int, string)
  | ValueDefinition(int)
  | TypeDefinition(int);
  type t =
  | Typed(Types.type_expr, typed)
  | Explanation(string)
  | Open;
};

/** These are the bits of info that we need to make in-app stuff awesome */
type extra = {
  internalReferences: Hashtbl.t(int, list(Location.t)),
  externalReferences: Hashtbl.t(string, list((string, path(tip), Location.t))),
  mutable locations: list((Location.t, Loc.t)),
  opens: Hashtbl.t(Location.t, openTracker),
};

let initExtra = () => {
  internalReferences: Hashtbl.create(10),
  externalReferences: Hashtbl.create(10),
  locations: [],
  opens: Hashtbl.create(10),
};