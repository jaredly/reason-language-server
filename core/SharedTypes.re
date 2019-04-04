
type kinds = [ `Function | `Array | `Variable | `Object | `Null | `EnumMember | `Module | `Enum | `Interface | `TypeParameter | `ModuleType ];

module SimpleType = {

  type expr('source) =
    | Variable(string)
    | AnonVariable
    | RowVariant(list((string, option(expr('source)))), bool)
    | Reference('source, list(expr('source)))
    | Tuple(list(expr('source)))
    | Fn(list((option(string), expr('source))), expr('source))
    | Other

  type body('source) =
    | Open
    | Abstract
    | Expr(expr('source))
    | Record(list((string, expr('source))))
    | Variant(list((string, list(expr('source)), option(expr('source)))))

  type declaration('source) = {
    name: string,
    variables: list(expr('source)),
    body: body('source)
  };

  let rec usedSourcesExpr = exp => switch exp {
    | Tuple(items) => items->Belt.List.map(usedSourcesExpr)->List.concat
    | Reference(source, args) => [source, ...args->Belt.List.map(usedSourcesExpr)->List.concat]
    | Fn(args, res) => args->Belt.List.map(snd)->Belt.List.map(usedSourcesExpr)->List.concat @ usedSourcesExpr(res)
    | RowVariant(rows, _) => rows->Belt.List.keepMap(snd)->Belt.List.map(usedSourcesExpr)->List.concat
    | Variable(_) | AnonVariable | Other => []
  };

  let usedSources = decl => switch (decl.body) {
    | Expr(exp) => usedSourcesExpr(exp)
    | Record(items) => items->Belt.List.map(snd)->Belt.List.map(usedSourcesExpr)->List.concat
    | Variant(items) => items->Belt.List.map(((_, items, res)) =>
      items->Belt.List.map(usedSourcesExpr)->List.concat @ switch res {
        | None => []
        | Some(res) => usedSourcesExpr(res)
      }
    )->List.concat
    | Open | Abstract => []
  };

  let rec cmp: 'a 'b . (('a, 'b) => bool, expr('a), expr('b)) => bool = (compareSources, one, two) => switch (one, two) {
    | (Variable(one), Variable(two)) => one == two
    | (AnonVariable, AnonVariable) => true
    | (Reference(sone, argsOne), Reference(stwo, argsTwo)) =>
      compareSources(sone, stwo) && List.length(argsOne) == List.length(argsTwo)
      && Belt.List.every2(argsOne, argsTwo, cmp(compareSources))
    | (Tuple(one), Tuple(two)) => List.length(one) == List.length(two) && Belt.List.every2(one, two, cmp(compareSources))
    | (Fn(args1, body1), Fn(args2, body2)) =>
      cmp(compareSources, body1, body2) &&
      List.length(args1) == List.length(args2) &&
      Belt.List.every2(args1, args2, ((l1, t1), (l2, t2)) => l1 == l2 && cmp(compareSources, t1, t2))
    | _ => false
  };

  let rec mapSource = (fn, expr) => switch expr {
    | Variable(s) => Variable(s)
    | AnonVariable => AnonVariable
    | Reference(s, args) => Reference(fn(s), args->Belt.List.map(mapSource(fn)))
    | Tuple(items) => Tuple(items->Belt.List.map(mapSource(fn)))
    | Fn(args, res) => Fn(args->Belt.List.map(
      ((label, arg)) => (label, mapSource(fn, arg))
    ), mapSource(fn, res))
    | RowVariant(rows, closed) => RowVariant(
      rows->Belt.List.map(((label, expr)) => (label, switch expr {
        | None => None
        | Some(expr) => Some(mapSource(fn, expr))
      })),
      closed
    )
    | Other => Other
  };

  let declMapSource = (fn, decl) => {
    name: decl.name,
    variables: decl.variables->Belt.List.map(mapSource(fn)),
    body: switch (decl.body) {
      | Open => Open
      | Abstract => Abstract
      | Expr(expr) => Expr(mapSource(fn, expr))
      | Record(items) => Record(items->Belt.List.map(((label, arg)) => (label, mapSource(fn, arg))))
      | Variant(constructors) => Variant(constructors->Belt.List.map(((name, args, result)) =>
        (name, args->Belt.List.map(mapSource(fn)), switch result {
          | None => None
          | Some(result) => Some(mapSource(fn, result))
        })))
    }
  };
};

type flexibleType = {
  toString: unit => string,
  variableKind: kinds,
  getConstructorPath: unit => option((Path.t, list(flexibleType))),
  getArguments: unit => (list((string, flexibleType)), flexibleType),
  asSimpleType: unit => SimpleType.expr(Path.t),
};

type flexibleDeclaration = {
  declToString: string => string,
  declarationKind: kinds,
  asSimpleDeclaration: string => SimpleType.declaration(Path.t),
  migrateAttributes: unit => Parsetree.attributes,
};

type filePath = string
type paths =
| Impl(filePath, option(filePath))
| Intf(filePath, option(filePath))
// .cm(t)i, .mli, .cmt, .rei
| IntfAndImpl(filePath, option(filePath), filePath, option(filePath));

let getImpl = p => switch p {
  | Impl(cmt, Some(s))
  | IntfAndImpl(_, _, cmt, Some(s)) => Some((cmt, s))
  | _ => None
};

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
| File(string, string)
| NotVisible
| IncludedModule(Path.t, visibilityPath)
| ExportedModule(string, visibilityPath)
| HiddenModule(string, visibilityPath)
| Expression(visibilityPath);

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
  };

  type kind =
  | Abstract(option((Path.t, list(flexibleType))))
  | Open
  | Tuple(list(flexibleType))
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
      | IncludedModule(_, inner) => loop(inner)
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
  | IncludedModule(_, inner) => pathFromVisibility(inner, current)
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

let showExtra = ({internalReferences, externalReferences, opens}) => {
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
    "\n  used:" ++ String.concat("", tracker.used |> List.map(((path, tip, _loc)) => {
      "\n    " ++ pathToString(path) ++ " : " ++ tipToString(tip)
    }))

  }) |> String.concat("\n");

  "## Extra:\n\n" ++ refs ++ "\n" ++ erefs ++ "\n### Opens\n" ++ opens
};