open State;

type kind =
  | Module
  | Function
  | Value
  | Struct
  | RootModule(string, option(string));

let kindToInt = k =>
  switch k {
  | RootModule(_) => 9
  | Module => 9
  | Function => 3
  | Struct => 22
  | Value => 12
  };

type item = {
  kind,
  uri: option(string),
  label: string,
  detail: option(string),
  documentation: option(string),
  deprecated: bool
};

let itemKind = doc => switch doc {
| Docs.Module(_) => Module
| ModuleAlias(_) => Module
| Function(_) => Function
| Constructor(_) => Value
| Attribute(_) => Value
| Value(_) => Value
| Type(_) => Struct
};

let rec showItem = (name, item) =>
  switch item {
  | Docs.Module(contents) =>
    "module "
    ++ name
    ++ " {\n"
    ++ (
      List.map(((name, _loc, _, item)) => showItem(name, item), contents)
      |> String.concat("\n")
    )
    ++ "\n}"
  | ModuleAlias(_) => "module alias " ++ name
  | Function(_) => "function"
  | Value(t) =>
    PrintType.default.value(PrintType.default, name, name, t)
    |> PrintType.prettyString
  | Type(t) =>
    PrintType.default.decl(PrintType.default, name, name, t)
    |> PrintType.prettyString
  | Constructor(decl, parentName, t) => {
    let top = PrintType.default.constructor(PrintType.default, decl) |> PrintType.prettyString;
    let bottom = PrintType.default.decl(PrintType.default, parentName, parentName, t) |> PrintType.prettyString;
    top ++ "\n\n" ++ bottom
  }
  | Attribute(ret, parentName, t) => {
    let top = name ++ ": " ++ (PrintType.default.expr(PrintType.default, ret) |> PrintType.prettyString);
    let bottom = PrintType.default.decl(PrintType.default, parentName, parentName, t) |> PrintType.prettyString;
    top ++ "\n\n" ++ bottom
  }
  };

let isCapitalized = name => name != "" && (name.[0] >= 'A' && name.[0] <= 'Z');

open Infix;

let getModuleResults = (name, state, cmt, src) => {
  State.docsForCmt(cmt, src, state)
  |?>> (
    ((doc, items)) => (
      Some(showItem(name, Docs.Module(items))),
      Some(doc |? "(no documentation)")
    )
  )
  |? (None, None);
};

let forModule = (state, k, cmt, src) => {
  let (detail, documentation) =
    if (State.hasProcessedCmt(state, cmt)) {
      getModuleResults(k, state, cmt, src);
    } else {
      (
        None,
        None
      );
    };
  {kind: RootModule(cmt, src), uri: src, label: k, detail, documentation, deprecated: false};
};

let forItem = (uri, name, loc, doc, item) => {
  {
    detail: Some(showItem(name, item)),
    uri,
    documentation: Some(doc |? "(no documentation)"),
    kind: itemKind(item),
    deprecated: false, /* TODO */
    label: name
  }
};

let completionItems = (uri, moduleContents, prefix) => {
  moduleContents |> List.fold_left((results, (name, loc, doc, item)) => switch item {
    | Docs.Type({type_kind: Type_variant(constructors)} as typ) => {
      let results = (constructors |> Utils.filterMap(({Types.cd_id: {name}} as decl) => (
        Utils.startsWith(name, prefix) ? Some(forItem(uri, name, loc, doc, Docs.Constructor(
          decl, name, typ
        ))) : None
      ))) @ results;
      Utils.startsWith(name, prefix) ? [forItem(uri, name, loc, doc, item), ...results] : results
    }
    | _ => Utils.startsWith(name, prefix) ? [forItem(uri, name, loc, doc, item), ...results] : results
  }, [])
};

let rec inDocs = (~resolveAlias, uri, parts, contents) =>
  switch (parts) {
  | [] => []
  | [single] =>
    completionItems(uri, contents, single)
  | [first, ...rest] =>
    contents
    |> Utils.find(((name, loc, doc, item)) =>
         switch (item) {
         | Docs.Module(contents) when name == first =>
           Some(inDocs(~resolveAlias, uri, rest, contents))
         | Docs.ModuleAlias(path) when name == first =>
            switch (resolveAlias(path, rest)) {
              | None => {
                Log.log("Unable to resolve module alias!!! " ++ name);
                Some([])
              }
              | Some((uri, contents, items)) =>
             Some(inDocs(~resolveAlias, uri, items, contents))
            }
         | _ => None
         }
       )
    |? []
  };

let rec resolveAlias = (state, path, children) => {
  let rec loop = (path, items) =>
    switch (path) {
    | Path.Pident({stamp: 0, name}) => State.docsForModule(name, state) |?>> (((_, contents), uri)) => (uri, contents, items)
    | Path.Pident(_) => None
    | Pdot(inner, name, _) => loop(inner, [name, ...items])
    | Papply(_) => None
    };
  loop(path, children);
};

let rec findSubModule = (state, name, contents) => switch contents {
  | [] => None
  | [(n, loc, doc, Docs.Module(innerContents)), ..._] when n == name => Some((n, loc, doc, innerContents))
  | [(n, loc, doc, Docs.ModuleAlias(path)), ..._] when n == name => {
    switch (resolveAlias(state, path, [])) {
      | None => None
      | Some((uri, contents, items)) => {
        let rec loop = (items, contents) => switch items {
          | [] => Some((n, loc, doc, contents))
          | [single] => findSubModule(state, single, contents)
          | [single, ...more] => {
            switch (findSubModule(state, single, contents)) {
              | None => None
              | Some((n, loc, doc, inner)) => loop(more, inner)
            }
          }
        };
        loop(items, contents)
      }
    }
    /* Some((n, loc, doc, innerContents)) */
  }
  | [_, ...rest] => findSubModule(state, name, rest)
};

/* TODO local opens */
let resolveOpens = (opens, state) => {
  List.fold_left((previous, name) => {
    let rec loop = prev => switch prev {
    | [] =>
      switch (State.docsForModule(name, state)) {
      | None => previous /* TODO warn? */
      | Some(((docs, contents), uri)) => previous @ [(name, contents, uri)]
      }
    | [(prevname, contents, uri), ...rest] =>
      switch (findSubModule(state, name, contents)) {
      | None => loop(rest)
      | Some((name, _loc, _, innerContents)) => previous @ [(name, innerContents, uri)]
      }
    };
    loop(previous)
  }, [], opens);
};

/** Some docs */
let get = (topModule, opens, parts, state, localData, pos) => {
  let opens = resolveOpens(opens, state);
  let opens = switch (State.docsForModule("Pervasives", state)) {
  | None => {
    Log.log("No pervasives found...");
    opens
  }
  | Some(((_, contents), uri)) => [("Pervasives", contents, uri), ...opens]
  };
  switch parts {
  | [] => []
  | [""] => []
  | [single] =>
    let localResults = switch (localData) {
    | None => []
    | Some(moduleData) => {
      Definition.completions(moduleData, single, pos) |> List.map(((name, loc, item, docs)) => forItem(Some("(current file)"), name, loc, docs, Definition.docsItem(item, moduleData)))
    }
    };


    let results = opens |> List.map(((_, contents, uri)) => contents |> Utils.filterMap(
      ((name, loc, doc, item)) => Utils.startsWith(name, single) ? Some(forItem(uri, name, loc, doc, item)) : None
    )) |> List.concat;

    let results = localResults @ results;

    if (isCapitalized(single)) {
      let results =
        List.fold_left(
          (results, (k, (cmt, src))) =>
            Utils.startsWith(k, single) && k != topModule ?
              [forModule(state, k, cmt, Some(src)), ...results] : results,
          results,
          state.localModules
        );
      let results =
        List.fold_left(
          (results, (k, (cmt, src))) =>
            switch k {
            | FindFiles.Plain(k) =>
              Utils.startsWith(k, single) && k != topModule ?
                [forModule(state, k, cmt, src), ...results] : results
            | _ => results
            },
          results,
          state.dependencyModules
        );
      results;

    } else {
      results
    }
  | [first, ...more] =>
    let resolveAlias = resolveAlias(state);
    switch (Utils.find(((name, contents, uri)) => findSubModule(state, first, contents) |?>> x => (x, uri), opens)) {
    | Some(((_, _, _, contents), uri)) => inDocs(~resolveAlias, uri, more, contents)
    | None =>
    switch (State.docsForModule(first, state)) {
    | None => [] /* TODO handle opens */
    | Some(((_, contents), uri)) => inDocs(~resolveAlias, uri, more, contents)
    }
    }
  };
};