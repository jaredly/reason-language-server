
open TopTypes;

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
  path: option(filePath),
  label: string,
  detail: option(string),
  documentation: option(string),
  deprecated: bool
};

let itemKind = doc => switch doc {
| Docs.T.Module(_) => Module
| ModuleAlias(_) => Module
| Function(_) => Function
| Constructor(_) => Value
| Attribute(_) => Value
| Value(_) => Value
| Type(_, _) => Struct
};

let rec showItem = (name, item) =>
  switch item {
  | Docs.T.Module(contents) =>
    "module "
    ++ name
    ++ " {\n"
    ++ (
      List.map(({Docs.T.name, kind}) => showItem(name, kind), contents)
      |> String.concat("\n")
    )
    ++ "\n}"
  | ModuleAlias(_) => "module alias " ++ name
  | Function(_) => "function"
  | Value(t) =>
    PrintType.default.value(PrintType.default, name, name, t)
    |> PrintType.prettyString
  | Type(t, extra) =>
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
    ({Docs.T.docstring, topLevel}) => (
      Some(showItem(name, Docs.T.Module(topLevel))),
      Some(docstring |? "(no documentation)")
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
  {kind: RootModule(cmt, src), path: src, label: k, detail, documentation, deprecated: false};
};

let forItem = (path, name, loc, doc, item) => {
  {
    detail: Some(showItem(name, item)),
    path: path,
    documentation: Some(doc |? "(no documentation)"),
    kind: itemKind(item),
    deprecated: false, /* TODO */
    label: name
  }
};

let completionItems = (uri, moduleContents, prefix) => {
  moduleContents |> List.fold_left((results, {Docs.T.name, loc, docstring, kind}) => switch kind {
    | Docs.T.Type({type_kind: Type_variant(constructors)} as typ, _extra) => {
      let results = (constructors |> Utils.filterMap(({Types.cd_id: {name}} as decl) => (
        Utils.startsWith(name, prefix) ? Some(forItem(uri, name, loc, docstring, Docs.T.Constructor(
          decl, name, typ
        ))) : None
      ))) @ results;
      Utils.startsWith(name, prefix) ? [forItem(uri, name, loc, docstring, kind), ...results] : results
    }
    | _ => Utils.startsWith(name, prefix) ? [forItem(uri, name, loc, docstring, kind), ...results] : results
  }, [])
};

let inDocs = (~resolveAlias, uri, parts, contents) => {
  switch (Docs.resolveDocsPath(~resolveAlias, uri, parts, contents)) {
    | None => []
    | Some((uri, contents, single)) => completionItems(uri, contents, single)
  }
};

let rec findSubModule = (state, needle, contents, ~package) => switch contents {
  | [] => None
  | [{Docs.T.name, loc, docstring, kind: Docs.T.Module(innerContents)}, ..._] when needle == name => Some((name, loc, docstring, innerContents))
  | [{name, loc, docstring, kind: Docs.T.ModuleAlias(path)}, ..._] when needle == name => {
    switch (State.resolveAlias(state, path, [], ~package)) {
      | None => None
      | Some((uri, contents, items)) => {
        let rec loop = (items, contents) => switch items {
          | [] => Some((name, loc, docstring, contents))
          | [single] => findSubModule(state, single, contents, ~package)
          | [single, ...more] => {
            switch (findSubModule(state, single, contents, ~package)) {
              | None => None
              | Some((name, loc, docstring, inner)) => loop(more, inner)
            }
          }
        };
        loop(items, contents)
      }
    }
    /* Some((n, loc, doc, innerContents)) */
  }
  | [_, ...rest] => findSubModule(state, needle, rest, ~package)
};

/* TODO local opens */
let resolveOpens = (opens, state, ~package) => {
  List.fold_left((previous, name) => {
    let rec loop = prev => switch prev {
    | [] =>
      switch (State.docsForModule(name, state, ~package)) {
      | None => previous /* TODO warn? */
      | Some(({Docs.T.topLevel}, uri)) => previous @ [(name, topLevel, uri)]
      }
    | [(prevname, contents, uri), ...rest] =>
      switch (findSubModule(state, name, contents, ~package)) {
      | None => loop(rest)
      | Some((name, _loc, _, innerContents)) => previous @ [(name, innerContents, uri)]
      }
    };
    loop(previous)
  }, [], opens);
};

/** Some docs */
let get = (~currentPath, topModule, opens, parts, state, localData, pos, ~package) => {
  let opens = resolveOpens(opens, state, ~package);
  let packageOpens = ["Pervasives", ...package.opens];
  let opens = List.fold_left(
    (opens, name) => switch (State.docsForModule(name, state, ~package)) {
      | None => {
        Log.log("Auto open " ++ name ++ " not found...");
        opens
      }
      | Some(({Docs.T.topLevel}, uri)) => {
        [(name, topLevel, uri), ...opens]
      }
      },
      opens,
      packageOpens
  );
  switch parts {
  | [] => []
  | [""] => []
  | [single] =>
    let localResults = switch (localData) {
    | None => []
    | Some(moduleData) => {
      Definition.completions(moduleData, single, pos) |> List.map(((name, loc, item, docs)) => forItem(Some(currentPath), name, loc, docs, item))
    }
    };

    let results = opens |> List.map(((_, contents, path)) => contents |> Utils.filterMap(
      ({Docs.T.name, loc, docstring, kind}) => Utils.startsWith(name, single) ? Some(forItem(path, name, loc, docstring, kind)) : None
    )) |> List.concat;

    let results = localResults @ results;

    if (isCapitalized(single)) {
      let results =
        List.fold_left(
          (results, (k, (cmt, src))) =>
            Utils.startsWith(k, single) && k != topModule ?
              [forModule(state, k, cmt, Some(src)), ...results] : results,
          results,
          package.localModules
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
          package.dependencyModules
        );
      results;
    } else {
      results
    }
  | [first, ...more] =>
  /* state.settings. */
  /* state.settings. */
/* state.settings. */
    /* TODO handle name overrides from local opens vs local modules */
    let localResults =
      localData
      |?> (
        moduleData =>
          Definition.completionPath(
            inDocs(~resolveAlias=State.resolveAlias(~package, state), Some(currentPath)),
            moduleData, first, more, pos,
            ((name, loc, item, docs, _range)) =>
              forItem(
                Some(currentPath),
                name,
                loc,
                docs,
                item,
              ),
              ~uri=currentPath,
            ~resolveDefinition=uri => State.resolveDefinition(uri, state, ~package, moduleData)
            )
      );
    switch (localResults) {
    | Some(r) => r
    | None =>
      switch (
        Utils.find(
          ((name, contents, uri)) =>
            findSubModule(state, first, contents, ~package) |?>> (x => (x, uri)),
          opens,
        )
      ) {
      | Some(((_, _, _, contents), uri)) =>
        inDocs(~resolveAlias=State.resolveAlias(~package, state), uri, more, contents)
      | None =>
        switch (State.docsForModule(first, state, ~package)) {
        | None =>
          Log.log("No docs found for " ++ first);
          []; /* TODO handle opens */
        | Some(({Docs.T.topLevel}, uri)) =>
          inDocs(~resolveAlias=State.resolveAlias(~package, state), uri, more, topLevel)
        }
      }
    };
  };
};