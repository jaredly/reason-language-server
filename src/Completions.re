open State;

type kind =
  | Module
  | Function
  | Value
  | Struct
  | RootModule(string, string);

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
  uri: string,
  label: string,
  detail: option(string),
  documentation: option(string),
  deprecated: bool
};

let itemKind = doc => switch doc {
| Docs.Module(_) => Module
| Function(_) => Function
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
  | Function(_) => "function"
  | Value(t) =>
    PrintType.default.value(PrintType.default, name, name, t)
    |> PrintType.prettyString
  | Type(t) =>
    PrintType.default.decl(PrintType.default, name, name, t)
    |> PrintType.prettyString
  };

let isCapitalized = name => name.[0] >= 'A' && name.[0] <= 'Z';

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

let rec inDocs = (uri, parts, contents) => {
  switch parts {
  | [] => []
  | [single] => contents |> Utils.filterMap(((name, loc, doc, item)) => Utils.startsWith(name, single) ? Some(forItem(uri, name, loc, doc, item)) : None)
  | [first, ...rest] => contents |> Utils.find(((name, loc, doc, item)) => switch item {
  | Docs.Module(contents) when name == first => Some(inDocs(uri, rest, contents))
  | _ => None
  }) |? []
  }
};

let rec findSubModule = (name, contents) => switch contents {
  | [] => None
  | [(n, loc, doc, Docs.Module(innerContents)), ..._] when n == name => Some((n, loc, doc, innerContents))
  | [_, ...rest] => findSubModule(name, rest)
};

let resolveOpens = (opens, state) => {
  List.fold_left((previous, name) => {
    let rec loop = prev => switch prev {
    | [] =>
      switch (State.docsForModule(name, state)) {
      | None => previous /* TODO warn? */
      | Some(((docs, contents), uri)) => previous @ [(name, contents, uri)]
      }
    | [(prevname, contents, uri), ...rest] =>
      switch (findSubModule(name, contents)) {
      | None => loop(rest)
      | Some((name, _loc, _, innerContents)) => previous @ [(name, innerContents, uri)]
      }
      /* let rec find = contents => switch contents {
      | [] => loop(rest)
      | [(n, _, Docs.Module(innerContents)), ..._] when n == name =>
      | [_, ...rest] => find(rest)
      }; */
      /* find(contents) */
    };
    loop(previous)
  }, [], opens);
};

/** Some docs */
let get = (topModule, opens, parts, state) => {
  let opens = resolveOpens(opens, state);
  let opens = switch (State.docsForModule("Pervasives", state)) {
  | None => {
    output_string(stderr, "No pervasives found...");
    opens
  }
  | Some(((_, contents), uri)) => [("Pervasives", contents, uri), ...opens]
  };
  switch parts {
  | [] => []
  | [single] =>
    let results = opens |> List.map(((_, contents, uri)) => contents |> Utils.filterMap(
      ((name, loc, doc, item)) => Utils.startsWith(name, single) ? Some(forItem(uri, name, loc, doc, item)) : None
    )) |> List.concat;
    if (isCapitalized(single)) {
    let results =
      List.fold_left(
        (results, (k, (cmt, src))) =>
          Utils.startsWith(k, single) && k != topModule ?
            [forModule(state, k, cmt, src), ...results] : results,
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
    switch (Utils.find(((name, contents, uri)) => findSubModule(first, contents) |?>> x => (x, uri), opens)) {
    | Some(((_, _, _, contents), uri)) => inDocs(uri, more, contents)
    | None =>
    switch (State.docsForModule(first, state)) {
    | None => [] /* TODO handle opens */
    | Some(((_, contents), uri)) => inDocs(uri, more, contents)
    }
    }
  };
};