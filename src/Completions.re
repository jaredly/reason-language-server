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
      List.map(((name, _, item)) => showItem(name, item), contents)
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

let getModuleResults =
    (processDoc, {cmt_modname, cmt_annots} as infos: Cmt_format.cmt_infos) =>
  Docs.forCmt(processDoc, infos)
  |?>> (
    ((doc, items)) => (
      Some(showItem(cmt_modname, Docs.Module(items))),
      Some(doc |? "(no documentation)")
    )
  )
  |? (None, None);

/* let getTopModule = (name, state) => {
     if (Hashtbl.mem(state.docs, name)) {
       `Doc(Hashtbl.find(state.docs, name))
     } else {
       switch (List.assoc(name, state.localModules)) {
       | exception Not_found => switch (List.assoc(Plain(name), state.dependencyModules)) {
       | exception Not_found => `NotFound
       | (cmt, src) => State.getCmt(cmt, state)
       }
       | (cmt, stc) => State.getCmt(cmt, state)
       }
     }
   }; */
let forModule = (state, k, cmt, src) => {
  let (detail, documentation) =
    if (Hashtbl.mem(state.cmtMap, cmt)) {
      getModuleResults(State.docConverter(src), Hashtbl.find(state.cmtMap, cmt));
    } else {
      (
        /* let cmt_infos = Cmt_format.read_cmt(cmt);
           Hashtbl.replace(state.cmtMap, cmt, cmt_infos);
           getModuleResults(cmt_infos) */
        None,
        None
      );
    };
  {kind: RootModule(cmt, src), label: k, detail, documentation, deprecated: false};
};

let forItem = (name, doc, item) => {
  {
    detail: Some(showItem(name, item)),
    documentation: Some(doc |? "(no documentation)"),
    kind: itemKind(item),
    deprecated: false, /* TODO */
    label: name
  }
};

let rec inDocs = (parts, contents) => {
  switch parts {
  | [] => []
  | [single] => contents |> Utils.filterMap(((name, doc, item)) => Utils.startsWith(name, single) ? Some(forItem(name, doc, item)) : None)
  | [first, ...rest] => contents |> Utils.find(((name, doc, item)) => switch item {
  | Docs.Module(contents) when name == first => Some(inDocs(rest, contents))
  | _ => None
  }) |? []
  }
};

/** Some docs */
let get = (parts, state) =>
  switch parts {
  | [] => []
  | [single] when isCapitalized(single) =>
    let results =
      List.fold_left(
        (results, (k, (cmt, src))) =>
          Utils.startsWith(k, single) ?
            [forModule(state, k, cmt, src), ...results] : results,
        [],
        state.localModules
      );
    let results =
      List.fold_left(
        (results, (k, (cmt, src))) =>
          switch k {
          | FindFiles.Plain(k) =>
            Utils.startsWith(k, single) ?
              [forModule(state, k, cmt, src), ...results] : results
          | _ => results
          },
        results,
        state.dependencyModules
      );
    results;
  | [first, ...more] =>
    switch (State.getDocs(first, state)) {
    | Error(_) => [] /* TODO handle opens */
    | Ok((_, contents)) => inDocs(more, contents)
    }
  };