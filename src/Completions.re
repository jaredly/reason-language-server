
open State;

type kind =
  | Module
  | RootModule(string);

type item = {
  kind,
  label: string,
  detail: option(string),
  documentation: option(string),
  deprecated: bool,
};

let kindToInt = k => switch k {
| RootModule(_) => 9
| Module => 9
};

let rec showItem = (name, item) => switch item {
| Docs.Module(contents) => "module " ++ name ++ " {\n" ++ (List.map(((name, _, item)) => showItem(name, item), contents) |> String.concat("\n")) ++ "\n}"
| Function(_) => "function"
| Value(t) => PrintType.default.value(PrintType.default, name, name, t) |> PrintType.prettyString
| Type(t) => PrintType.default.decl(PrintType.default, name, name, t) |> PrintType.prettyString
};

let isCapitalized = name => name.[0] >= 'A' && name.[0] <= 'Z';
open Infix;

let getModuleResults = ({cmt_modname, cmt_annots} as infos: Cmt_format.cmt_infos) => {
  (Docs.forCmt(infos) |?>> ((doc, items)) => {
    (
      Some(showItem(cmt_modname, Docs.Module(items))),
      Some(doc |? "(no documentation)"),
    )
  }) |? (None, None)
};

/** Some docs */
let get = (scope, name, state) => {
  if (scope == [] && isCapitalized(name)) {
    let forModule = (k, cmt) => {
      let (detail, documentation) = if (Hashtbl.mem(state.cmtMap, cmt)) {
        getModuleResults(Hashtbl.find(state.cmtMap, cmt))
      } else {
        /* let cmt_infos = Cmt_format.read_cmt(cmt);
        Hashtbl.replace(state.cmtMap, cmt, cmt_infos);
        getModuleResults(cmt_infos) */
        (None, None)
      };
      {
        kind: RootModule(cmt),
        label: k,
        detail,
        documentation,
        deprecated: false,
      }
    };
    let results = List.fold_left((results, (k, (cmt, _))) => Utils.startsWith(k, name)
    ? [forModule(k, cmt), ...results]
    : results,
    [],
    state.localModules);
    let results = List.fold_left((results, (k, (cmt, _))) => switch k {
    | FindFiles.Plain(k) => Utils.startsWith(k, name) ? [forModule(k, cmt), ...results] : results
    | _ => results
    }, results, state.dependencyModules);
    results
  } else {
    []
  }
};
