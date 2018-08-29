open Result;

let showModuleTopLevel = (~markdown, topLevel: list(SharedTypes.declared(SharedTypes.Module.item))) => {
  let contents = topLevel |. Belt.List.map(item => {
    item.name.txt
  }) |> String.concat("\n");
  Some(markdown ? "```\n" ++ contents ++ "\n```" : contents)
};

let showModule = (~markdown, ~file: SharedTypes.file, declared: option(SharedTypes.declared(SharedTypes.Module.kind))) => {
  switch declared {
    | None => showModuleTopLevel(~markdown, file.contents.topLevel)
    | Some({contents: Structure({topLevel})}) => {
      showModuleTopLevel(~markdown, topLevel)
    }
    | Some({contents: Ident(_)}) => Some("Unable to resolve module reference")
  }
};

open Infix;
let newHover = (~rootUri, ~file: SharedTypes.file, ~extra, ~getModule, ~markdown, loc) => {
  switch (loc) {
    | SharedTypes.Loc.Explanation(text) => Some(text)
    /* TODO store a "defined" for Open (the module) */
    | Open => Some("an open")
    | TypeDefinition(name, tdecl, stamp) => None
    /* TODO: show information on the module */
    | Module(LocalReference(stamp, tip)) => {
      let%opt md = Query.hashFind(file.stamps.modules, stamp);
      let%opt (file, declared) = References.resolveModuleReference(~file, ~getModule, md);
      showModule(~markdown, ~file, declared)
    }
    | Module(GlobalReference(moduleName, path, tip)) => {
      let%opt file = getModule(moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
      let%opt stamp = Query.exportedForTip(~env, name, tip);
      let%opt md = Query.hashFind(file.stamps.modules, stamp);
      let%opt (file, declared) = References.resolveModuleReference(~file, ~getModule, md);
      showModule(~markdown, ~file, declared)
    }
    | Module(_) => Some("A module")
    | TopLevelModule(name) => Some("File: " ++ name)
    | Typed(_, Definition(_, Attribute(_) | Constructor(_))) => None
    | Constant(t) => {
      Some(switch t {
      | Const_int(_) => "int"
      | Const_char(_) => "char"
      | Const_string(_) => "string"
      | Const_float(_) => "float"
      | Const_int32(_) => "int32"
      | Const_int64(_) => "int64"
      | Const_nativeint(_) => "int"
      })
    }
    | Typed(t, _) => {
      let typeString = 
        PrintType.default.expr(PrintType.default, t)
        |> PrintType.prettyString;

      let codeBlock = t => markdown
        ? "```\n" ++ t ++ "\n```"
        : t;

      let typeString = codeBlock(typeString);

      Some({
        let%opt ({name, deprecated, docstring}, {uri, moduleName}, res) = References.definedForLoc(
          ~file,
          ~getModule,
          loc,
        );

        let uri = Utils.startsWith(uri, rootUri)
          ? "<root>" ++ Utils.sliceToEnd(uri, String.length(rootUri))
          : uri;

        let parts = switch (res) {
          | `Declared => {
            [Some(typeString), docstring, Some(uri)]
          }
          | `Constructor({name: {txt}, args, res}) => {
            [Some(typeString),
            Some(codeBlock(txt ++ "(" ++ (args |. Belt.List.map(((t, _)) => {
              let typeString = 
                PrintType.default.expr(PrintType.default, t)
                |> PrintType.prettyString;
              typeString

            }) |> String.concat(", ")) ++ ")")),
            docstring,
            Some(uri)]
          }
          | `Attribute({SharedTypes.Type.Attribute.name: {txt}, typ}) => {
            [Some(typeString), docstring, Some(uri)]
          }
        };

        Some(String.concat("\n\n", parts |. Belt_List.keepMap(x => x)))
      } |? typeString)

    }
  };
}; 