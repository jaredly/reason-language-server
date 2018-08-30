open Result;

let showModuleTopLevel = (~name, ~markdown, topLevel: list(SharedTypes.declared(SharedTypes.Module.item))) => {
  let contents =
    topLevel
    |. Belt.List.map(item =>
         switch (item.contents) {
         /** TODO pretty print module contents */
         | Module(_) => "  module " ++ item.name.txt ++ ";"
         | Type({typ}) =>
           "  "
           ++ (PrintType.indentGroup(PrintType.default.decl(PrintType.default, item.name.txt, item.name.txt, typ)) |> PrintType.prettyString)
         | Value({typ}) =>
           "  let "
           ++ item.name.txt
           ++ ": "
           ++ (PrintType.indentGroup(PrintType.default.expr(PrintType.default, typ)) |> PrintType.prettyString)
           ++ ";"
         | ModuleType(_) => "  module type " ++ item.name.txt ++ ";"
         }
       )
    |> String.concat("\n");
  let full = "module " ++ name ++ " = {" ++ "\n" ++ contents ++ "\n}";
  Some(markdown ? "```\n" ++ full ++ "\n```" : full);
};

let showModule = (~markdown, ~file: SharedTypes.file, ~name, declared: option(SharedTypes.declared(SharedTypes.Module.kind))) => {
  switch declared {
    | None => showModuleTopLevel(~name, ~markdown, file.contents.topLevel)
    | Some({contents: Structure({topLevel})}) => {
      showModuleTopLevel(~name, ~markdown, topLevel)
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
      let name = switch declared {
        | Some(d) => d.name.txt
        | None => file.moduleName
      };
      showModule(~name, ~markdown, ~file, declared)
    }
    | Module(GlobalReference(moduleName, path, tip)) => {
      let%opt file = getModule(moduleName);
      let env = {Query.file, exported: file.contents.exported};
      let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
      let%opt stamp = Query.exportedForTip(~env, name, tip);
      let%opt md = Query.hashFind(file.stamps.modules, stamp);
      let%opt (file, declared) = References.resolveModuleReference(~file, ~getModule, md);
      let name = switch declared {
        | Some(d) => d.name.txt
        | None => file.moduleName
      };
      showModule(~name, ~markdown, ~file, declared)
    }
    | Module(_) => None
    | TopLevelModule(name) => {
      let%opt file = getModule(name);
      showModule(~name=file.moduleName, ~markdown, ~file, None)
    }
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

      let env = {Query.file, exported: file.contents.exported};
      let codeBlock = t => markdown
        ? "```\n" ++ t ++ "\n```"
        : t;


      let typeString = codeBlock(typeString);
      let extraTypeInfo = {
        let%opt (env, {name, contents}) = Query.digConstructor(~env, ~getModule, t);
        switch (contents.kind) {
          | Record(attributes) => {
            Some("\n\n" ++ codeBlock("type " ++ name.txt  ++ " = {\n" ++ (attributes |. Belt.List.map(({name: {txt}, typ}) => {
              "  " ++ txt ++ ": " ++ (
                PrintType.default.expr(PrintType.default, typ)
                |> PrintType.prettyString
              )
            }) |> String.concat(",\n")) ++ "\n}"))
          }
          | Variant(constructors) => {
            Some("\n\n" ++ codeBlock(
              "type " ++ name.txt ++ " = \n" ++
              (
                constructors |.Belt.List.map(({name: {txt}, args}) => {
                  "  | " ++ txt ++ (
                    args == []
                    ? ""
                    : "(" ++ String.concat(", ",
                    args |> List.map(((typ, _)) => {
                      PrintType.default.expr(PrintType.default, typ)
                      |> PrintType.prettyString

                    })
                    ) ++ ")"
                  )
                }) |> String.concat("\n")
              )
            ))
          }
          | _ => None
        }
      };
      let typeString = typeString ++ (extraTypeInfo |? "");

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