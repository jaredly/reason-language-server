open Result;

let digConstructor = (~env, ~getModule, expr) => {
  let%opt path = Process_402.digConstructor(expr);
  switch (Query.resolveFromCompilerPath(~env, ~getModule, path)) {
  | `Not_found => None
  | `Stamp(stamp) =>
    let%opt t = Query.hashFind(env.file.stamps.types, stamp);
    Some((env, t));
  | `Exported(env, name) =>
    let%opt stamp = Query.hashFind(env.exported.types, name);
    let%opt t = Query.hashFind(env.file.stamps.types, stamp);
    Some((env, t));
  | _ => None
  }
};

let showModuleTopLevel = (~name, ~markdown, topLevel: list(SharedTypes.declared(SharedTypes.Module.item))) => {
  let contents =
    topLevel
    |. Belt.List.map(item =>
         switch (item.contents) {
         /** TODO pretty print module contents */
         | Module(_) => "  module " ++ item.name.txt ++ ";"
         | Type({typ}) =>
           "  "
           ++ (Process_402.PrintType.indentGroup(Process_402.PrintType.default.decl(Process_402.PrintType.default, item.name.txt, item.name.txt, typ)) |> Process_402.PrintType.prettyString)
         | Value({typ}) =>
           "  let "
           ++ item.name.txt
           ++ ": "
           ++ (Process_402.PrintType.indentGroup(Process_402.PrintType.default.expr(Process_402.PrintType.default, typ)) |> Process_402.PrintType.prettyString)
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
    | Typed(dontUseT, _) => {
      let typeString = dontUseT.toString();
      let extraTypeInfo = {
        let env = {Query.file, exported: file.contents.exported};
        let%opt (env, {name: {txt}, contents: {typ}}) = digConstructor(~env, ~getModule, t);
        Some(typ.toString())
      };

      let codeBlock = text => markdown ? "```\n" ++ text ++ "\n```" : text;
      let typeString = codeBlock(typeString);
      let typeString = typeString ++ (switch (extraTypeInfo) {
        | None => ""
        | Some(extra) => "\n\n" ++ codeBlock(extra)
      });

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
                Process_402.PrintType.default.expr(Process_402.PrintType.default, t)
                |> Process_402.PrintType.prettyString;
              typeString

            }) |> String.concat(", ")) ++ ")")),
            docstring,
            Some(uri)]
          }
          | `Attribute({SharedTypes.Type.Attribute.name: {txt}, typ}) => {
            [Some(typeString), docstring, Some(uri)]
          }
        };

        Some(String.concat("\n\n", parts |. Belt.List.keepMap(x => x)))
      } |? typeString)

    }
  };
}; 