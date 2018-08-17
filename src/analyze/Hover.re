open Result;

open Infix;
let newHover = (~rootUri, ~file, ~extra, ~getModule, ~markdown, loc) => {
  switch (loc) {
    | SharedTypes.Loc.Explanation(text) => Some(text)
    /* TODO store a "defined" for Open (the module) */
    | Open => Some("an open")
    | TypeDefinition(name, tdecl, stamp) => None
    | Module(_) => Some("its a module")
    | TopLevelModule(name) => Some("File: " ++ name)
    | Typed(_, Definition(_, Attribute(_) | Constructor(_))) => None
    | Typed(t, _) => {
      let typeString = 
        PrintType.default.expr(PrintType.default, t)
        |> PrintType.prettyString;

      let typeString = markdown
        ? "```\n" ++ typeString ++ "\n```"
        : typeString;

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
            [Some(typeString), docstring, Some(uri)]
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