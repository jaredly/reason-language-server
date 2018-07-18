open Result;

let getHover = (uri, line, character, state, ~package) => {
  let result = State.getCompilationResult(uri, state, ~package);
  /* TODO report errors better if we're beyond where the parser error occurred */
  switch result {
  /* | AsYouType.ParseError(text) => Error("Cannot hover -- parser error: " ++ text) */
  | TypeError(_, cmt, data)
  | Success(_, cmt, data) =>
    Ok(switch (Definition.locationAtPos((line, character), data)) {
    | None => switch (Definition.explanationAtPos((line, character), data)) {
        | None => None
        | Some((loc, text)) =>
          Some((text, loc))
      }
    | Some((loc, {desc: Types.Tnil}, _)) => None
    | Some((loc, expr, defn)) =>
      let useMarkdown = !state.settings.clientNeedsPlainText;
      let typ =
        PrintType.default.expr(PrintType.default, expr)
        |> PrintType.prettyString;
      let typ = useMarkdown ? "```\n" ++ typ ++ "\n```" : typ;
      let tooltip =
        switch (State.getResolvedDefinition(uri, defn, data, state, ~package)) {
        | None => typ
        | Some((item, loc, docs, docUri)) =>
          typ
          ++ (
            switch docs {
            | Some(t) => "\n\n" ++ t
            | None => ""
            }
          )
          ++ "\n\n" ++ (Infix.fold(docUri, "", docUri => (useMarkdown ? "*" : "")
          ++ (Utils.startsWith(docUri, state.rootUri ++ "/")
          ? Utils.sliceToEnd(docUri, String.length(state.rootUri ++ "/"))
          : uri) ++ (useMarkdown ? "*" : "")))
        };
      Some((tooltip, loc));
    })
  };
};

open Infix;
let newHover = (~file, ~extra, ~getModule, ~markdown, loc) => {
  switch (loc) {
    | SharedTypes.Loc.Explanation(text) => Some(text)
    /* TODO store a "defined" for Open (the module) */
    | Open => Some("an open")
    | TypeDefinition(name, tdecl, stamp) => None
    | Module(_) => Some("its a module")
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