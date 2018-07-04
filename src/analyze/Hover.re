open Result;

let getHover = (uri, line, character, state, ~package) => {
  let result = State.getCompilationResult(uri, state, ~package);
  /* TODO report errors better if we're beyond where the parser error occurred */
  switch result {
  | AsYouType.ParseError(text) => Error("Cannot hover -- parser error: " ++ text)
  | TypeError(_, cmt, data)
  | Success(_, cmt, data) =>
    Ok(switch (Definition.locationAtPos((line, character), data)) {
    | None => None
    | Some((loc, {desc: Types.Tnil}, _)) => None
    | Some((loc, expr, defn)) =>
      let useMarkdown = !state.clientNeedsPlainText;
      let typ =
        PrintType.default.expr(PrintType.default, expr)
        |> PrintType.prettyString;
      let typ = useMarkdown ? "```\n" ++ typ ++ "\n```" : typ;
      let tooltip =
        switch (State.getResolvedDefinition(uri, defn, data, state, ~package)) {
        | None => typ
        | Some((loc, docs, docUri)) =>
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