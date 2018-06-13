open Result;

let getHover = (uri, line, character, state) => {
  let result = State.getCompilationResult(uri, state);
  let result =
    switch result {
    | AsYouType.ParseError(text) =>
      Error("Cannot hover -- parser error: " ++ text)
    | TypeError(_, cmt, data) => Ok(data)
    | Success(_, cmt, data) => Ok(data)
    };
  switch result {
  | Error(t) => Some((t, Location.none))
  | Ok(data) =>
    switch (Definition.locationAtPos((line, character), data)) {
    | None => None
    | Some((loc, expr, defn)) =>
      let useMarkdown = state.clientCapabilities.hoverMarkdown;
      let typ =
        PrintType.default.expr(PrintType.default, expr)
        |> PrintType.prettyString;
      let typ = useMarkdown ? "```\n" ++ typ ++ "\n```" : typ;
      let tooltip =
        switch (State.getResolvedDefinition(uri, defn, data, state)) {
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
    }
  };
};