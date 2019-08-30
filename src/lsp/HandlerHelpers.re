module R = Result;
open Belt;
open TopTypes;
open Infix;

let maybeHash = (h, k) => if (Hashtbl.mem(h, k)) { Some(Hashtbl.find(h, k)) } else { None };
let getPackage = Packages.getPackage(~reportDiagnostics=NotificationHandlers.reportDiagnostics);

let rec getItemsFromModule = ({SharedTypes.Module.topLevel}) => {
  open SharedTypes;
  let fn = ({name: {txt}, extentLoc, contents}) => {
    let (item, siblings) = switch contents {
      | Module.Value(v) => (v.typ.variableKind, [])
      | Type(t) => (t.typ.declarationKind, [])
      | Module(Structure(contents)) => (`Module, getItemsFromModule(contents))
      | Module(Ident(_)) => (`Module, [])
      | ModuleType(_) => (`ModuleType, [])
    };
    if (extentLoc.loc_ghost) {
      siblings
    } else {
      [(txt, extentLoc, item), ...siblings]
    }
  };
  let x = topLevel |. List.map(fn) |. List.toArray |. List.concatMany;
  x
};

let getTypeAndHoverTextForPosition = (~uri: string, ~position: (int, int), ~state: state)  => {
  let%try package = getPackage(uri, state);
  let%try (text, _version, _isClean) = maybeHash(state.documentText, uri) |> R.orError("No document text found");
  let%try offset = PartialParser.positionToOffset(text, position) |> R.orError("invalid offset");
  let%try full = State.getDefinitionData(uri, state, ~package);


  let%try (commas, _labelsUsed, lident, i) = PartialParser.findFunctionCall(text, offset - 1) |> R.orError("Could not find function call");
  let lastPos = i + String.length(lident) - 1;
  let%try pos = PartialParser.offsetToPosition(text, lastPos) |?>> Utils.cmtLocFromVscode |> R.orError("Could not offset to position");
  let {SharedTypes.file, extra} = full;

  let res = (switch (References.locForPos(~extra, pos)) {
  | None =>
    let tokenParts = Utils.split_on_char('.', lident);
    let rawOpens = PartialParser.findOpens(text, offset);
    let%opt declared = NewCompletions.findDeclaredValue(
      ~useStdlib=package.compilerVersion->BuildSystem.usesStdlib,
      ~full,
      ~package,
      ~rawOpens,
      ~getModule=State.fileForModule(state, ~package),
      pos,
      tokenParts
    );
    let typ = declared.contents.typ;
    Some((typ, declared.docstring |? "No docs"))
  | Some((_, loc)) =>
    let%opt typ =
      switch (loc) {
      | Typed(t, _) => Some(t)
      | _ => None
      };
    let%opt hoverText =
      Hover.newHover(
        ~rootUri=state.rootUri,
        ~file,
        ~getModule=State.fileForModule(state, ~package),
        ~markdown=! state.settings.clientNeedsPlainText,
        ~showPath=state.settings.showModulePathOnHover,
        loc,
      );
    Some((typ, hoverText));
  });

  res |> R.orError("Failed to find type info and hover text")
};

let getArgumentsForPosition = (~uri: string, ~position: (int, int), ~state: state) => {
  let%try (typ, hoverText) = getTypeAndHoverTextForPosition(~uri, ~position, ~state);
  let (args, _rest) = typ.getArguments();
  Ok(args)
};