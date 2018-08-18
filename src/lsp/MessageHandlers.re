module R = Result;
open Belt;
open R;
open TopTypes;
open Infix;

let extend = (obj, items) => Json.obj(obj) |?>> current => Json.Object(current @ items);

let log = Log.log;
let (-?>) = (a,b) => b;

let maybeHash = (h, k) => if (Hashtbl.mem(h, k)) { Some(Hashtbl.find(h, k)) } else { None };
type handler = Handler(string, Json.t => result('a, string), (state, 'a) => result((state, Json.t), string)) : handler;

let handlers: list((string, (state, Json.t) => result((state, Json.t), string))) = [
  ("textDocument/definition", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    let%try position = RJson.get("position", params) |?> Protocol.rgetPosition;
    let%try package = State.getPackage(uri, state);
    let%try data = State.getDefinitionData(uri, state, ~package);

    let position = Utils.cmtLocFromVscode(position);

    open Infix;
    {
      let%opt (uri, loc) =
        References.definitionForPos(
          ~package,
          ~file=data.file,
          ~extra=data.extra,
          ~getModule=State.fileForModule(state, ~package),
          position,
        );
      Some(Ok((state, Json.Object([
        ("uri", Json.String(uri)),
        ("range", Protocol.rangeOfLoc(loc)),
      ]))))
    } |? Ok((state, Json.Null))
  }),

  ("textDocument/signatureHelp", (state, params) => {
    let%try (uri, position) = Protocol.rPositionParams(params);
    let%try (text, verison, isClean) = maybeHash(state.documentText, uri) |> orError("No document text found");
    let%try package = State.getPackage(uri, state);
    let%try offset = PartialParser.positionToOffset(text, position) |> orError("invalid offset");
    let%try {extra} = State.getDefinitionData(uri, state, ~package);
    let position = Utils.cmtLocFromVscode(position);

    {
      let%opt (commas, labelsUsed, lident, i) = PartialParser.findFunctionCall(text, offset - 1);
      let lastPos = i + String.length(lident) - 1;
      let%opt pos = PartialParser.offsetToPosition(text, lastPos) |?>> Utils.cmtLocFromVscode;
      let%opt (_, loc) = References.locForPos(~extra, pos);
      let%opt typ = switch loc {
        | Typed(t, _) => Some(t)
        | _ => None
      };
      let rec loop = t => switch (t.Types.desc) {
        | Types.Tsubst(t)
        | Tlink(t) => loop(t)
        | Tarrow(label, argt, res, _) =>
          let (args, fin) = loop(res);
          ([(label, argt), ...args], fin)
        | _ => ([], t)
      };
      let (args, rest) = loop(typ);
      let%opt args = args == [] ? None : Some(args);
      open Rpc.J;
      Some(Ok((state, o([
        ("activeParameter", i(commas)),
        ("signatures", l([
          o([
            ("label", s(PrintType.default.expr(PrintType.default, typ) |> PrintType.prettyString )),
            ("parameters", l(args |. List.map(((label, argt)) => {
              o([
                ("label", s(label)),
                ("documentation", s(PrintType.default.expr(PrintType.default, argt) |> PrintType.prettyString))
              ])
            })))
          ])
        ]))
      ]))))

    } |? Ok((state, Json.Null));
  }),

  ("textDocument/completion", (state, params) => {
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try (text, verison, isClean) = maybeHash(state.documentText, uri) |> orError("No document text found");
    let%try package = State.getPackage(uri, state);
    let%try offset = PartialParser.positionToOffset(text, pos) |> orError("invalid offset");
    /* TODO get last non-syntax-erroring definitions */
    /* let%try (file, extra) = State.fileForUri(state, ~package, uri) |> orError("No definitions"); */
    open Rpc.J;
    let%try completions = switch (PartialParser.findCompletable(text, offset)) {
    | Nothing => {
      Log.log("Nothing completable found :/");
      Ok([])
    }
    | Labeled(string) => {
      Log.log("don't yet support completion for argument labels, but I hope to soon!");
      Ok([])
    }
    | Lident(string) => {
      /* Log.log("Completing for string " ++ string); */
      let parts = Str.split(Str.regexp_string("."), string);
      let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
      let currentModuleName = String.capitalize(Filename.chop_extension(Filename.basename(uri)));
      let opens = PartialParser.findOpens(text, offset);

      let%try {SharedTypes.file, extra} = State.getBestDefinitions(uri, state, ~package);
      let useMarkdown = !state.settings.clientNeedsPlainText;
      let allModules = List.map(package.localModules, fst) @ List.map(package.dependencyModules, fst);
      let items = NewCompletions.get(
        ~full={file, extra},
        ~package,
        ~opens,
        ~getModule=State.fileForModule(state, ~package),
        ~allModules,
        pos,
        parts,
      );
      /* Log.log("Got items: " ++ string_of_int(List.length(items))); */

      List.map(items, ((uri, {name: {txt: name, loc: {loc_start: {pos_lnum}}}, deprecated, docstring, contents})) => o([
        ("label", s(name)),
        ("kind", i(NewCompletions.kindToInt(contents))),
        ("detail", NewCompletions.detail(name, contents) |> s),
        ("documentation",

        s((docstring |? "No docs") ++ "\n\n" ++
        uri ++ ":" ++ string_of_int(pos_lnum))),
        /* docstring |?>> Protocol.contentKind(useMarkdown) |? Json.Null), */
        /* ("data", switch kind {
          | RootModule(cmt, src) => o([("cmt", s(cmt)), ("name", s(label)), ...(fold(src, [], src => [("src", s(src))]))])
          | _ => null
          }) */
      ])) |. Ok;
    }
    };
    Ok((state, l(completions)))
  }),

  ("completionItem/resolve", (state, params) => {
    Ok((state, Json.Null))
    /* switch (params |> Json.get("documentation") |?> Json.string) {
    | Some(_) => Ok((state, params))
    | None =>
      let result = (params |> Json.get("data")
      |?> data => Json.get("cmt", data) |?> Json.string
      |?> cmt => Json.get("src", data) |?> Json.string
      |?> src => Json.get("name", data) |?> Json.string
      |?>> name => {
        let (detail, docs) = Completions.getModuleResults(name, state, cmt, Some(src));

        open Rpc.J;
        extend(params, [
          ("detail", detail |?>> s |? null),
          ("documentation", docs |?>> Protocol.contentKind(!state.settings.clientNeedsPlainText) |? null),
        ]) |? params
      }) |? params;
      Ok((state, result))
    } */
  }),

  ("textDocument/documentHighlight", (state, params) => {
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);

    let res = {
      let pos = Utils.cmtLocFromVscode(pos);
      let%opt (file, extra) = State.fileForUri(state, ~package, uri) |> R.toOptionAndLog;

      let%opt_wrap refs = References.forPos(~file, ~extra, pos);
      open Rpc.J;
      (state, l(List.map(refs, (loc) => o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("kind", i(2))
      ]))));
    } |? (state, Json.Null);

    Ok(res)
  }),

  ("textDocument/references", (state, params) => {
    open InfixResult;
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);
    let%try_wrap (file, extra) = State.fileForUri(state, ~package, uri);

    open Infix;

    {
      let%opt (_, loc) = References.locForPos(~extra, Utils.cmtLocFromVscode(pos));
      let allModules = List.map(package.localModules, fst);
      let%opt allReferences = References.forLoc(
        ~file,
        ~extra,
        ~allModules,
        ~getModule=State.fileForModule(state, ~package),
        ~getExtra=State.extraForModule(state, ~package),
        loc
      ) |> toOptionAndLog;

      open Rpc.J;
      Some((
        state,
        List.map(
          allReferences,
          ((fname, references)) => {
            let locs = fname == uri
              ? List.keep(references, loc => !Protocol.locationContains(loc, pos))
              : references;
            List.map(locs, loc => Protocol.locationOfLoc(~fname, loc))
          }
        )
        |. List.toArray
        |. List.concatMany
        |. l
      ));

    } |? (state, Json.Null);
  }),

  ("textDocument/rename", (state, params) => {
    open InfixResult;
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);
    let%try (file, extra) = State.fileForUri(state, ~package, uri);
    let%try newName = RJson.get("newName", params);

    open Infix;
    {
      let%opt (_, loc) = References.locForPos(~extra, Utils.cmtLocFromVscode(pos));
      let allModules = List.map(package.localModules, fst);
      let%opt allReferences = References.forLoc(
        ~file,
        ~extra,
        ~allModules,
        ~getModule=State.fileForModule(state, ~package),
        ~getExtra=State.extraForModule(state, ~package),
        loc
      ) |> toOptionAndLog;

      open Rpc.J;
      Some(Ok((
        state,
        o([
          ("changes", o(
            List.map(allReferences, ((fname, references)) =>

              (fname,
                l(List.map(references, loc => o([
                  ("range", Protocol.rangeOfLoc(loc)),
                  ("newText", newName),
                ])))
              )

            )
          ))
        ])
      )));
    } |? Ok((state, Json.Null))
  }),

  ("textDocument/codeLens", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    /* let%try package = State.getPackage(uri, state); */
    switch (State.getPackage(uri, state)) {
    | Error(message) => {
      let items = [("Unable to load compilation data: " ++ message, {
        Location.loc_start: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
        Location.loc_end: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
        loc_ghost: false,
      })];
      open Rpc.J;
      Ok((state, l(List.map(items, ((text, loc)) => o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("command", o([
          ("title", s(text)),
          ("command", s(""))
        ]))
      ])))))
    }
    | Ok(package) =>

      /* Log.log("<< codleens me please"); */
      open Infix;

      let topLoc = {
        Location.loc_start: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
        Location.loc_end: {Lexing.pos_fname: "", pos_lnum: 1, pos_bol: 0, pos_cnum: 0},
        loc_ghost: false,
      };

      let getLensItems = ({SharedTypes.file, extra}) => {
        let showToplevelTypes = state.settings.perValueCodelens; /* TODO config option */
        let lenses = showToplevelTypes ? file.contents.topLevel |. List.keepMap(({name: {loc}, contents}) => {
          switch contents {
          | Value({typ}) => PrintType.default.expr(PrintType.default, typ) |> PrintType.prettyString |> s => Some((s, loc))
          | _ => None
          }
        }) : [];

        let showOpens = state.settings.opensCodelens;
        /* let lenses = showOpens ? lenses @ Definition.opens(moduleData) : lenses; */
        let lenses = showOpens ? lenses @ {
          CodeLens.forOpens(extra)
        } : lenses;

        let showDependencies = state.settings.dependenciesCodelens;
        let lenses = showDependencies ? [("Dependencies: " ++ String.concat(", ",
          List.map(SharedTypes.hashList(extra.externalReferences), fst)
        ), topLoc), ...lenses] : lenses;

        lenses
      };

      let items = {
        let full = switch (State.getCompilationResult(uri, state, ~package)) {
          | Ok(Success(_, full)) => {
            Ok(Some(full))
          }
          | Ok(_) => Ok(State.getLastDefinitions(uri, state))
          | Error(m) => Error(m)
        };
        switch full {
          | Error(message) => [(message, topLoc)]
          | Ok(None) => [("Unable to get compilation data", topLoc)]
          | Ok(Some(full)) => getLensItems(full)
        };
      };
      open Rpc.J;
      Ok((state, l(List.map(items, ((text, loc)) => o([
        ("range", Protocol.rangeOfLoc(loc)),
        ("command", o([
          ("title", s(text)),
          ("command", s(""))
        ]))
      ])))))
    }
  }),

  ("textDocument/hover", (state, params) => {
    let%try (uri, pos) = Protocol.rPositionParams(params);
    let%try package = State.getPackage(uri, state);
    let%try (file, extra) = State.fileForUri(state, ~package, uri);

    {
      let pos = Utils.cmtLocFromVscode(pos);
      let%opt (location, loc) = References.locForPos(~extra, pos);
      let%opt text = Hover.newHover(
        ~rootUri=state.rootUri,
        ~file,
        ~extra,
        ~getModule=State.fileForModule(state, ~package),
        ~markdown=!state.settings.clientNeedsPlainText,
        loc
      );

      open Rpc.J;
      Some(Ok((state, o([
        ("range", Protocol.rangeOfLoc(location)),
        ("contents", text |> Protocol.contentKind(!state.settings.clientNeedsPlainText))
      ]))))
    } |? Ok((state, Json.Null));
  }),

  ("textDocument/rangeFormatting", fun (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    let%try package = State.getPackage(uri, state);
    let%try (start, end_) = RJson.get("range", params) |?> Protocol.rgetRange;
    let%try refmtPath = State.refmtForUri(uri, package);
    let%try refmtPath = refmtPath |> R.orError("Cannot refmt ocaml yet");
    
    let text = State.getContents(uri, state);
    open Infix;
    let maybeResult = {
      let%try startPos = PartialParser.positionToOffset(text, start) |> orError("Invalid start position");
      let%try endPos = PartialParser.positionToOffset(text, end_) |> orError("Invalid end position");

      /** TODO: instead of bailing, it should extend the selection to encompass the whole line, and then go for it. */
      if (fst(start) == fst(end_) && text.[endPos] != '\n') {
        Belt.Result.Ok((state, Rpc.J.null));
      } else {
        let substring = String.sub(text, startPos, endPos - startPos);

        open Utils;
        let trailingNewlines = substring |> countTrailing('\n');
        let (leadingNewlines, charsToFirstLines) = {
          let splitted = substring |> split_on_char('\n');
          let l = List.length(splitted);
          let rec loop = (i, leadingLines, skipChars) => {
            let line = List.getExn(splitted, i);
            switch (line |. String.trim |. String.length) {
            | 0 => loop(i + 1, leadingLines + 1, skipChars + (line |> String.length))
            | _ => (leadingLines, skipChars + 1)
            };
          };
          loop(0, 0, 0);
        };

        /* Strip all leading new lines from substring */
        let (startPos, substring) =
          if (leadingNewlines > 0) {
            (
              startPos + leadingNewlines,
              String.sub(
                substring,
                charsToFirstLines,
                String.length(substring) - charsToFirstLines,
              ),
            );
          } else {
            (startPos, substring);
          };

        let indent = getFullLineOfPos(startPos, text) |> countLeading(' ');
        let cursorToFirstLineSpaces = substring |> countLeading(' ');

        let appendIndent = (~firstLineSpaces=?, indent, s) => {
          let indentString = repeat(indent, " ");
          if (indent == 0) {
            s;
          } else {
            split_on_char('\n', s)
            |. List.mapWithIndex((index, line) =>
                switch (index, firstLineSpaces, String.length(line)) {
                | (_, _, 0) => line
                | (0, Some(spaces), _) => repeat(spaces, " ") ++ line
                | _ => indentString ++ line
                }
              )
            |> String.concat("\n");
          };
        };
        let%try_wrap text = AsYouType.format(~formatWidth=state.settings.formatWidth, substring, refmtPath);
        Rpc.J.(
          state,
          l([
            o([
              ("range", Infix.(|!)(Json.get("range", params), "what")),
              (
                "newText",
                s(
                  repeat(leadingNewlines, "\n")
                  ++ appendIndent(
                      ~firstLineSpaces=cursorToFirstLineSpaces,
                      indent,
                      text,
                    )
                  ++ repeat(trailingNewlines, "\n"),
                ),
              ),
            ]),
          ]),
        );
      };
    };
    maybeResult
  }),

  ("textDocument/documentSymbol", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    let%try package = State.getPackage(uri, state);

    let%try (file, extra) = State.fileForUri(state, ~package, uri);

    open SharedTypes;

    let rec getItems = ({Module.topLevel}) => {
      let fn = ({name: {txt, loc}, extentLoc, contents}) => {
        let (item, siblings) = switch contents {
          | Module.Value(v) => (Protocol.variableKind(v.typ), [])
          | Type(t) => (Protocol.typeKind(t.typ), [])
          | Module(Structure(contents)) => (`Module, getItems(contents))
          | Module(Ident(_)) => (`Module, [])
          | ModuleType(_) => (`ModuleType, [])
        };
        [(txt, extentLoc, item), ...siblings]
      };
      let x = topLevel |. List.map(fn) |. List.toArray |. List.concatMany;
      x
    };

    (getItems(file.contents) |> items => {
      open Rpc.J;
      Ok((state, l(List.map(items, ((name, loc, typ)) => o([
        ("name", s(name)),
        ("kind", i(Protocol.symbolKind(typ))),
        ("location", Protocol.locationOfLoc(loc)),
        /* ("containerName", s(String.concat(".", path))) */
      ])))))
    })
  }),

  ("textDocument/formatting", (state, params) => {
    open InfixResult;
    let%try uri = params |> RJson.get("textDocument") |?> RJson.get("uri") |?> RJson.string;
    let%try package = State.getPackage(uri, state);
    let text = State.getContents(uri, state);
    let%try refmtPath = State.refmtForUri(uri, package);
    let%try refmtPath = refmtPath |> R.orError("Cannot refmt ocaml yet");
    let%try_wrap newText = AsYouType.format(~formatWidth=state.settings.formatWidth, text, refmtPath);
    open Rpc.J;
    (state, text == newText ? Json.Null : l([o([
      ("range", Protocol.rangeOfInts(
        0, 0,
        List.length(Str.split(Str.regexp_string("\n"), text)) + 1, 0
      )),
      ("newText", s(newText))
    ])]))
  })
];
