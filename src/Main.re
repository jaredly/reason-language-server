open Infix;
open Result;

let out = open_out("/Users/jared/ls.log");

let log = msg => {
  output_string(stderr, msg ++ "\n");
  output_string(out, msg ++ "\n");
  flush(out);
};

let extend = (obj, items) => Json.obj(obj) |?>> current => Json.Object(current @ items);

/**
 * we get initialized, with a rootUri
 * if the rootPath doesn't have a bsconfig.json, show that as a notification, and be quiet afterwards
 *
 * load up dependencies, probably
 * although maybe I only want to load them on a case by case?
 * yeah that could get memory hoggy quick
 * also it would be nice to clean house occasionally, have a LRU cache or something.
 *
 * Anyway, let's talk about completion.
 * I'll do my hacky parser to detect "open"s, so I know what things to load up
 * We'll defer as much as possible, but:
 *
 * First, have a listing of all accessible modules (name, (cmti, sourcefile))
 *
 * then, if it gets requested via autocomplete, load the cmti to get infos
 *
 * also, if it's "open"d, we'll load it up n show it.
 *
 * And I'll do the thing where I assume pervasives is open.
 *
 *
 * Probably punt on documentation for the moment.
*/;

let capabilities =
  Rpc.J.(
    o([
      ("textDocumentSync", i(1)),
      ("hoverProvider", t),
      /* TODO list # and . as trigger characters */
      ("completionProvider", o([
        ("resolveProvider", t),
        ("triggerCharacters", l([s(".")]))
      ])),
      ("signatureHelpProvider", t),
      ("definitionProvider", t),
      ("typeDefinitionProvider", t),
      ("referencesProvider", t),
      ("documentSymbolProvider", t),
      /* ("codeActionProvider", t), */
      ("codeLensProvider", t),
      /*
       * Found how to do the showReferences thing
       * https://github.com/Microsoft/vscode/blob/c6b1114292288e76e2901e05e860faf3a08b4b5a/extensions/typescript-language-features/src/features/implementationsCodeLensProvider.ts
       * but it seems I need to instantiate the object from javascript
       */
      /* ("executeCommandProvider", o([

      ])) */
      /* ("executeCommandOptions", t), */
      ("documentFormattingProvider", t),
      ("renameProvider", t)
    ])
);

let parseUri = uri => {
  if (Utils.startsWith(uri, "file://")) {
    Some(Utils.sliceToEnd(uri, String.length("file://")))
  } else {
    None
  }
};

open State;

let maybeHash = (h, k) => if (Hashtbl.mem(h, k)) { Some(Hashtbl.find(h, k)) } else { None };

let getInitialState = (params) => {
  let uri = Json.get("rootUri", params) |?> Json.string |?> parseUri |> resultOfOption("No root uri");
  open InfixResult;
  uri |?> uri => {
    Files.readFile(uri /+ "bsconfig.json") |> orError("No bsconfig.json found") |?>> Json.parse |?>> config => {
      let compiledBase = FindFiles.getCompiledBase(uri, config);
      let localModules = FindFiles.findProjectFiles(~debug=false, None, uri, config, compiledBase) |> List.map(((full, rel)) => (FindFiles.getName(rel), (full, rel)));
      let dependencyModules = FindFiles.findDependencyFiles(~debug=false, uri, config);
      let cmtMap = Hashtbl.create(30);
      let documentText = Hashtbl.create(5);
      let docs = Hashtbl.create(30);
      localModules |> List.iter(((modName, (cmt, source))) => {
        let cmt_info = Cmt_format.read_cmt(cmt);
        Hashtbl.replace(cmtMap, cmt, cmt_info);
        Infix.(Docs.forCmt(State.docConverter(source), cmt_info) |?< info => Hashtbl.replace(docs, modName, info))
      });
      {
        rootPath: uri,
        documentText,
        localCompiledBase: compiledBase,
        localModules,
        localCompiledMap: localModules |> List.map(((_, (cmt, src))) => (src, cmt)),
        dependencyModules,
        cmtMap,
        docs,
      }
    };
  }
};

let getTextDocument = doc => Json.get("uri", doc) |?> Json.string
    |?> uri => Json.get("version", doc) |?> Json.number
    |?> version => Json.get("text", doc) |?> Json.string
    |?>> text => (uri, version, text);

let notificationHandlers: list((string, (state, Json.t) => result(state, string))) = [
  ("textDocument/didOpen", (state, params) => {
    (params |> Json.get("textDocument") |?> getTextDocument |?>> ((uri, version, text))  => {
      /* Hmm how do I know if it's modified? */
      Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), true));
      state
    }) |> orError("Invalid params")
  }),
  ("textDocument/didChange", (state, params) => {
    open InfixResult;
    params |> RJson.get("textDocument") |?> doc => RJson.get("uri", doc) |?> RJson.string
    |?> uri => RJson.get("version", doc) |?> RJson.number
    |?> version => RJson.get("contentChanges", params) |?> RJson.array
    |?> changes => List.nth(changes, List.length(changes) - 1) |> RJson.get("text") |?> RJson.string
    |?>> text => {
      /* Hmm how do I know if it's modified? */
      Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), false));
      state
    }
  }),
];

let markup = text => Json.Object([("kind", Json.String("markdown")), ("value", Json.String(text))]);

let messageHandlers: list((string, (state, Json.t) => result((state, Json.t), string))) = [
  ("textDocument/completion", (state, params) => {
    open InfixResult;
    (RJson.get("textDocument", params) |?> RJson.get("uri") |?> RJson.string
    |?> (maybeHash(state.documentText) |.> orError("No document text found")) |?> ((text, version, isClean)) => RJson.get("position", params) |?> Protocol.rgetPosition |?> ((line, character)) => {
      (PartialParser.positionToOffset(text, line, character) |> orError("invalid offset")) |?>> offset => {
        open Rpc.J;
        log("Finding in\n" ++ String.sub(text, 0, offset));
        let completions = switch (PartialParser.findCompletable(text, offset)) {
        | Nothing => []
        | Labeled(string) => []
        /* [o([("label", s(string ++ "_arg"))])] */
        | Lident(string) => {
          log("Completing for string " ++ string);
          let parts = Str.split(Str.regexp_string("."), string);
          let parts = string.[String.length(string) - 1] == '.' ? parts @ [""] : parts;
          /* let (scope, name) = string.[String.length(string) - 1] == '.' ? (parts, "") : {
            let rec loop = (l) => switch l {
            | [] => assert(false)
            | [one] => ([], one)
            | [one, ...more] => {
                let (scope, name) = loop(more);
                ([one, ...scope], name)
              };
            };
            loop(parts)
          }; */
          Completions.get(parts, state) |> List.map(({Completions.kind, label, detail, documentation}) => o([
            ("label", s(label)),
            ("kind", i(Completions.kindToInt(kind))),
            ("detail", Infix.(detail |?>> s |? null)),
            ("documentation", Infix.(documentation |?>> markup |? null)),
            ("data", switch kind {
              | RootModule(cmt) => s(cmt)
              | _ => null
              })
          ]))
        }
        };
        (state, l(completions))
      }
    });
  }),
  ("completionItem/resolve", (state, params) => {
    switch (params |> Json.get("documentation") |?> Json.string) {
    | Some(_) => Ok((state, params))
    | None =>
      let result = (params |> Json.get("data") |?> Json.string |?>> cmt => {
        let cmt_infos = Cmt_format.read_cmt(cmt);
        Hashtbl.replace(state.cmtMap, cmt, cmt_infos);
        let (detail, docs) = Completions.getModuleResults(State.docConverter("fake.ml"), cmt_infos);

        /* TODO */
        open Rpc.J;
        extend(params, [
          ("detail", detail |?>> s |? null),
          ("documentation", docs |?>> markup |? null),
        ]) |? params
      }) |? params;
      Ok((state, result))
    }
  }),
  ("textDocument/codeLens", (state, params) => {
    open Protocol;
    Ok((state, Rpc.J.(l([
      o([
        ("range", range(~start=pos(~line=0, ~character=0), ~end_=pos(~line=0, ~character=0))),
        ("command", o([
          ("title", s("Party all day")),
          ("command", s("")),
        ]))
      ]),
    ]))))
  }),
  ("textDocument/hover", (state, params) => {
    Ok((state, Json.String("Ok folks")))
  })
];



let main = () => {
  log("Booting up");
  BasicServer.run(
    ~ignoreErrors=true,
    ~log,
    ~messageHandlers,
    ~notificationHandlers,
    ~capabilities=_params => capabilities,
    ~getInitialState
  );
  log("Finished");
  close_out(out);
};