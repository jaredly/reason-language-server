open Infix;
open Result;

let out = open_out("/Users/jared/ls.log");

let log = msg => {
  output_string(stderr, msg ++ "\n");
  output_string(out, msg ++ "\n");
  flush(out);
};

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
      ("completionProvider", t),
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

type state = {
  rootPath: string,
  localCompiledBase: string,
  localModules: list((string, (string, string))),
  localCompiledMap: list((string, string)),
  dependencyModules: list((FindFiles.modpath, (string, string))),
  cmtMap: Hashtbl.t(string, Cmt_format.cmt_infos),
  documentText: Hashtbl.t(string, (string, int, bool)),
  /* workspace folders... */
};

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
      localModules |> List.iter(((_, (cmt, source))) => {
        Hashtbl.replace(cmtMap, cmt, Cmt_format.read_cmt(cmt));
      });
      {
        rootPath: uri,
        documentText,
        localCompiledBase: compiledBase,
        localModules,
        localCompiledMap: localModules |> List.map(((_, (cmt, src))) => (src, cmt)),
        dependencyModules,
        cmtMap,
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
      Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), true));
      state
    }
  })
];

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
        | Labeled(string) => [o([("label", s(string ++ "_arg"))])]
        | Lident(string) => [o([("label", s(string ++ "_lident"))])]
        };
        (state, l(completions))
      }
    });
    /* {textDocument: {uri}, position: {line, character}} */
    /* Error("Can't handle completions yet") */
    /* Ok((state, Rpc.J.(l([
      o([
        ("label", s("Hello_world"))
      ])
    ])))) */
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