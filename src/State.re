type state = {
  rootPath: string,
  localCompiledBase: string,
  localModules: list((string, (string, string))),
  localCompiledMap: list((string, string)),
  includeDirectories: list(string),
  dependencyModules: list((FindFiles.modpath, (string, string))),
  cmtCache:
    Hashtbl.t(
      string,
      (
        float, /* modified time */
        Cmt_format.cmt_infos,
        (option(string), list(Docs.full))
      )
    ),
  pathsForModule: Hashtbl.t(string, (string, string)),
  /* cmtMap: Hashtbl.t(string, Cmt_format.cmt_infos), */
  /* docs: Hashtbl.t(string, (option(string), list(Docs.full))), */
  documentText: Hashtbl.t(string, (string, int, bool)),
  compiledDocuments: Hashtbl.t(string, AsYouType.result)
  /* workspace folders... */
};

/* let getCmt = (cmt, state) => {
     if (Hashtbl.mem(state.cmtMap, cmt)) {
       Hashtbl.find(state.cmtMap, cmt)
     } else {
       let infos = Cmt_format.read_cmt(cmt);
       Hashtbl.replace(state.cmtMap, cmt, infos);
       infos
     }
   }; */
let isMl = path =>
  Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".mli");

let odocToMd = text => {
  let top = MarkdownOfOCamldoc.convert(0, text);
  Omd.to_markdown(top);
};

let docConverter = src => isMl(src) ? odocToMd : (x => x);

let newDocs = (cmtCache, changed, cmt, src) => {
  let infos = Cmt_format.read_cmt(cmt);
  switch (Docs.forCmt(docConverter(src), infos)) {
  | None => None
  | Some(docs) =>
    Hashtbl.replace(cmtCache, cmt, (changed, infos, docs));
    Some(docs);
  };
};

let hasProcessedCmt = (state, cmt) => Hashtbl.mem(state.cmtCache, cmt);

let docsForCmt = (cmt, src, state) =>
  if (Hashtbl.mem(state.cmtCache, cmt)) {
    let (mtime, infos, docs) = Hashtbl.find(state.cmtCache, cmt);
    /* TODO I should really throttle this mtime checking to like every 50 ms or so */
    switch (Files.getMtime(cmt)) {
    | None => None
    | Some(changed) =>
      if (changed > mtime) {
        newDocs(state.cmtCache, changed, cmt, src);
      } else {
        Some(docs);
      }
    };
  } else {
    switch (Files.getMtime(cmt)) {
    | None => None
    | Some(changed) => newDocs(state.cmtCache, changed, cmt, src)
    };
  };

let updateContents = (uri, text, version, state) => {
  Hashtbl.remove(state.compiledDocuments, uri);
  Hashtbl.replace(state.documentText, uri, (text, int_of_float(version), false));
  state
};

let getCompilationResult = (uri, state) => {
  if (Hashtbl.mem(state.compiledDocuments, uri)) {
    Hashtbl.find(state.compiledDocuments, uri)
  } else {
    let (text, _, _) = Hashtbl.find(state.documentText, uri);
    let result = AsYouType.process(text, state.rootPath, state.includeDirectories);
    Hashtbl.replace(state.compiledDocuments, uri, result);
    result
  }
};

let docsForModule = (modname, state) =>
  Infix.(
    if (Hashtbl.mem(state.pathsForModule, modname)) {
      let (cmt, src) = Hashtbl.find(state.pathsForModule, modname);
      docsForCmt(cmt, src, state);
    } else {
      None;
    }
  );