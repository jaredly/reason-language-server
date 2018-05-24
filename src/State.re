
type state = {
  rootPath: string,
  localCompiledBase: string,
  localModules: list((string, (string, string))),
  localCompiledMap: list((string, string)),
  dependencyModules: list((FindFiles.modpath, (string, string))),
  cmtMap: Hashtbl.t(string, Cmt_format.cmt_infos),
  docs: Hashtbl.t(string, (option(string), list(Docs.full))),
  documentText: Hashtbl.t(string, (string, int, bool)),
  /* workspace folders... */
};

let getCmt = (cmt, state) => {
  if (Hashtbl.mem(state.cmtMap, cmt)) {
    Hashtbl.find(state.cmtMap, cmt)
  } else {
    let infos = Cmt_format.read_cmt(cmt);
    Hashtbl.replace(state.cmtMap, cmt, infos);
    infos
  }
};

let isMl = path => Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".mli");

let odocToMd = text => {
  let top = MarkdownOfOCamldoc.convert(0, text);
  Omd.to_markdown(top)
};

let docConverter = src => isMl(src) ? odocToMd : (x => x);

/* TODO track stale docs */
let getDocs = (modname, state) => {
  open Infix;
  if (Hashtbl.mem(state.docs, modname)) {
    Result.Ok(Hashtbl.find(state.docs, modname))
  } else {
    let docs = switch (List.assoc(modname, state.localModules)) {
    | (cmt, src) => Docs.forCmt(docConverter(src), getCmt(cmt, state))
    | exception Not_found => switch (List.assoc(FindFiles.Plain(modname), state.dependencyModules)) {
      | (cmt, src) => Docs.forCmt(docConverter(src), getCmt(cmt, state))
      | exception Not_found => None
      }
    };
    docs |?< d => Hashtbl.replace(state.docs, modname, d);
    docs |> Result.orError("Unable to read cmt")
  }
};