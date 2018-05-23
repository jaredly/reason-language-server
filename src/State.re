
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
