
open Compiler_libs_406;
open SharedTypes;
open Belt.Result;

let fileForCmi = (cmi, uri, processDoc) => {
  let%opt infos = Shared.tryReadCmi(cmi) |> Result.toOptionAndLog;
  ProcessCmt.forCmi(uri, processDoc, infos);
};

let fileForCmt = (cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  ProcessCmt.forCmt(uri, processDoc, infos)
};

let fullForCmt = (cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  let%try file = ProcessCmt.forCmt(uri, processDoc, infos);
  let%try_wrap extra = ProcessExtra.forCmt(~file, infos);
  {file, extra}
};

module PrintType = PrintType