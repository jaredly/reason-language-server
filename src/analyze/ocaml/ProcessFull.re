
open SharedTypes;
open Belt.Result;

let tryReadCmi = cmi =>
  switch (Cmi_format.read_cmi(cmi)) {
  | exception _ => Error("Invalid cmi format - probably wrong ocaml version")
  | x => Ok(x)
  };

let tryReadCmt = cmt =>
  switch (Cmt_format.read_cmt(cmt)) {
  | exception _ => Error("Invalid cmt format - probably wrong ocaml version")
  | x => Ok(x)
  };

let fileForCmi = (cmi, uri, processDoc) => {
  let%opt infos = tryReadCmi(cmi) |> Result.toOptionAndLog;
  ProcessCmt.forCmi(uri, processDoc, infos);
};

let fileForCmt = (cmt, uri, processDoc) => {
  let%try infos = tryReadCmt(cmt);
  ProcessCmt.forCmt(uri, processDoc, infos)
};

let fullForCmt = (cmt, uri, processDoc) => {
  let%try infos = tryReadCmt(cmt);
  let%try file = ProcessCmt.forCmt(uri, processDoc, infos);
  let%try_wrap extra = ProcessExtra.forCmt(~file, infos);
  {file, extra}
};