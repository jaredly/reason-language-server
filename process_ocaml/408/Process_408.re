
open SharedTypes;
open Belt.Result;

let fileForCmi = (~moduleName, cmi, uri, processDoc) => {
  let%opt infos = Shared.tryReadCmi(cmi) |> RResult.toOptionAndLog;
  ProcessCmt.forCmi(~moduleName, uri, processDoc, infos);
};

let fileForCmt = (~moduleName, cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  ProcessCmt.forCmt(~moduleName, uri, processDoc, infos)
};

let fullForCmt = (~moduleName, ~allLocations, cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  let%try file = ProcessCmt.forCmt(~moduleName, uri, processDoc, infos);
  let%try_wrap extra = ProcessExtra.forCmt(~file, ~allLocations, infos);
  {file, extra}
};

/* let sourceForCmt = cmt => {
  let%try infos = Shared.tryReadCmt(cmt);
  switch (infos.cmt_annots) {
  | Implementation(structure) => {
    Pprintast.structure(Stdlib.Format.str_formatter, Untypeast.untype_structure(structure));
    Ok(Format.flush_str_formatter());
  }
  | Interface(signature) =>
    Pprintast.signature(Stdlib.Format.str_formatter, Untypeast.untype_signature(signature));
    Ok(Format.flush_str_formatter());
  | _ => Error("Cannot show ppxed source for files with type errors at the moment")
  }
}; */

let astForCmt = cmt => {
  let%try infos = Shared.tryReadCmt(cmt);
  switch (infos.cmt_annots) {
  | Implementation(structure) => {
    Ok(`Implementation(Untypeast.untype_structure(structure)))
    /* Printast.implementation(Stdlib.Format.str_formatter, );
    Ok(Format.flush_str_formatter()); */
  }
  | Interface(signature) =>
    Ok(`Interface(Untypeast.untype_signature(signature)))
    /* Printast.interface(Stdlib.Format.str_formatter, Untypeast.untype_signature(signature));
    Ok(Format.flush_str_formatter()); */
  | _ => Error("Cannot show ppxed source for files with type errors at the moment")
  }
};

module PrintType = PrintType
