
let hashList = tbl => Hashtbl.fold((key, value, result) => [(key, value), ...result], tbl, []);

let makeModule = (moduleName, contents) =>
  Ast_helper.Str.module_(
    Ast_helper.Mb.mk(Location.mknoloc(moduleName), Ast_helper.Mod.mk(Parsetree.Pmod_structure(contents))),
  );

let versionModuleName = version => "Version" ++ string_of_int(version);
let typesModuleName = version => "Types" ++ string_of_int(version);