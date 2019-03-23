
let hashList = tbl => Hashtbl.fold((key, value, result) => [(key, value), ...result], tbl, []);


let versionModuleName = version => "Version" ++ string_of_int(version);