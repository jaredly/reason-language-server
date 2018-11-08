
open Compiler_libs_402;

let fileForCmi: (~moduleName: string, string, string, string => string) => option(SharedTypes.file);
let fileForCmt: (~moduleName: string, string, string, string => string) => result(SharedTypes.file, string);
let fullForCmt: (~moduleName: string, ~allLocations: bool, string, string, string => string) => result(SharedTypes.full, string);
