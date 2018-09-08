
let fileForCmi: (string, string, string => string) => option(SharedTypes.file);
let fileForCmt: (string, string, string => string) => result(SharedTypes.file, string);
let fullForCmt: (string, string, string => string) => result(SharedTypes.full, string);
