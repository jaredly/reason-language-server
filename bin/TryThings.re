open Infix;
let refmt = "." /+ "node_modules" /+ "bs-platform" /+ "lib" /+ "refmt.exe";

AsYouType.runRefmt(~cacheLocation=".", "let x = 10", refmt)