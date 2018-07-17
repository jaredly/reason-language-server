open Infix;

let rec resolveNodeModulePath = (~startPath=".", name) => {
  let path = startPath /+ "node_modules" /+ name;
  switch (startPath) {
  | "/" =>
    Files.exists(path) ?
      Result.Ok(path) : Result.Error("module '" ++ name ++ "' not found")
  | _ =>
    Files.exists(path) ?
      Result.Ok(path) :
      resolveNodeModulePath(~startPath=Filename.dirname(startPath), name)
  };
};