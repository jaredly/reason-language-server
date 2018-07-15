open Infix;

let _cache = Belt_HashMapString.make(5);

let cacheKey = (root, name) => root ++ name;

let rec resolveNodeModulePath = (~startPath=".", name) =>
  switch (Belt_HashMapString.get(_cache, cacheKey(startPath, name))) {
  | Some(path) => Result.Ok(path)
  | None =>
    let path = startPath /+ "node_modules" /+ name;
    switch (startPath) {
    | "/" =>
      Files.exists(path) ?
        {
          Belt_HashMapString.set(_cache, cacheKey(startPath, name), path);
          Result.Ok(path);
        } :
        Result.Error("module '" ++ name ++ "' not found")
    | _ =>
      Files.exists(path) ?
        {
          Belt_HashMapString.set(_cache, cacheKey(startPath, name), path);
          Result.Ok(path);
        } :
        resolveNodeModulePath(~startPath=Filename.dirname(startPath), name)
    };
  };