open Infix;

let rec resolveNodeModulePath = (~startPath=".", name) => {
  let path = startPath /+ "node_modules" /+ name;
  switch (startPath) {
  | "/" => if (Files.exists(path)) {
      Some(path);
    } else {
      switch (Sys.getenv("NODE_PATH")) {
      | node_path =>
        node_path
        |> Str.split(Str.regexp(":"))
        |> List.map((path) => Filename.concat(path, name))
        |> List.find_opt((path) => Files.exists(path));
      | exception Not_found => None
      };
    };
  | _ =>
    Files.exists(path) ?
      Some(path) :
      resolveNodeModulePath(~startPath=Filename.dirname(startPath), name)
  };
};
