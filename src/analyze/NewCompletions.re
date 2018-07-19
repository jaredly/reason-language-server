
open SharedTypes;

let rec pathOfModuleOpen = items => switch items {
  | [] => Tip("place holder")
  | [one, ...rest] => Nested(one, pathOfModuleOpen(rest))
};

/* TODO local opens */
let resolveOpens = (opens, ~getModule) => {
  List.fold_left((previous, path) => {
    let rec loop = prev => switch prev {
    | [] =>
      switch (path) {
        | Tip(_) => previous
        | Nested(name, inner) => 
          switch (getModule(name)) {
          | None => previous /* TODO warn? */
          | Some(file) => {
              switch (Query.resolvePath(~env=Query.fileEnv(file), ~getModule, ~path)) {
                | None => previous
                | Some((env, _placeholder)) => previous @ [env]
              }
            }
          }
      }
    | [env, ...rest] =>
      switch (Query.resolvePath(~env, ~getModule, ~path)) {
      | None => loop(rest)
      | Some((env, _placeholder)) => previous @ [env]
      }
    };
    loop(previous)
  }, [], opens);
};

let completionForDeclareds = (declareds, prefix) => {
  Hashtbl.fold((_stamp, declared, results) => {
    if (Utils.startsWith(declared.name.txt, prefix)) {
      [{...declared, contents: ()}, ...results]
    } else {
      results
    }
  }, declareds, [])
};

/* let completionForConstructors = (declareds, prefix) => {
  Hashtbl.fold((_stamp, declared, results) => {
    switch (declared.contents.kind) {
      | Variant(constructors) => Belt.List.keep(constructors, c => Utils.startsWith(c.name.txt, prefix)) @ results
      | _ => results
    }
  }, declareds, [])
}; */

let isCapitalized = name => {
  /* switch (name.[0]) {
    |'A'..'Z' => true
    |_ => false
  }; */
};

/* let completionsForStamps = (~stamps, prefix) => {
  if (prefix == "") {

  } else {
  }
}; */







let get = (
  ~full,
  ~package,
  ~opens,
  ~getModule,
  ~allModules,
  pos,
  tokenParts,
) => {
  let opens = resolveOpens(opens |> List.map(Str.split(Str.regexp_string("."))) |> List.map(pathOfModuleOpen), ~getModule);
  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  let opens = Belt.List.map(Belt.List.keepMap(packageOpens, getModule), Query.fileEnv) @ opens;

  /* switch tokenParts {
    | [] => []
    | [""] => /* TODO do type-driven stuffs */ []
    | [single] => 1
  } */
};
