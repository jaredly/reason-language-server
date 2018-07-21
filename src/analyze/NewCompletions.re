
open SharedTypes;

let rec pathOfModuleOpen = items => switch items {
  | [] => Tip("place holder")
  | [one, ...rest] => Nested(one, pathOfModuleOpen(rest))
};

/* TODO local opens */
let resolveOpens = (~env, opens, ~getModule) => {
  List.fold_left((previous, path) => {
    let rec loop = prev => switch prev {
    | [] =>
      switch (path) {
        | Tip(_) => previous
        | Nested(name, path) => 
          switch (getModule(name)) {
          | None => {
            Log.log("Could not get module " ++ name);
            previous /* TODO warn? */
          }
          | Some(file) => {
              switch (Query.resolvePath(~env=Query.fileEnv(file), ~getModule, ~path)) {
                | None => {
                  Log.log("Could not resolve in " ++ name);
                  previous
                }
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
    Log.log("resolving open " ++ pathToString(path));
    switch (Query.resolvePath(~env, ~getModule, ~path)) {
      | None =>
      Log.log("Not local");
       loop(previous)
      | Some((env, _)) =>
      Log.log("Was local");
      previous @ [env]
    }
    /* loop(previous) */
  }, [], opens);
};

let completionForDeclareds = (declareds, prefix, transformContents) => {
  Hashtbl.fold((_stamp, declared, results) => {
    if (Utils.startsWith(declared.name.txt, prefix)) {
      [{...declared, contents: transformContents(declared.contents)}, ...results]
    } else {
      results
    }
  }, declareds, [])
};

let completionForExporteds = (exporteds, stamps: Hashtbl.t(int, SharedTypes.declared('a)), prefix, transformContents) => {
  Hashtbl.fold((name, stamp, results) => {
    if (Utils.startsWith(name, prefix)) {
      let declared = Hashtbl.find(stamps, stamp);
      [{...declared, contents: transformContents(declared.contents)}, ...results]
    } else {
      results
    }
  }, exporteds, [])
};

let completionForConstructors = (exportedTypes, stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)), prefix) => {
  Hashtbl.fold((_name, stamp, results) => {
    let t = Hashtbl.find(stamps, stamp);
    switch (t.contents.kind) {
    | Variant(constructors) => 
    (
      Belt.List.keep(constructors, c => Utils.startsWith(c.name.txt, prefix))
      |. Belt.List.map(c => (c, t))
    ) @ results
    | _ => results
    }
  }, exportedTypes, [])
};

let completionForAttributes = (exportedTypes, stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)), prefix) => {
  Hashtbl.fold((_name, stamp, results) => {
    switch (Hashtbl.find(stamps, stamp).contents.kind) {
    | Record(attributes) => Belt.List.keep(attributes, c => Utils.startsWith(c.name.txt, prefix)) @ results
    | _ => results
    }
  }, exportedTypes, [])
};

let isCapitalized = name => {
  if (name == "") {
    false
  } else {
  let c = name.[0];
  switch (c) {
    |'A'..'Z' => true
    |_ => false
  };
  }
};

/**
The three possibilities

lower.suffix -> `Attribute([lower, lower], suffix)

lower.Upper.suffix -> `AbsAttribute([Upper], suffix)

Upper.lower -> `Normal([Upper], lower)
*/

let determineCompletion = items => {
  let rec loop = (offset, items) => switch items {
  | [] => assert(false)
  | [one] => `Normal(Tip(one))
  | [one, two] when !isCapitalized(one) => `Attribute([one], two)
  | [one, two] => `Normal(Nested(one, Tip(two)))
  | [one, ...rest] => if (isCapitalized(one)) {
    switch (loop(offset + String.length(one) + 1, rest)) {
    | `Normal(path) => `Normal(Nested(one, path))
    | x => x
    }
  } else {
    switch (loop(offset + String.length(one) + 1, rest)) {
    | `Normal(path) => `AbsAttribute(path)
    | `Attribute(path, suffix) => `Attribute([one, ...path], suffix)
    | x => x
    }
  }
  };
  loop(0, items)
};

/* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
   */
let getEnvWithOpens = (~env: Query.queryEnv, ~getModule, ~opens: list(Query.queryEnv), path) => {
  /* TODO do "resolve from stamps" */
  /* for ppx, I think I'd like a "if this is nonnull, bail w/ it".
     So the opposite of let%opt - let%bail or something */
  switch (Query.resolvePath(~env, ~path, ~getModule)) {
  | Some(x) =>
    Some(x)
  | None =>
    let rec loop = opens => switch opens {
      | [] => switch path {
        | Tip(_) => None
        | Nested(top, path) => {
          let%opt file = getModule(top);
          let env = Query.fileEnv(file);
          Query.resolvePath(~env, ~getModule, ~path) |> Infix.logIfAbsent("Unable to resolve the path")
        }
      }
      | [env, ...rest] => switch (Query.resolvePath(~env, ~getModule, ~path)) {
        | Some(x) => Some(x)
        | None => loop(rest)
        }
    };
    loop(opens)
  }
};

type k =
| Module(Module.kind)
| Value(Value.t)
| Type(Type.t)
| ModuleType(Module.kind)
| Constructor(Type.Constructor.t, declared(Type.t))
| Attribute(Type.Attribute.t)
| FileModule(string)
;

let kindToInt = k =>
  switch k {
  | Module(_) => 9
  | FileModule(_) => 9
  | ModuleType(_) => 9
  | Constructor(_, _) => 4
  | Attribute(_) => 5
  | Type(_) => 22
  | Value(_) => 12
  };

let detail = (name, contents) => switch contents {
  | Type({typ}) => 
      PrintType.default.decl(PrintType.default, name, name, typ)
      |> PrintType.prettyString
  | Value({typ}) =>
      PrintType.default.value(PrintType.default, name, name, typ)
      |> PrintType.prettyString
  | Module(m) => "module"
  | ModuleType(m) => "module type"
  | FileModule(m) => "file module"
  | Attribute({typ}) => PrintType.default.expr(PrintType.default, typ) |> PrintType.prettyString
  | Constructor(c, t) =>
    SharedTypes.Type.Constructor.show(c)
    ++ "\n" ++
    (PrintType.default.decl(PrintType.default, t.name.txt, t.name.txt, t.contents.typ)
    |> PrintType.prettyString)

};

let valueCompletions = (~env: Query.queryEnv, ~getModule, ~suffix) => {
  let results = [];
  let results = if (suffix == "" || isCapitalized(suffix)) {
    results @ completionForExporteds(env.exported.modules, env.file.stamps.modules, suffix, m => Module(m))
    @ (
      /* TODO declared thingsz */
      completionForConstructors(env.exported.types, env.file.stamps.types, suffix)
      |. Belt.List.map(((c, t)) => {...emptyDeclared(c.name.txt), contents: Constructor(c, t)})
    )
  } else {
    results
  };

  let results = if (suffix == "" || !isCapitalized(suffix)) {
    results @ completionForExporteds(env.exported.values, env.file.stamps.values, suffix, v => Value(v)) @
    completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t))
  } else {
    results
  };

  Log.log("Getting value completions " ++ env.file.uri);
  Log.log(String.concat(", ", results |. Belt.List.map(x => x.name.txt)));

  results |. Belt.List.map(x => (env.file.uri, x));
};

let attributeCompletions = (~env: Query.queryEnv, ~getModule, ~suffix) => {
  let results = [];
  let results = if (suffix == "" || isCapitalized(suffix)) {
    results @ completionForExporteds(env.exported.modules, env.file.stamps.modules, suffix, m => Module(m))
  } else {
    results
  };

  let results = if (suffix == "" || !isCapitalized(suffix)) {
    results @ completionForExporteds(env.exported.values, env.file.stamps.values, suffix, v => Value(v)) @
    /* completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t)) */
    (
      completionForAttributes(env.exported.types, env.file.stamps.types, suffix)
      |. Belt.List.map(c => {...emptyDeclared(c.name.txt), contents: Attribute(c)})
    )
  } else {
    results
  };

  results |. Belt.List.map(x => (env.file.uri, x));
};

/**

TODO filter out things that are defined after the current position

 */

let get = (
  ~full,
  ~package,
  ~opens,
  ~getModule,
  ~allModules,
  pos,
  tokenParts,
) => {
  Log.log("Opens folkz > " ++ string_of_int(List.length(opens)) ++ " " ++ String.concat(" ... ", opens));
  let env = Query.fileEnv(full.file);
  let opens = resolveOpens(~env, opens |> List.map(Str.split(Str.regexp_string("."))) |> List.map(pathOfModuleOpen), ~getModule);
  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));
  let opens = Belt.List.map(Belt.List.keepMap(packageOpens, getModule), Query.fileEnv) @ opens;
  Log.log("Opens nows " ++ string_of_int(List.length(opens)) ++ " " ++ String.concat(" ", Belt.List.map(opens, e => e.file.uri)));

  switch tokenParts {
    | [] => []
    | [""] =>
        valueCompletions(~env, ~getModule, ~suffix="")
        @ List.concat(Belt.List.map(opens, env => valueCompletions(~env, ~getModule, ~suffix="")))
        @ Belt.List.map(allModules, name => ("um wait for uri", {...emptyDeclared(name), contents: FileModule(name)}))
    | [suffix] =>
      let one = valueCompletions(~env, ~getModule, ~suffix) ;
      let two = List.concat(
          Belt.List.map(opens, env => valueCompletions(~env, ~getModule, ~suffix))
        );
      let three = Belt.List.keepMap(allModules, name => Utils.startsWith(name, suffix) ? Some(("wait for uri", {...emptyDeclared(name), contents: FileModule(name)})) : None);
      one @ two @ three
    | multiple => {
      open Infix;
      let env = Query.fileEnv(full.file);

      /* Log.log(SharedTypes.showExtra(full.extra)); */

      Log.log("multiepl");
      switch (determineCompletion(multiple)) {
      | `Normal(path) => {
          let%opt_wrap (env, suffix) = getEnvWithOpens(~env, ~getModule, ~opens, path);
          valueCompletions(~env, ~getModule, ~suffix) @ List.concat(
            Belt.List.map(opens, env => valueCompletions(~env, ~getModule, ~suffix))
          )
        } |? {
          [];
        }
      | `Attribute(target, suffix) => {
        Log.log("ok attribute");

        switch (target) {
          | [] => None
          | [first, ...rest] => {
            let%opt declared = Query.findInScope(pos, env.file.stamps.values);
            switch (ProcessExtra.dig(declared.contents.typ).desc) {
              | Tconstr(path, _args, _memo) => {
                let%opt typ = switch (Query.resolveFromCompilerPath(~env, ~getModule, path)) {
                | `Not_found => None
                | `Stamp(stamp) =>
                  Query.hashFind(env.file.stamps.types, stamp)
                | `Exported(env, name) =>
                  let%opt stamp = Query.hashFind(env.exported.types, name);
                  Query.hashFind(env.file.stamps.types, stamp)
                | _ => None
                };
                switch (typ.contents.kind) {
                | Record(attributes) =>
                  Some(attributes |. Belt.List.keepMap(a => {
                    if (Utils.startsWith(a.name.txt, suffix)) {
                      Some((env.file.uri, {
                        ...emptyDeclared(a.name.txt),
                        contents: Attribute(a)
                      }))
                    } else {
                      None
                    }
                  }))
                | _ => None
                }
              }
              | _ => None
            };
          }
        };



        /* let (l, c) = pos;
        let pos = (l - 1, c + offset);
        {

          Log.log("Ok attribute folz " ++ string_of_int(offset) ++ " suffix " ++ suffix);
          let%opt (_, loc) = References.locForPos(~extra=full.extra, pos);
          Log.log("got a loc");

          switch (loc) {
          | Typed({Types.desc: Tconstr(path, _, _)}, _) =>

            switch (ProcessExtra.getTypeAtPath(~env, path)) {
              | `Local({stamp, contents: {kind: Record(attributes)}}) => {
                Log.log("Got the loc at that pos");
                Some(attributes |. Belt.List.keepMap(a => {
                  if (Utils.startsWith(a.name.txt, suffix)) {
                    Some((env.file.uri, {
                      ...emptyDeclared(a.name.txt),
                      contents: Attribute(a)
                    }))
                  } else {
                    None
                  }
                }))
              }
              | `Global(moduleName, path) =>
                Log.log("its global");
                None
              | _ => {
                Log.log("Wrong kind of loc");
                None
              }
            };

            /* [("no uri", {...emptyDeclared(
              "Typed attribute completions not supported yet"
            ), contents: FileModule("Just kidding")})] */

          | _ => {
            Log.log("welp, loc not typed");
            None
          }
          }


        } |? []; */

      } |? []
      | `AbsAttribute(path) => {
          let%opt_wrap (env, suffix) = getEnvWithOpens(~env, ~getModule, ~opens, path);

          attributeCompletions(~env, ~getModule, ~suffix) @ List.concat(
            Belt.List.map(opens, env => attributeCompletions(~env, ~getModule, ~suffix))
          )
        } |? [];
      }
    }
  }
};
