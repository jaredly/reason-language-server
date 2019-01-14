open SharedTypes;

open Infix;
let showConstructor = ({SharedTypes.Type.Constructor.name: {txt}, args, res}) => {
  txt ++ (args == []
    ? ""
    : "(" ++ String.concat(", ", args |. Belt.List.map(((typ, _)) => (
      typ.toString()
    ))) ++ ")")
  ++ ((res |?>> typ => "\n" ++ typ.toString()) |? "")
};

let rec pathOfModuleOpen = items =>
  switch (items) {
  | [] => Tip("place holder")
  | [one, ...rest] => Nested(one, pathOfModuleOpen(rest))
  };

/* TODO local opens */
let resolveOpens = (~env, ~previous, opens, ~getModule) =>
  List.fold_left(
    (previous, path) => {
      /** Finding an open, first trying to find it in previoulsly resolved opens */
      let rec loop = prev =>
        switch (prev) {
        | [] =>
          switch (path) {
          | Tip(_) => previous
          | Nested(name, path) =>
            switch (getModule(name)) {
            | None =>
              Log.log("Could not get module " ++ name);
              previous; /* TODO warn? */
            | Some(file) =>
              switch (
                Query.resolvePath(
                  ~env=Query.fileEnv(file),
                  ~getModule,
                  ~path,
                )
              ) {
              | None =>
                Log.log("Could not resolve in " ++ name);
                previous;
              | Some((env, _placeholder)) => previous @ [env]
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
        loop(previous);
      | Some((env, _)) =>
        Log.log("Was local");
        previous @ [env];
      };
      /* loop(previous) */
    },
    previous,
    opens,
  );

let completionForDeclareds = (~pos, declareds, prefix, transformContents) =>
  /* Log.log("complete for declares " ++ prefix); */
  Hashtbl.fold(
    (_stamp, declared, results) =>
      if (Utils.startsWith(declared.name.txt, prefix)
          && Utils.locationContainsFuzzy(declared.scopeLoc, pos)) {
        [
          {...declared, contents: transformContents(declared.contents)},
          ...results,
        ];
      } else {
        /* Log.log("Nope doesn't count " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ m); */
        results;
      },
    declareds,
    [],
  );

let completionForExporteds =
    (
      exporteds,
      stamps: Hashtbl.t(int, SharedTypes.declared('a)),
      prefix,
      transformContents,
    ) =>
  Hashtbl.fold(
    (name, stamp, results) =>
      /* Log.log("checking exported: " ++ name); */
      if (Utils.startsWith(name, prefix)) {
        let declared = Hashtbl.find(stamps, stamp);
        [
          {...declared, contents: transformContents(declared.contents)},
          ...results,
        ];
      } else {
        results;
      },
    exporteds,
    [],
  );

let completionForConstructors =
    (
      exportedTypes,
      stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)),
      prefix,
    ) => {

  Hashtbl.fold(
    (_name, stamp, results) => {
      let t = Hashtbl.find(stamps, stamp);
      switch (t.contents.kind) {
      | Variant(constructors) =>
        {
          Belt.List.keep(constructors, c =>
            Utils.startsWith(c.name.txt, prefix)
          )
          |. Belt.List.map(c => (c, t))
        }
        @ results
      | _ => results
      };
    },
    exportedTypes,
    [],
  );
    };

let completionForAttributes =
    (
      exportedTypes,
      stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)),
      prefix,
    ) =>
  Hashtbl.fold(
    (_name, stamp, results) => {
      let t = Hashtbl.find(stamps, stamp);
      switch (t.contents.kind) {
      | Record(attributes) =>
        (
          Belt.List.keep(attributes, c =>
            Utils.startsWith(c.name.txt, prefix)
          )
          |. Belt.List.map(c => (c, t))
        )
        @ results
      | _ => results
      };
    },
    exportedTypes,
    [],
  );

let isCapitalized = name =>
  if (name == "") {
    false;
  } else {
    let c = name.[0];
    switch (c) {
    | 'A'..'Z' => true
    | _ => false
    };
  };

/**
The three possibilities

lower.suffix -> `Attribute([lower, lower], suffix)

lower.Upper.suffix -> `AbsAttribute([Upper], suffix)

Upper.lower -> `Normal([Upper], lower)
*/

let rec pathFromTokenParts = items => {
  switch items {
    | [] => assert(false)
    | [one] => Tip(one)
    | [one, ...rest] => Nested(one, pathFromTokenParts(rest))
  }
};

let determineCompletion = items => {
  let rec loop = (offset, items) =>
    switch (items) {
    | [] => assert(false)
    | [one] => `Normal(Tip(one))
    | [one, two] when ! isCapitalized(one) => `Attribute(([one], two))
    | [one, two] => `Normal(Nested(one, Tip(two)))
    | [one, ...rest] =>
      if (isCapitalized(one)) {
        switch (loop(offset + String.length(one) + 1, rest)) {
        | `Normal(path) => `Normal(Nested(one, path))
        | x => x
        };
      } else {
        switch (loop(offset + String.length(one) + 1, rest)) {
        | `Normal(path) => `AbsAttribute(path)
        | `Attribute(path, suffix) => `Attribute(([one, ...path], suffix))
        | x => x
        };
      }
    };
  loop(0, items);
};

/* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
   */
let getEnvWithOpens =
    (
      ~pos,
      ~env: Query.queryEnv,
      ~getModule,
      ~opens: list(Query.queryEnv),
      path,
    ) =>
  /* let%opt declared = ; */
  /* for ppx, I think I'd like a "if this is nonnull, bail w/ it".
     So the opposite of let%opt - let%bail or something */
  /* Query.resolvePath(~env, ~path, ~getModule) */
  switch (Query.resolveFromStamps(~env, ~path, ~getModule, ~pos)) {
  | Some(x) => Some(x)
  | None =>
    let rec loop = opens =>
      switch (opens) {
      | [env, ...rest] =>
        Log.log("Looking for env in " ++ env.Query.file.uri);
        switch (Query.resolvePath(~env, ~getModule, ~path)) {
        | Some(x) => Some(x)
        | None => loop(rest)
        }
      | [] =>
        switch (path) {
        | Tip(_) => None
        | Nested(top, path) =>
          Log.log("Getting module " ++ top);
          let%opt file = getModule(top);
          Log.log("got it");
          let env = Query.fileEnv(file);
          Query.resolvePath(~env, ~getModule, ~path)
          |> Infix.logIfAbsent("Unable to resolve the path");
        }
      };
    loop(opens);
  };

type k =
  | Module(Module.kind)
  | Value(Value.t)
  | Type(Type.t)
  | ModuleType(Module.kind)
  | Constructor(Type.Constructor.t, declared(Type.t))
  | Attribute(Type.Attribute.t, declared(Type.t))
  | FileModule(string);

let kindToInt = k =>
  switch (k) {
  | Module(_) => 9
  | FileModule(_) => 9
  | ModuleType(_) => 9
  | Constructor(_, _) => 4
  | Attribute(_, _) => 5
  | Type(_) => 22
  | Value(_) => 12
  };

let detail = (name, contents) =>
  switch (contents) {
  | Type({typ}) =>
    typ.declToString(name)
  | Value({typ}) =>
    typ.toString()
  | Module(_) => "module"
  | ModuleType(_) => "module type"
  | FileModule(_) => "file module"
  | Attribute({typ}, t) =>
    name
    ++ ": "
    ++ (
      typ.toString()
    )
    ++ "\n\n"
    ++ (
      t.contents.typ.declToString(t.name.txt)
    )
  | Constructor(c, t) =>
  showConstructor(c)
    ++ "\n\n"
    ++ (
      t.contents.typ.declToString(t.name.txt)
    )
  };

let localValueCompletions = (~pos, ~env: Query.queryEnv, suffix) => {
  let results = [];
  Log.log("---------------- LOCAL VAL");
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      results
      @ completionForDeclareds(~pos, env.file.stamps.modules, suffix, m =>
          Module(m)
        )
      @ (
        /* TODO declared thingsz */
        completionForConstructors(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |. Belt.List.map(((c, t)) =>
             {...emptyDeclared(c.name.txt), contents: Constructor(c, t)}
           )
      );
    } else {
      results;
    };

  let results =
    if (suffix == "" || ! isCapitalized(suffix)) {
      results
      @ completionForDeclareds(~pos, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      @ completionForDeclareds(~pos, env.file.stamps.types, suffix, t =>
          Type(t)
        )
      @ (
        completionForAttributes(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |. Belt.List.map(((c, t)) =>
             {...emptyDeclared(c.name.txt), contents: Attribute(c, t)}
           )
      );
    } else {
      results;
    };

  results |. Belt.List.map(x => (env.file.uri, x));
};

let valueCompletions = (~env: Query.queryEnv, suffix) => {
  Log.log(" - Completing in " ++ env.file.uri);
  let results = [];
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      let moduleCompletions = completionForExporteds(
          env.exported.modules, env.file.stamps.modules, suffix, m =>
          Module(m)
        );
      /* Log.log(" -- capitalized " ++ string_of_int(Hashtbl.length(env.exported.types)) ++ " exported types"); */
      /* env.exported.types |> Hashtbl.iter((name, _) => Log.log("    > " ++ name)); */
      results
      @ moduleCompletions
      @ (
        /* TODO declared thingsz */
        completionForConstructors(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |. Belt.List.map(((c, t)) =>
             {...emptyDeclared(c.name.txt), contents: Constructor(c, t)}
           )
      );
    } else {
      results;
    };

  let results =
    if (suffix == "" || ! isCapitalized(suffix)) {
      Log.log(" -- not capitalized");
      results
      @ completionForExporteds(
          env.exported.values, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      @ completionForExporteds(
          env.exported.types, env.file.stamps.types, suffix, t =>
          Type(t)
        )
      @ (
        completionForAttributes(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |. Belt.List.map(((c, t)) =>
             {...emptyDeclared(c.name.txt), contents: Attribute(c, t)}
           )
      );
    } else {
      results;
    };

  /* Log.log("Getting value completions " ++ env.file.uri);
     Log.log(String.concat(", ", results |. Belt.List.map(x => x.name.txt))); */

  results |. Belt.List.map(x => (env.file.uri, x));
};

let attributeCompletions = (~env: Query.queryEnv, ~suffix) => {
  let results = [];
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      results
      @ completionForExporteds(
          env.exported.modules, env.file.stamps.modules, suffix, m =>
          Module(m)
        );
    } else {
      results;
    };

  let results =
    if (suffix == "" || ! isCapitalized(suffix)) {
      results
      @ completionForExporteds(
          env.exported.values, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      /* completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t)) @ */
      @ (
        completionForAttributes(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |. Belt.List.map(((c, t)) =>
             {...emptyDeclared(c.name.txt), contents: Attribute(c, t)}
           )
      );
    } else {
      results;
    };

  results |. Belt.List.map(x => (env.file.uri, x));
};

/**

TODO filter out things that are defined after the current position

*/

let resolveRawOpens = (~env, ~getModule, ~rawOpens, ~package) => {
  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));

  let opens =
    resolveOpens(
      ~env,
      ~previous=
        Belt.List.map(
          Belt.List.keepMap(packageOpens, getModule),
          Query.fileEnv,
        ),
      rawOpens
      |> List.map(Str.split(Str.regexp_string(".")))
      |> List.map(pathOfModuleOpen),
      ~getModule,
    );

  opens
};

/** This function should live somewhere else */
let findDeclaredValue =
    (
      ~full,
      ~package,
      /* the text that we found e.g. open A.B.C, this is "A.B.C" */
      ~rawOpens,
      ~getModule,
      pos,
      tokenParts,
    ) => {
  let env = Query.fileEnv(full.file);

  let opens = resolveRawOpens(~env, ~getModule, ~rawOpens, ~package);

  let path = pathFromTokenParts(tokenParts);

  let%opt (env, suffix) = getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path);

  let%opt stamp = Utils.maybeHash(env.exported.values, suffix);
  Utils.maybeHash(env.file.stamps.values, stamp);
};


let get =
    (~full, ~package, ~rawOpens, ~getModule, ~allModules, pos, tokenParts) => {
  Log.log(
    "Opens folkz > "
    ++ string_of_int(List.length(rawOpens))
    ++ " "
    ++ String.concat(" ... ", rawOpens),
  );
  let env = Query.fileEnv(full.file);

  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));

  let opens = resolveRawOpens(~env, ~getModule, ~rawOpens, ~package);
  Log.log(
    "Opens nows "
    ++ string_of_int(List.length(opens))
    ++ " "
    ++ String.concat(" ", Belt.List.map(opens, e => e.file.uri)),
  );

  switch (tokenParts) {
  | [] => []
  | [suffix] =>
    let locallyDefinedValues =
      localValueCompletions(~pos, ~env, suffix);
    let alreadyUsedIdentifiers = Hashtbl.create(10);
    let valuesFromOpens =
      Belt.List.reduce(
        opens,
        [],
        (results, env) => {
          let completionsFromThisOpen =
            valueCompletions(~env, suffix);
          Belt.List.keep(completionsFromThisOpen, ((_uri, declared)) => {
            if (! Hashtbl.mem(alreadyUsedIdentifiers, declared.name.txt)) {
              Hashtbl.add(alreadyUsedIdentifiers, declared.name.txt, true);
              true;
            } else {
              false;
            }}
          )
          @ results;
        },
      );
    /* TODO complete the namespaced name too */
    let localModuleNames =
      Belt.List.keepMap(allModules, name => {
        /* Log.log("Checking " ++ name); */

        Utils.startsWith(name, suffix) && !String.contains(name, '-') ?
          Some((
            "wait for uri",
            {...emptyDeclared(name), contents: FileModule(name)},
          )) :
          None
      }
      );
    locallyDefinedValues @ valuesFromOpens @ localModuleNames;
  | multiple =>
    open Infix;
    Log.log("Completing for " ++ String.concat("<.>", multiple));

    switch (determineCompletion(multiple)) {
    | `Normal(path) =>
      {
        Log.log("normal " ++ pathToString(path));
        let%opt_wrap (env, suffix) =
          getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path);
        Log.log("Got the env");
        valueCompletions(~env, suffix);
      }
      |? []
    | `Attribute(target, suffix) =>
      {
        Log.log("suffix :" ++ suffix);
        switch (target) {
        | [] => None
        | [first, ...rest] =>
          Log.log("-------------- Looking for " ++ first);
          let%opt declared =
            Query.findInScope(pos, first, env.file.stamps.values);
          Log.log("Found it! " ++ declared.name.txt);
          let%opt (path, _args) = declared.contents.typ.getConstructorPath();
          let%opt (env, typ) =
            Hover.digConstructor(~env, ~getModule, path);
          let%opt (env, typ) =
            Belt.List.reduce(
              rest,
              Some((env, typ)),
              (current, name) => {
                let%opt (env, typ) = current;
                switch (typ.contents.kind) {
                | Record(attributes) =>
                  let%opt attr =
                    attributes |. Belt.List.getBy(a => a.name.txt == name);
                  Log.log("Found attr " ++ name);
                  let%opt (path, _args) = attr.typ.getConstructorPath();
                  Hover.digConstructor(~env, ~getModule, path);
                | _ => None
                };
              },
            );
          switch (typ.contents.kind) {
          | Record(attributes) =>
            Some(
              attributes
              |. Belt.List.keepMap(a =>
                   if (Utils.startsWith(a.name.txt, suffix)) {
                     Some((
                       env.file.uri,
                       {
                         ...emptyDeclared(a.name.txt),
                         contents: Attribute(a, typ),
                       },
                     ));
                   } else {
                     None;
                   }
                 ),
            )
          | _ => None
          };
        };
      }
      |? []
    | `AbsAttribute(path) =>
      {
        let%opt_wrap (env, suffix) =
          getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path);

        attributeCompletions(~env, ~suffix)
        @ List.concat(
            Belt.List.map(opens, env =>
              attributeCompletions(~env, ~suffix)
            ),
          );
      }
      |? []
    };
  };
};