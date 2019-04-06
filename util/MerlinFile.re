open Infix;

/** This is a dirty hack to get around the bug in bsb native that doesn't do the proper ppx flags for ppxs */
let fixPpx = (flg, base) => {
  switch (Str.split(Str.regexp_string(" "), flg)) {
    | ["-ppx", ppx] when Str.string_match(Str.regexp("[a-zA-Z_]+"), ppx, 0) && !Str.string_match(Str.regexp("[a-zA-Z_]:"), ppx, 0) => {
      "-ppx " ++ (base /+ "lib" /+ "bs" /+ "native" /+ String.lowercase_ascii(ppx) ++ ".native")
    }
    | _ => flg
  }
};

let parseMerlin = (base, text) => {
  let lines = Str.split(Str.regexp_string("\n"), text);
  List.fold_left(
    ((source, build, flags), line) => {
      if (Utils.startsWith(line, "FLG ")) {
        (source, build, [fixPpx(Utils.chopPrefix(line, "FLG "), base), ...flags])
      } else if (Utils.startsWith(line, "S ")) {
        ([Utils.chopPrefix(line, "S "), ...source], build, flags)
      } else if (Utils.startsWith(line, "B ")) {
        (source, [Utils.chopPrefix(line, "B "), ...build], flags)
      } else {
        (source, build, flags)
      }
    },
    ([], [], []),
    lines
  )
};

// let maybeConcat = (base, path) => path.[0] == '/' ? path : Filename.concat(base, path);

let isRelativePath = Sys.os_type == "Win32"
? path => !Str.string_match(Str.regexp("[A-Z]:"), path, 0)
: path => path != "" && path.[0] != '/';

let isBuildFile = name =>
  Stdlib.Filename.check_suffix(name, ".cmt")
  || Stdlib.Filename.check_suffix(name, ".cmti")
  || Stdlib.Filename.check_suffix(name, ".cmi");

let isSourceFile = name =>
  Stdlib.Filename.check_suffix(name, ".re")
  || Stdlib.Filename.check_suffix(name, ".rei")
  || Stdlib.Filename.check_suffix(name, ".ml")
  || Stdlib.Filename.check_suffix(name, ".mli")
  || Stdlib.Filename.check_suffix(name, ".rel")
  || Stdlib.Filename.check_suffix(name, ".reli");

let rec tryReduce: 'a 'b 'e . (list('a), 'b, ('b, 'a) => result('b, 'e)) => result('b, 'e) = (items, current, fn) => {
  switch items {
    | [] => Ok(current)
    | [one, ...rest] => switch (fn(current, one)) {
      | Error(e) => Error(e)
      | Ok(v) => tryReduce(rest, v, fn)
    }
  }
};

let maybeHash = (h, k) => switch (Hashtbl.find(h, k)) {
  | exception Not_found => None
  | x => Some(x)
};

let (|??) = (a, b) => switch a {
  | None => b
  | Some(m) => Some(m)
};

let orLog = v => switch v {
  | Error(m) => 
  Log.log("Error: " ++ m);
  None
  | Ok(m) => Some(m)
};

type files = {src: option(string), srci: option(string), cmi: option(string), cmt: option(string), cmti: option(string)};

let orBlank = v => switch v {
  | None => "()"
  | Some(m) => m
};

let showFiles = ({src, srci, cmi, cmt, cmti}) =>
  Printf.sprintf("src: %s, srci: %s, cmi: %s, cmt: %s, cmti: %s",
  src->orBlank,
  srci->orBlank,
  cmi->orBlank,
  cmt->orBlank,
  cmti->orBlank
  );

let calcPaths = (mname, files) => {
  switch (files) {
  | {src: None, srci: None, cmt: Some(cmt), cmti: Some(cmti)} =>
    Ok(`IntfAndImpl((cmti, None, cmt, None)))
  | {src: None, srci: None, cmt: None, cmti: Some(cmti)} => Ok(`Intf((cmti, None)))
  | {src: None, srci: None, cmt: Some(cmt), cmti: None} => Ok(`Impl((cmt, None)))
  | {src: None, srci: None, cmi: Some(cmi)} => Ok(`Intf((cmi, None)))
  | {src: None, srci: None} =>
    Error("No source files for module " ++ mname ++ " - " ++ showFiles(files))
  | {src: Some(src), srci: Some(srci), cmt: Some(cmt), cmti: Some(cmti)} =>
    Ok(`IntfAndImpl((cmti, Some(srci), cmt, Some(src))))
  | {src: Some(src), srci: Some(srci), cmi: Some(cmi), cmt: Some(cmt), cmti: None} =>
    Ok(`IntfAndImpl((cmi, Some(srci), cmt, Some(src))))
  | {src: Some(src), srci: Some(srci), cmi: Some(cmi)} =>
    Ok(`IntfAndImpl((cmi, Some(srci), cmi, Some(src))))
  | {src: Some(src), srci: None, cmt: Some(cmt)} => Ok(`Impl((cmt, Some(src))))
  | {src: Some(src), srci: None, cmi: Some(cmi)} => Ok(`Impl((cmi, Some(src))))
  | {src: None, srci: Some(srci), cmti: Some(cmti)} => Ok(`Intf((cmti, Some(srci))))
  | {src: None, srci: Some(srci), cmi: Some(cmi)} => Ok(`Intf((cmi, Some(srci))))
  | _ =>
    Error("Insufficient build files found for module " ++ mname ++ " - " ++ showFiles(files))
  };
};

/** Returns a `pathsForModule`, `nameForPath`, `localModules` and `dependencyModules` */
let getModulesFromMerlin = (base, text) => {
  Log.log("start");
  let (source, build, _flags) = parseMerlin(base, text);

  let (localSource, depSource) = source->Belt.List.partition(isRelativePath);

  Log.log(Printf.sprintf("Local %d, Deps %d\n", List.length(localSource), List.length(depSource)));

  // let buildByBasename = Hashtbl.create(List.length(source) * 5);

  let cmiByModuleName = Hashtbl.create(100);
  let cmtByModuleName = Hashtbl.create(100);
  let cmtiByModuleName = Hashtbl.create(100);

  let addAndCheck = (tbl, k, v) => {
    if (Hashtbl.mem(tbl, k)) {
      Log.log("DUPLICATE " ++ k ++ " : new value " ++ v ++ " : old value " ++ Hashtbl.find(tbl, k))
    };
    Hashtbl.replace(tbl, k, v)
  };

  build->Belt.List.forEach(buildDir => {
    let buildDir = maybeConcat(base, buildDir);
    Log.log("## Build dir " ++ buildDir);
    Files.readDirectory(buildDir)
    ->Belt.List.keep(isBuildFile)
    ->Belt.List.forEach(name => {
        let full = fileConcat(buildDir, name);
        Log.log("Build file " ++ full);
        let moduleName = name->String.capitalize_ascii->Filename.chop_extension;
        if (Filename.check_suffix(name, ".cmi")) {
          cmiByModuleName->addAndCheck(moduleName, full)
        } else if (Filename.check_suffix(name, ".cmt")) {
          cmtByModuleName->addAndCheck(moduleName, full)
        } else if (Filename.check_suffix(name, ".cmti")) {
          cmtiByModuleName->addAndCheck(moduleName, full)
        };
        // if (buildByBasename->Hashtbl.mem(name)) {
        //   Log.log(
        //     "DUPLICATE "
        //     ++ name
        //     ++ " : "
        //     ++ full
        //     ++ " > prev "
        //     ++ Hashtbl.find(buildByBasename, name),
        //   );
        // };
        // buildByBasename->Hashtbl.replace(name, full);
      })
  }
  );

  let pathsForModule = Hashtbl.create(30);
  let nameForPath = Hashtbl.create(30);

  let add = (name, paths) => switch paths {
    | `Intf(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
    | `Impl(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
    | `IntfAndImpl(_, intf, _, impl) =>
        intf |?< path => Hashtbl.replace(nameForPath, path, name);
        impl |?< path => Hashtbl.replace(nameForPath, path, name);
    | _ => ()
  };

  let localModuleNames = Hashtbl.create(10);
  let depsModuleNames = Hashtbl.create(10);

  depSource->Belt.List.forEach(dep => {
    // Log.log("For dep " ++ dep);
    let allFiles = dep->Files.readDirectory;
    let prefix = allFiles->Belt.List.reduce(None, (found, name) => {
      switch (found) {
        | Some(m) => Some(m)
        | None =>
          switch (Str.split(Str.regexp_string("__"), name)) {
            | [prefix, _name, ..._] =>
            // Log.log("Prefix dor " ++ dep ++ ": " ++ prefix);
            Some(prefix)
            | _ => None
          }
      }
    });
    let filesByName = Hashtbl.create(10);
    let moduleNames = Hashtbl.create(10);
    allFiles->Belt.List.keep(isSourceFile)->Belt.List.forEach(file => {
      let full = dep /+ file;
      // Log.log("Dep " ++ full)
      filesByName->Hashtbl.replace(file, full)
      moduleNames->Hashtbl.replace(file->Filename.chop_extension, ())
    });
    moduleNames->Hashtbl.to_seq_keys->Stdlib.List.of_seq->Belt.List.forEach(mname => {
      let moduleName = switch prefix {
        | None => mname
        | Some(prefix) => prefix ++ "__" ++ (prefix == "stdlib" ? mname : mname->String.capitalize_ascii)
      } |> String.capitalize_ascii;
      // let moduleName = fullName->String.capitalize_ascii;
      let src = filesByName->maybeHash(mname ++ ".ml") |?? filesByName->maybeHash(mname ++ ".re");
      let srci = filesByName->maybeHash(mname ++ ".mli") |?? filesByName->maybeHash(mname ++ ".rei");
      let cmi = cmiByModuleName->maybeHash(moduleName);
      let cmt = cmtByModuleName->maybeHash(moduleName);
      let cmti = cmtiByModuleName->maybeHash(moduleName);
      let (moduleName, cmi, cmt, cmti) = switch (cmi, cmt, cmti) {
        // TODO it would be nice not to have to do this fallback
        | (None, None, None) => (
          mname->String.capitalize_ascii,
          cmiByModuleName->maybeHash(mname->String.capitalize_ascii),
          cmtByModuleName->maybeHash(mname->String.capitalize_ascii),
          cmtiByModuleName->maybeHash(mname->String.capitalize_ascii),
        )
        | _ => (moduleName, cmi, cmt, cmti)
      };
      // let fullName = fullName->String.capitalize_ascii;
      depsModuleNames->Hashtbl.replace(moduleName, ());
      let%opt_consume paths = calcPaths(moduleName, {src, srci, cmi, cmt, cmti}) |> orLog;
      pathsForModule->Hashtbl.replace(moduleName, paths);
      add(moduleName, paths);
    })
  });

  localSource->Belt.List.forEach(local => {
    let local = local == "." ? base : base /+ local;
    Log.log("For local dir " ++ local);
    let dune = local /+ "dune";
    if (!Files.exists(dune)) {
      Log.log("No dune file for source directory: " ++ local ++ ". Skipping");
    } else {
      let duneData = JbuildFile.parse(Files.readFileExn(dune));
      let filesByName = Hashtbl.create(10);
      let moduleNames = Hashtbl.create(10);
      local->Files.readDirectory->Belt.List.keep(isSourceFile)->Belt.List.forEach(file => {
        let full = local /+ file;
        Log.log("Full " ++ full);
        filesByName->Hashtbl.replace(file, full)
        moduleNames->Hashtbl.replace(file->Filename.chop_extension, ())
      });

      let libsAndBinaries = JbuildFile.getLibsAndBinaries(duneData);
      libsAndBinaries->Belt.List.forEach(((kind, public, private, modules)) => {
        let modules = switch modules {
          | None => Stdlib.Hashtbl.to_seq_keys(moduleNames)->Stdlib.List.of_seq;
          | Some(modules) => modules
        };
        Log.log("One ");
        let%opt_consume privateName = switch (public, private) {
          | (Some(p), None) => Ok(p)
          | (_, Some(p)) => Ok(p)
          | (None, None) => Error("No name found for dune item")
        } |> orLog;
        modules->Belt.List.forEach(mname => {
          let moduleName =
            kind == `UnwrappedLibrary
              ? mname
              : mname->String.capitalize_ascii == privateName->String.capitalize_ascii
                  ? mname->String.capitalize_ascii : privateName ++ "__" ++ mname->String.capitalize_ascii;
          // let moduleName = 
          let src = filesByName->maybeHash(mname ++ ".ml") |?? filesByName->maybeHash(mname ++ ".re");
          let srci = filesByName->maybeHash(mname ++ ".mli") |?? filesByName->maybeHash(mname ++ ".rei");
          let cmi = cmiByModuleName->maybeHash(moduleName);
          let cmt = cmtByModuleName->maybeHash(moduleName);
          let cmti = cmtiByModuleName->maybeHash(moduleName);
          let%opt_consume paths = calcPaths(moduleName, {src, srci, cmi, cmt, cmti}) |> orLog;
          localModuleNames->Hashtbl.replace(moduleName, ());
          pathsForModule->Hashtbl.replace(moduleName, paths);
          add(moduleName, paths);
        });
        switch kind {
          | `Library =>
            Log.log("A library " ++ privateName);
            let libModuleName = privateName->String.capitalize_ascii;
            let cmi = cmiByModuleName->maybeHash(libModuleName);
            let cmt = cmtByModuleName->maybeHash(libModuleName);
            let cmti = cmtiByModuleName->maybeHash(libModuleName);
            let%opt_consume paths = calcPaths(libModuleName, {src: None, srci: None, cmi, cmt, cmti}) |> orLog;
            Log.log("Ok " ++ libModuleName);
            // let privateName = privateName->String.capitalize_ascii;
            localModuleNames->Hashtbl.replace(libModuleName, ());
            pathsForModule->Hashtbl.replace(libModuleName, paths);
            add(libModuleName, paths);
          | `Binary => ()
          | `UnwrappedLibrary => ()
        }
      });
      ()
    }
  });

  // Go through the local source directories
  // for each one, look for a build directory that matches it
  // buut to do that I need to parse the dunefile, probably.
  // So:
  // Source directory, dunefile
  //   give me a list of `name, Library | Executable, modules`
  // Then I'll look for the compilation units that correspond.
  // Then for dependencies:
  // If there's a directory w/ both source & b in it, then process that
  // otherwise, :shrug:?
  // 
  // if there's a directory that directly matches, go with that.
  // Other issues: 

  // let add = (name, paths) => switch paths {
  //   | SharedTypes.Intf(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
  //   | SharedTypes.Impl(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
  //   | SharedTypes.IntfAndImpl(_, intf, _, impl) =>
  //       intf |?< path => Hashtbl.replace(nameForPath, path, name);
  //       impl |?< path => Hashtbl.replace(nameForPath, path, name);
  //   | _ => ()
  // };

  // dependencyModules |> List.iter(((modName, paths)) => {
  //   add(modName, paths);
  //   Hashtbl.replace(pathsForModule, modName, paths)
  // });

  // localModules |> List.iter(((modName, paths)) => {
  //   add(modName, paths);
  //   Hashtbl.replace(pathsForModule, modName, paths)
  // });

  (
    pathsForModule,
    nameForPath,
    localModuleNames->Hashtbl.to_seq_keys->List.of_seq,
    depsModuleNames->Hashtbl.to_seq_keys->List.of_seq,
    build
  );

};

// let getModulesFromMerlin = (base, text) => {
//   let (source, build, flags) = parseMerlin(base, text);
//   let sourceMap = Hashtbl.create(20);
//   let buildMap = Hashtbl.create(20);

//   source |> List.iter(dir => {
//     let full = maybeConcat(base, dir);
//     Files.readDirectory(full) |> List.iter(name => {
//       if (Filename.check_suffix(name, ".mli") || Filename.check_suffix(name, ".rei")) {
//         let base = Filename.chop_extension(name) |> String.capitalize_ascii;
//         Hashtbl.replace(sourceMap, base, Filename.concat(full, name))
//       } else if (Filename.check_suffix(name, ".ml") || Filename.check_suffix(name, ".re")) {
//         let base = Filename.chop_extension(name) |> String.capitalize_ascii;
//         if (!Hashtbl.mem(sourceMap, base)) {
//           Hashtbl.replace(sourceMap, base, Filename.concat(full, name))
//         }
//       }
//     })
//   });

//   build |> List.iter(dir => {
//     let full = maybeConcat(base, dir);
//     Files.readDirectory(full) |> List.iter(name => {
//       if (Filename.check_suffix(name, ".cmti")) {
//         /* TODO handle namespacing */
//         let base = Filename.chop_extension(name) |> String.capitalize_ascii;
//         Hashtbl.replace(buildMap, base, Filename.concat(full, name))
//       } else if (Filename.check_suffix(name, ".cmt")) {
//         let base = Filename.chop_extension(name) |> String.capitalize_ascii;
//         if (!Hashtbl.mem(buildMap, base)) {
//           Hashtbl.replace(buildMap, base, Filename.concat(full, name))
//         }
//       }
//     })
//   });

//   let nm = Filename.concat(base, "node_modules");
//   let (local, deps) = Hashtbl.fold((_modname, path, (local, deps)) => {
//     let item = (path, None);
//     if (Utils.startsWith(path, nm) || !Utils.startsWith(path, base)) {
//       (local, [item, ...deps])
//     } else {
//       ([item, ...local], deps)
//     }
//   }, buildMap, ([], []));
//   (local, deps, flags)
// };

let getFlags = base =>
  RResult.InfixResult.(
    Files.readFile(base ++ "/.merlin")
    |> RResult.orError("no .merlin file")
    |?>> parseMerlin(base)
    |?>> (((_, _, flags)) => flags |> List.rev)
  );

let getBackend = (rootPath) => {
  let path = Filename.concat(rootPath, ".merlin");
  switch (Files.maybeStat(path)) {
  | Some({Unix.st_kind: Unix.S_REG}) =>
    let ic = open_in(path);
    let rec loop = () =>
      switch (input_line(ic)) {
      | exception End_of_file =>
        close_in(ic);
        RResult.Ok("js")
      | s =>
        if (s == "####{BSB GENERATED: NO EDIT") {
          switch(input_line(ic)) {
            | exception End_of_file => RResult.Error("Bsb merlin comment not ended correctly");
            | backendLine =>
            let len = String.length("# -backend ");
            let totalLen = String.length(backendLine);
            try (RResult.Ok(String.sub(backendLine, len, totalLen - len))) {
              | _ => RResult.Ok("native")
            }
          }
        } else {
          loop()
        }
      };
    loop()
  | _ => RResult.Ok("js")
  }
}
