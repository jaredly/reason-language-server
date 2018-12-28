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

let maybeConcat = (base, path) => path.[0] == '/' ? path : Filename.concat(base, path);

let getModulesFromMerlin = (base, text) => {
  let (source, build, flags) = parseMerlin(base, text);
  let sourceMap = Hashtbl.create(20);
  let buildMap = Hashtbl.create(20);

  source |> List.iter(dir => {
    let full = maybeConcat(base, dir);
    Files.readDirectory(full) |> List.iter(name => {
      if (Filename.check_suffix(name, ".mli") || Filename.check_suffix(name, ".rei")) {
        let base = Filename.chop_extension(name) |> String.capitalize_ascii;
        Hashtbl.replace(sourceMap, base, Filename.concat(full, name))
      } else if (Filename.check_suffix(name, ".ml") || Filename.check_suffix(name, ".re")) {
        let base = Filename.chop_extension(name) |> String.capitalize_ascii;
        if (!Hashtbl.mem(sourceMap, base)) {
          Hashtbl.replace(sourceMap, base, Filename.concat(full, name))
        }
      }
    })
  });

  build |> List.iter(dir => {
    let full = maybeConcat(base, dir);
    Files.readDirectory(full) |> List.iter(name => {
      if (Filename.check_suffix(name, ".cmti")) {
        /* TODO handle namespacing */
        let base = Filename.chop_extension(name) |> String.capitalize_ascii;
        Hashtbl.replace(buildMap, base, Filename.concat(full, name))
      } else if (Filename.check_suffix(name, ".cmt")) {
        let base = Filename.chop_extension(name) |> String.capitalize_ascii;
        if (!Hashtbl.mem(buildMap, base)) {
          Hashtbl.replace(buildMap, base, Filename.concat(full, name))
        }
      }
    })
  });

  let nm = Filename.concat(base, "node_modules");
  let (local, deps) = Hashtbl.fold((_modname, path, (local, deps)) => {
    let item = (path, None);
    if (Utils.startsWith(path, nm) || !Utils.startsWith(path, base)) {
      (local, [item, ...deps])
    } else {
      ([item, ...local], deps)
    }
  }, buildMap, ([], []));
  (local, deps, flags)
};

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
