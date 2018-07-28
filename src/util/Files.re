
let split = (str, string) => Str.split(Str.regexp_string(str), string);

let absify = path => {
  if (path == "" || path == ".") {
    Unix.getcwd()
  } else if (path.[0] == '/') {
    path
  } else {
    Filename.concat(Unix.getcwd(), path)
  }
};

let removeExtraDots = path => Str.global_replace(Str.regexp_string("/./"), "/", path)
|> Str.global_replace(Str.regexp({|^\./\.\./|}), "../");

let startsWith = (text, prefix) => String.length(prefix) <= String.length(text)
  && String.sub(text, 0, String.length(prefix)) == prefix;
let sliceToEnd = (str, pos) => String.sub(str, pos, String.length(str) - pos);

[@test [
  (("/a/b/c", "/a/b/d"), "../d"),
  (("/a/b/c", "/a/b/d/e"), "../d/e"),
  (("/a/b/c", "/d/e/f"), "../../../d/e/f"),
  (("/a/b/c", "/a/b/c/d/e"), "./d/e"),
]]
let relpath = (base, path) => {
  if (startsWith(path, base)) {
    let baselen = String.length(base);
    let rest = String.sub(path, baselen, String.length(path) - baselen);
    if (rest == "") {
      "." ++ Filename.dir_sep
    } else if (rest.[0] == Filename.dir_sep.[0]) {
      if (String.length(rest) > 1 && rest.[1] == '.') {
        sliceToEnd(rest, 1)
      } else {
        "." ++ rest
      }
    } else if (rest.[0] == '.') {
      rest
    } else {
      "." ++ Filename.dir_sep ++ rest
    }
  } else {
    let rec loop = (bp, pp) => {
      switch (bp, pp) {
      | ([".", ...ra], _) => loop(ra, pp)
      | (_, [".", ...rb]) => loop(bp, rb)
      | ([a, ...ra], [b, ...rb]) when a == b => loop(ra, rb)
      | _ => (bp, pp)
      }
    };
    let (base, path) = loop(split(Filename.dir_sep, base), split(Filename.dir_sep, path));
    String.concat(Filename.dir_sep, (base == [] ? ["."] : List.map((_) => "..", base)) @ path) |> removeExtraDots
  }
};

let symlink = (source, dest) => {
  Unix.symlink(relpath(Filename.dirname(dest), source), dest)
};

let maybeStat = (path) =>
  try (Some(Unix.stat(path))) {
  | Unix.Unix_error(Unix.ENOENT, _, _) => None
  };

let getMtime = path => switch (maybeStat(path)) {
  | Some({Unix.st_mtime}) => Some(st_mtime)
  | _ => None
};

let readFile = path => {
  switch (maybeStat(path)) {
  | Some({Unix.st_kind: Unix.S_REG}) =>
    let ic = open_in(path);
    let try_read = () =>
      switch (input_line(ic)) {
      | exception End_of_file => None
      | x => Some(x)
      };
    let rec loop = (acc) =>
      switch (try_read()) {
      | Some(s) => loop([s, ...acc])
      | None =>
        close_in(ic);
        List.rev(acc)
      };
    let text = loop([]) |> String.concat(String.make(1, '\n'));
    Some(text)
  | _ => None
  }
};

let readFileExn = path => switch (readFile(path)) {
| None => failwith("Unable to read " ++ path)
| Some(text) => text
};

let readFileResult = path => switch (readFile(path)) {
| None => Result.Error("Unable to read " ++ path)
| Some(text) => Result.Ok(text)
};

let writeFile = (path, contents) => {
  try {
    let out = open_out(path);
    output_string(out, contents);
    close_out(out);
    true
  } {
    | _ => false
  }
};

let writeFileExn = (path, contents) => {
  if (!writeFile(path, contents)) {
    failwith("Unable to write to file " ++ path)
  }
};

let copy = (~source, ~dest) =>
  switch (maybeStat(source)) {
  | None => false
  | Some({Unix.st_perm}) =>
    let fs = Unix.openfile(source, [Unix.O_RDONLY], st_perm);
    let fd = Unix.openfile(dest, [Unix.O_WRONLY, Unix.O_CREAT, Unix.O_TRUNC], st_perm);
    let buffer_size = 8192;
    let buffer = Bytes.create(buffer_size);
    let rec copy_loop = () =>
      switch (Unix.read(fs, buffer, 0, buffer_size)) {
      | 0 => ()
      | r =>
        ignore(Unix.write(fd, buffer, 0, r));
        copy_loop()
      };
    copy_loop();
    Unix.close(fs);
    Unix.close(fd);
    true
  };

let copyExn = (~source, ~dest) => if (!copy(~source, ~dest)) {
  failwith("Unable to copy " ++ source ++ " to " ++ dest);
};

let exists = (path) =>
  switch (maybeStat(path)) {
  | None => false
  | Some(_) => true
  };

let ifExists = path => exists(path) ? Some(path) : None;

let isFile = path => switch (maybeStat(path)) {
| Some({Unix.st_kind: Unix.S_REG}) => true
| _ => false
};

let isDirectory = path => switch (maybeStat(path)) {
| Some({Unix.st_kind: Unix.S_DIR}) => true
| _ => false
};

let readDirectory = (dir) => {
  let maybeGet = (handle) =>
    try (Some(Unix.readdir(handle))) {
    | End_of_file => None
    };
  let rec loop = (handle) =>
    switch (maybeGet(handle)) {
    | None =>
      Unix.closedir(handle);
      []
    | Some(name) when name == Filename.current_dir_name || name == Filename.parent_dir_name => loop(handle)
    | Some(name) => [name, ...loop(handle)]
    };
  loop(Unix.opendir(dir))
};

let rec mkdirp = (dest) =>
  if (! exists(dest)) {
    let parent = Filename.dirname(dest);
    mkdirp(parent);
    Unix.mkdir(dest, 0o740);
    if (!exists(dest)){
      failwith("Unable to create " ++ dest);
    }
  };

let rec copyDeep = (~source, ~dest) => {
  mkdirp(Filename.dirname(dest));
  switch (maybeStat(source)) {
  | None => ()
  | Some({Unix.st_kind: Unix.S_DIR}) =>
    readDirectory(source)
    |> List.iter(
         (name) =>
           copyDeep(~source=Filename.concat(source, name), ~dest=Filename.concat(dest, name))
       )
  | Some({Unix.st_kind: Unix.S_REG}) => copy(~source, ~dest) |> ignore
  | _ => ()
  }
};

let rec removeDeep = path => {
  switch (maybeStat(path)) {
  | None => ()
  | Some({Unix.st_kind: Unix.S_DIR}) =>
    readDirectory(path)
    |> List.iter((name) => removeDeep(Filename.concat(path, name)));
    Unix.rmdir(path);
  | _ => Unix.unlink(path)
  }
};

let rec walk = (path, fn) => {
  switch (maybeStat(path)) {
  | None => ()
  | Some({Unix.st_kind: Unix.S_DIR}) => readDirectory(path) |> List.iter((name) => walk(Filename.concat(path, name), fn));
  | _ => fn(path)
  }
};

let rec collectDirs = (path) => {
  switch (maybeStat(path)) {
  | None => []
  | Some({Unix.st_kind: Unix.S_DIR}) => [path, ...readDirectory(path) |> List.map((name) => collectDirs(Filename.concat(path, name))) |> List.concat];
  | _ => []
  }
};

let rec collect = (~checkDir=x => true, path, test) => {
  switch (maybeStat(path)) {
  | None => []
  | Some({Unix.st_kind: Unix.S_DIR}) => if (checkDir(path)) {
    readDirectory(path) |> List.map((name) => collect(~checkDir, Filename.concat(path, name), test)) |> List.concat;
  } else {
    []
  }
  | _ => test(path) ? [path] : []
  }
};
