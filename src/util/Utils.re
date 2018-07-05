/*
  steal from OCaml stdlib
  https://github.com/ocaml/ocaml/blob/7c9c210884e1b46f21af5bb4dfab995bb3336cf7/stdlib/string.ml#L205-L214 
*/
let split_on_char = (sep, s) => {
  open String;
  let r = ref([]);
  let j = ref(length(s));
  for (i in length(s) - 1 downto 0) {
    if (unsafe_get(s, i) == sep) {
      r := [sub(s, i + 1, j^ - i - 1), ...r^];
      j := i;
    };
  };
  [sub(s, 0, j^), ...r^];
};

let getFullLineOfPos = (pos, s) => {
  let left =
    switch (String.rindex_from(s, pos, '\n') + 1) {
    | pos => pos
    | exception Not_found => 0
    };
  let right =
    switch (String.index_from(s, pos, '\n') - 1) {
    | pos => pos
    | exception Not_found => String.length(s) - 1
    };
  String.sub(s, left, right - left);
};

let repeat = (length, s) => {
  let result = ref("");
  for (i in 1 to length) {
    result:= result^ ++ s
  };
  result^
};

let countLeading = (value, s) => {
  let length = String.length(s);
  let rec loop = (count, i) =>
    if (i < length && s.[i] == value) {
      loop(count + 1, i + 1);
    } else {
      count;
    };
  loop(0, 0);
};

let countTrailing = (value, s) => {
  let length = String.length(s);
  let rec loop = (count, i) =>
    if (i >= 0 && s.[i] == value) {
      loop(count + 1, i - 1);
    } else {
      count;
    };
  loop(0, length - 1);
};
/**
 * `startsWith(string, prefix)`
 * true if the string starts with the prefix
 */
let startsWith = (s, prefix) => {
  let p = String.length(prefix);
  p <= String.length(s) && String.sub(s, 0, p) == prefix
};

let stripAnsii = text => Str.global_replace(Str.regexp("\027\\[[0-9;]+m"), "", text);

let sliceToEnd = (s, start) => {
  let l = String.length(s);
  start <= l ? String.sub(s, start, l - start) : s
};

let toUri = (path) =>
  if (Sys.os_type == "Unix") {
    "file://" ++ path
  } else {
    "file://"
    ++ (
      Str.global_replace(Str.regexp_string("\\"), "/", path)
      |> Str.substitute_first(Str.regexp("^([A-Z]):"), text => {
        let name = Str.matched_group(1, text);
        "/" ++ String.lowercase(name) ++ "%3A"
      })
    )
  };

let parseUri = (uri) =>
  if (startsWith(uri, "file://")) {
    let withoutScheme = sliceToEnd(uri, String.length("file://"));
    if (Sys.os_type == "Unix") {
      Some(withoutScheme)
    } else {
      Some(
        withoutScheme
        |> Str.substitute_first(Str.regexp("^/([a-z])%3A"), text => {
          let name = Str.matched_group(1, text);
          String.uppercase(name) ++ ":"
        })
        /* OCaml doesn't want to do a find & replace where the replacement is a single backslash. So this works */
        |> Str.split(Str.regexp_string("/"))
        |> String.concat({|\|})
      )
    }
  } else {
    None
  };

let endOfLocation = (loc, length) =>
  Location.{
    ...loc,
    loc_start: {
      ...loc.loc_end,
      pos_cnum: loc.loc_end.pos_cnum - length
    }
  };

let clampLocation = (loc, length) =>
  Location.{
    ...loc,
    loc_end: {
      ...loc.loc_start,
      pos_cnum: loc.loc_start.pos_cnum + length
    }
  };

let chopPrefix = (s, prefix) => sliceToEnd(s, String.length(prefix));

let filterMap = (fn, items) =>
  List.fold_left(
    (results, item) =>
      switch (fn(item)) {
      | None => results
      | Some(x) => [x, ...results]
      },
    [],
    items
  );

/** An optional List.find */
let rec find = (fn, items) =>
  switch items {
  | [] => None
  | [one, ...rest] =>
    switch (fn(one)) {
    | None => find(fn, rest)
    | Some(x) => Some(x)
    }
  };

let showLocation = ({Location.loc_start, loc_end}) =>
  Lexing.(
    Printf.sprintf(
      "%d:%d - %d:%d",
      loc_start.pos_lnum,
      loc_start.pos_cnum - loc_start.pos_bol,
      loc_end.pos_lnum,
      loc_end.pos_cnum - loc_end.pos_bol
    )
  );
