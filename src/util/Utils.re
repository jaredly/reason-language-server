let startsWith = (s, prefix) => {
  let p = String.length(prefix);
  p <= String.length(s) && String.sub(s, 0, p) == prefix
};

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
      |> Str.global_replace(Str.regexp_string("C:"), "/c%3A")
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
        |> Str.global_replace(Str.regexp_string("/c%3A"), "C:")
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

/*
  returns true if a MarkupKind[] contains "markdown"
  NOTE: maybe we should also check if "markdown" is first
*/
let hasMarkdownCap = (markupKind) => 
  Infix.(
    markupKind
      |> Json.array
      |?>> List.map(cap => Json.string(cap) |? "")
      |?>> List.mem("markdown")
  )