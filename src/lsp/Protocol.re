open Rpc.J;

let pos = (~line, ~character) => o([("line", i(line)), ("character", i(character))]);

let range = (~start, ~end_) => o([("start", start), ("end", end_)]);

open Infix;

let getTextDocument = doc => {
  let%opt uri = Json.get("uri", doc) |?> Json.string;
  let%opt version = Json.get("version", doc) |?> Json.number;
  let%opt text = Json.get("text", doc) |?> Json.string;
  Some((uri, version, text))
};

let getPosition = (pos) => {
  let%opt line = Json.get("line", pos) |?> Json.number;
  let%opt character = Json.get("character", pos) |?> Json.number;
  Some((int_of_float(line), int_of_float(character)))
};

let rgetPosition = (pos) => {
  open Result.InfixResult;
  let%try line = RJson.get("line", pos) |?> RJson.number;
  let%try character = RJson.get("character", pos) |?> RJson.number;
  Ok((int_of_float(line), int_of_float(character)))
};

let rgetRange = (pos) => {
  open Result.InfixResult;
  let%try start = RJson.get("start", pos) |?> rgetPosition;
  let%try end_ = RJson.get("end", pos) |?> rgetPosition;
  Ok((start, end_))
};

let rPositionParams = (params) => {
  open Result.InfixResult;
  let%try uri = RJson.get("textDocument", params) |?> RJson.get("uri") |?> RJson.string;
  let%try pos = RJson.get("position", params) |?> rgetPosition;
  Ok((uri, pos))
};

let posOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) =>
  o([("line", i(pos_lnum - 1)), ("character", i(pos_cnum - pos_bol))]);

let tupleOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => (pos_lnum - 1, pos_cnum - pos_bol);

let contentKind = (useMarkdown, text) =>
  Json.Object([("kind", Json.String(useMarkdown ? "markdown" : "text")), ("value", Json.String(text))]);

let rangeOfLoc = ({Location.loc_start, loc_end}) =>
  o([("start", posOfLexing(loc_start)), ("end", posOfLexing(loc_end))]);

let locationOfLoc = (~fname=?, {Location.loc_start: {Lexing.pos_fname}} as loc) =>
  o([
    ("range", rangeOfLoc(loc)),
    (
      "uri",
      s(
        switch fname {
        | Some(x) => x
        | None => Utils.toUri(pos_fname)
        }
      )
    )
  ]);

let rangeOfInts = (l0, c0, l1, c1) =>
  o([("start", pos(~line=l0, ~character=c0)), ("end", pos(~line=l1, ~character=c1))]);

let locationContains = ({Location.loc_start, loc_end}, pos) =>
  tupleOfLexing(loc_start) <= pos && tupleOfLexing(loc_end) >= pos;

/** Check if pos is within the location, but be fuzzy about when the location ends.
If it's within 5 lines, go with it.
 */
let locationContainsFuzzy = ({Location.loc_start, loc_end}, (l, c)) =>
  tupleOfLexing(loc_start) <= (l, c) && tupleOfLexing(loc_end) >= (l - 5, c);

let symbolKind = (kind) =>
  switch kind {
  | `File => 1
  | `Module => 2
  | `ModuleType => 2
  | `Namespace => 3
  | `Package => 4
  | `Class => 5
  | `Method => 6
  | `Property => 7
  | `Field => 8
  | `Constructor => 9
  | `Enum => 10
  | `Interface => 11
  | `Function => 12
  | `Variable => 13
  | `Constant => 14
  | `String => 15
  | `Number => 16
  | `Boolean => 17
  | `Array => 18
  | `Object => 19
  | `Key => 20
  | `Null => 21
  | `EnumMember => 22
  | `Struct => 23
  | `Event => 24
  | `Operator => 25
  | `TypeParameter => 26
  };

/*
  returns true if a MarkupKind[] contains "markdown"
*/
let hasMarkdownCap = (markupKind) => {
  let%opt kinds = Json.array(markupKind) |?>> optMap(Json.string);
  Some(List.mem("markdown", kinds))
};
