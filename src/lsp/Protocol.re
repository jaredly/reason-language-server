open Rpc.J;

let pos = (~line, ~character) => o([("line", i(line)), ("character", i(character))]);

let range = (~start, ~end_) => o([("start", start), ("end", end_)]);

open Infix;

let getTextDocument = (doc) =>
  Json.get("uri", doc)
  |?> Json.string
  |?> (
    (uri) =>
      Json.get("version", doc)
      |?> Json.number
      |?> (
        (version) => Json.get("text", doc) |?> Json.string |?>> ((text) => (uri, version, text))
      )
  );

let getPosition = (pos) =>
  Json.get("line", pos)
  |?> Json.number
  |?> (
    (line) =>
      Json.get("character", pos)
      |?> Json.number
      |?>> ((character) => (int_of_float(line), int_of_float(character)))
  );

let rgetPosition = (pos) =>
  Result.InfixResult.(
    RJson.get("line", pos)
    |?> RJson.number
    |?> (
      (line) =>
        RJson.get("character", pos)
        |?> RJson.number
        |?>> ((character) => (int_of_float(line), int_of_float(character)))
    )
  );

let rgetRange = (pos) =>
  Result.InfixResult.(
    RJson.get("start", pos)
    |?> rgetPosition
    |?> ((start) => RJson.get("end", pos) |?> rgetPosition |?>> ((end_) => (start, end_)))
  );

let rPositionParams = (params) =>
  Result.InfixResult.(
    RJson.get("textDocument", params)
    |?> RJson.get("uri")
    |?> RJson.string
    |?> ((uri) => RJson.get("position", params) |?> rgetPosition |?>> ((pos) => (uri, pos)))
  );

let posOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) =>
  o([("line", i(pos_lnum - 1)), ("character", i(pos_cnum - pos_bol))]);

let tupleOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => (pos_lnum - 1, pos_cnum - pos_bol);

let markup = (text) =>
  Json.Object([("kind", Json.String("markdown")), ("value", Json.String(text))]);

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
        | None => "file://" ++ pos_fname
        }
      )
    )
  ]);

let rangeOfInts = (l0, c0, l1, c1) =>
  o([("start", pos(~line=l0, ~character=c0)), ("end", pos(~line=l1, ~character=c1))]);

let locationContains = ({Location.loc_start, loc_end}, pos) =>
  tupleOfLexing(loc_start) <= pos && tupleOfLexing(loc_end) >= pos;

let symbolKind = (kind) =>
  switch kind {
  | `File => 1
  | `Module => 2
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

let rec variableKind = (t) =>
  switch t.Types.desc {
  | Tlink(t) => variableKind(t)
  | Tsubst(t) => variableKind(t)
  | Tarrow(_) => `Function
  | Ttuple(_) => `Array
  | Tconstr(_) => `Variable
  | Tobject(_) => `Object
  | Tnil => `Null
  | Tvariant(_) => `EnumMember
  | Tpoly(_) => `EnumMember
  | Tpackage(_) => `Module
  | _ => `Variable
  };

let typeKind = (t) =>
  switch t.Types.type_kind {
  | Type_open
  | Type_abstract => `TypeParameter
  | Type_record(_) => `Interface
  | Type_variant(_) => `Enum
  };