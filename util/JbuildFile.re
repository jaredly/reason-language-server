open Infix;
open RResult;

let rec show = item => switch item {
  | `Ident(text) => "Ident<" ++ text ++ ">"
  | `Number(n) => "Number<" ++ string_of_float(n) ++ ">"
  | `String(s) => "String<" ++ s ++ ">"
  | `List(items) => "[" ++ String.concat(", ", List.map(show, items)) ++ "]"
};

let rec getNamedIdent = items => switch items {
  | [] => None
  | [`List([`Ident("name"), `Ident(s)]), ..._] => Some(s)
  | [`List([`Ident("name"), ..._]), ...rest] =>
    getNamedIdent(rest)
  | [`List([`Ident("public_name"), `Ident(s)]), ..._] => Some(s)
  | [`List([`Ident("public_name"), ..._]), ...rest] =>
    getNamedIdent(rest)
  | [_, ...rest] => getNamedIdent(rest)
};

let findLibraryName = jbuildConfig => {
  let rec loop = items => switch items {
    | [] => None
    | [`List([`Ident("library"), `List(items)]), ..._]
    | [`List([`Ident("library"), ...items]), ..._] =>
      /* Log.log("Found a library!");
      Log.log(String.concat("\n", List.map(show, items))); */
      getNamedIdent(items)
    | [item, ...rest] => {
      loop(rest)
    }
  };
  loop(jbuildConfig)
};

let hasIncludeSubdirs = jbuildConfig => {
  let rec loop = items => switch items {
    | [] => false
    | [`List([`Ident("include_subdirs"), `Ident("unqualified")]), ..._] => true
    | [_, ...rest] => loop(rest)
  };
  loop(jbuildConfig)
};
/* (include_subdirs unqualified) */

let findExecutableName = jbuildConfig => {
  let rec loop = items => switch items {
    | [] => None
    | [`List([`Ident("executable"), `List(items)]), ..._]
    | [`List([`Ident("executable"), ...items]), ..._] =>
      getNamedIdent(items)
    | [item, ...rest] => {
      loop(rest)
    }
  };
  loop(jbuildConfig)
};

let findName = jbuildConfig => {
  switch (findLibraryName(jbuildConfig)) {
    | Some(name) => `Library(name)
    | None => switch (findExecutableName(jbuildConfig)) {
      | Some(name) => `Executable(name)
      | None => `NoName
    }
  }
};

/**
(jbuild_version 1)

(library
 ((name MarkdownReasonReactFolx)
  (public_name markdown-reason-react)
  (libraries (reason))))

*/

let fail = (text, i, msg) => failwith(msg);

let parseString = (text, pos) => {
  /* let i = ref(pos); */
  let buffer = Buffer.create(String.length(text));
  let ln = String.length(text);
  let rec loop = (i) =>
    i >= ln ?
      fail(text, i, "Unterminated string") :
      (
        switch text.[i] {
        | '"' => i + 1
        | '\\' =>
          i + 1 >= ln ?
            fail(text, i, "Unterminated string") :
            (
              switch text.[i + 1] {
              | '/' =>
                Buffer.add_char(buffer, '/');
                loop(i + 2)
              | 'f' =>
                Buffer.add_char(buffer, '\012');
                loop(i + 2)
              | _ =>
                Buffer.add_string(buffer, Scanf.unescaped(String.sub(text, i, 2)));
                loop(i + 2)
              }
            )
        | c =>
          Buffer.add_char(buffer, c);
          loop(i + 1)
        }
      );
  let final = loop(pos);
  (Buffer.contents(buffer), final)
};

let rec skipComment = (raw, ln, i) => i >= ln ? i : switch (raw.[i]) {
  | '\n' => i + 1
  | _ => skipComment(raw, ln, i + 1)
};

let rec skipWhite = (raw, ln, i) => i >= ln ? i : switch (raw.[i]) {
  | ' ' | '\n' | '\t' => skipWhite(raw, ln, i + 1)
  | ';' => skipWhite(raw, ln, skipComment(raw, ln, i + 1))
  | _ => i
};

let rec parseIdent = (raw, ln, i) => i >= ln ? i : switch (raw.[i]) {
  | 'a'..'z' | 'A'..'Z' | '_' | '-' | '.' | '0'..'9' => parseIdent(raw, ln, i + 1)
  | _ => i
};

let rec parseInt = (raw, ln, i) => i >= ln ? i : switch (raw.[i]) {
  | '0'..'9' => parseInt(raw, ln, i + 1)
  | _ => i
};

let rec parseNumber = (raw, ln, i) => i >= ln ? i : {
  let i = parseInt(raw, ln, i);
  if (i < ln && raw.[i] == '.') {
    parseInt(raw, ln, i + 1)
  } else {
    i
  }
};

let rec atomToString = atom => switch atom {
  | `Ident(n) => n
  | `List(items) => "[" ++ (String.concat(", ", List.map(atomToString, items))) ++ "]"
  | `Number(n) => string_of_float(n)
  | `String(s) => "\"" ++ String.escaped(s) ++ "\""
};

let rec parseAtom = (raw, ln, i) => switch (raw.[i]) {
  | '%' =>
    if(raw.[i + 1] === '{') {
      let (lst, i) = parseList(~term='}', raw, ln, i + 2);
      (List.hd(lst), i);
    } else {
      let last = parseIdent(raw, ln, i + 1);
      (`Ident(String.sub(raw, i, last - i)), last);
    }
  | '0'..'9' =>
    let last = parseNumber(raw, ln, i + 1);
    (`Number(float_of_string(String.sub(raw, i, last - i))), last)
  | '"' =>
    let (text, last) = parseString(raw, i + 1);
    (`String(text), last)
  | '(' => {
    let (items, i) = parseList(raw, ln, i + 1);
    (`List(items), i)
  }
  /* Any other ASCII chars */
  | '!'..'~' =>
    let last = parseIdent(raw, ln, i + 1);
    (`Ident(String.sub(raw, i, last - i)), last);
  | _ => failwith("Unexpected char: " ++ String.sub(raw, i, 1) ++ " at " ++ string_of_int(i))
}
and parseList = (~term=')', raw, ln, i) => {
  let i = skipWhite(raw, ln, i);
  i >= ln ? ([], i) : switch (raw.[i]) {
    | x when x === term => ([], i + 1)
    | _ =>
      let (item, i) = parseAtom(raw, ln, i);
      let (rest, i) = parseList(~term, raw, ln, i);
      ([item, ...rest], i)
  }
};

let parse = raw => {
  let ln = String.length(raw);
  let rec loop = i => {
    let i = skipWhite(raw, ln, i);
    i >= ln ? [] : {
      let (atom, i) = parseAtom(raw, ln, i);
      [atom, ...loop(i)]
    }
  };
  loop(0)
};

let readFromDir = (dirPath) => {
  let filePath = dirPath /+ "dune";
  switch (Files.readFileResult(filePath)) {
  | Ok(x) => Ok((filePath, x))
  | Error(_) =>
    let filePath = dirPath /+ "jbuild";
    switch (Files.readFileResult(filePath)) {
    | Ok(x) => Ok((filePath, x))
    | Error(x) => Error(x)
    }
  };
};
