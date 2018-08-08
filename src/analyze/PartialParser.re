
type current = Comment(int) | String | Normal;
/* NOTE disabled because I couldn't distinguish between char and 'a type variable */
/* | Char; */
let hasUnterminatedCommentOrString = (text, ln) => {
  let rec loop = (current, i) =>
    i >= ln ?
      current != Normal :
      (
        switch (current) {
        | String =>
          text.[i] == '"' ?
            loop(Normal, i + 1) :
            loop(String, text.[i] == '\\' ? i + 2 : i + 1)
        /* | Char =>
          text.[i] == '\'' ?
            loop(Normal, i + 1) :
            loop(Char, text.[i] == '\\' ? i + 2 : i + 1) */
        | Comment(level) =>
          i + 1 >= ln ?
            true :
            text.[i] == '*' && text.[i + 1] == '/' ?
              loop(level == 0 ? Normal : Comment(level - 1), i + 2) :
              text.[i] == '/' && text.[i + 1] == '*' ?
                loop(Comment(level + 1), i + 2) :
                loop(Comment(level), i + 1)
        | Normal =>
          text.[i] == '"' ?
            loop(String, i + 1) :
            /* text.[i] == '\'' ?
              loop(Char, i + 1) : */
              text.[i] == '/' && i + 1 < ln && text.[i + 1] == '*' ?
                loop(Comment(0), i + 2) : loop(Normal, i + 1)
        }
      );
  loop(Normal, 0);
};

let rec findBack = (text, char, i) => {
  if (i < 0) { 0 } else if (text.[i] == char && (i == 0 || text.[i-1] != '/')) {
    i - 1
  } else {
    findBack(text, char, i - 1)
  }
};

let rec findOpenComment = (text, i) => {
  if (i < 1) { 0 } else if (text.[i] == '*' && text.[i - 1] == '/') {
    i - 2
  } else {
    findOpenComment(text, i - 1)
  }
};

let rec findBackSkippingCommentsAndStrings = (text, char, pair, i, level) => {
  let loop = findBackSkippingCommentsAndStrings(text, char, pair);
  if (i < 0) { 0 } else if (text.[i] == char) {
    if (level == 0) {
      i - 1
    } else {
      loop(i - 1, level - 1);
    }
  } else if (text.[i] == pair) {
    loop(i - 1, level + 1)
  } else {
    switch (text.[i]) {
    | '"' => loop(findBack(text, '"', i - 1), level)
    | '\'' => loop(findBack(text, '\'', i - 1), level)
    | '/' when (i >= 1 && text.[i - 1] == '*') => loop(findOpenComment(text, i - 2), level)
    | _ => loop(i - 1, level)
    }
  }
};

let rec skipWhite = (text, i) => if (i < 0) { 0 } else {
  switch (text.[i]) {
  | ' ' | '\n' | '\t' => skipWhite(text, i - 1)
  | _ => i
  }
};

let rec startOfLident = (text, i) => if (i < 0) { 0 } else {
  switch (text.[i]) {
  | 'a'..'z' | 'A'..'Z' | '.' | '_' | '0'..'9' => startOfLident(text, i - 1)
  | _ => i + 1
  }
};

let rec findArgLabel = (text, i) => if (i < 0) { None } else {
  switch (text.[i]) {
  | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' => findArgLabel(text, i - 1)
  /* TODO support ?punning and ~punning */
  | '~' => Some(i)
  | _ => None
  }
};

open Infix;

let findFunctionCall = text => {
  let rec loop = (commas, labels, i) => {
    if (i > 0) {
      switch (text.[i]) {
      | '}' => loop(commas, labels, findBackSkippingCommentsAndStrings(text, '{', '}', i - 1, 0))
      | ']' => loop(commas, labels, findBackSkippingCommentsAndStrings(text, '[', ']', i - 1, 0))
      | ')' => loop(commas, labels, findBackSkippingCommentsAndStrings(text, '(', ')', i - 1, 0))
      | '"' => loop(commas, labels, findBack(text, '"', i - 1))
      | '=' => switch (findArgLabel(text, i - 1)) {
        | None => loop(commas, labels, i - 1)
        | Some(i0) => loop(commas, [String.sub(text, i0 + 1, i - i0 - 1), ...labels], i0 - 1)
        }
      /* Not 100% this makes sense, but I think so? */
      | '{' | '[' => None
      | '(' => switch (text.[i - 1]) {
        | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' => {
          let i0 = startOfLident(text, i - 2);
          Some((commas, labels, String.sub(text, i0, i - i0)))
        }
        | _ => loop(commas, labels, i - 1)
      }
      | ',' => loop(commas + 1, labels, i - 1)
      | _ => if (i >= 1 && text.[i] == '/' && text.[i - 1] == '*') {
          loop(commas, labels, findOpenComment(text, i - 2))
        } else {
          loop(commas, labels, i - 1)
        }
      }
    } else {
      None;
    }
  };
  loop(0, [], String.length(text) - 1) |?>> ((commas, labels, lident)) => (commas, Array.of_list(labels), lident);
};



let findJsxTag = text => {
  let rec loop = (labels, i) => {
    if (i > 0) {
      switch (text.[i]) {
      | '}' => loop(labels, findBackSkippingCommentsAndStrings(text, '{', '}', i - 1, 0))
      | ']' => loop(labels, findBackSkippingCommentsAndStrings(text, '[', ']', i - 1, 0))
      | ')' => loop(labels, findBackSkippingCommentsAndStrings(text, '(', ')', i - 1, 0))
      | '"' => loop(labels, findBack(text, '"', i - 1))
      | '=' => switch (text.[i - 1]) {
        | 'a'..'z' | 'A'..'Z' | '_' => {
          let i0 = startOfLident(text, i - 1);
          /* TODO support punning */
          loop([String.sub(text, i0, i - i0), ...labels], i0 - 1)
        }
        | _ => loop(labels, i - 1)
        }
      | '{' | '[' | '(' | '>' | ';' => None
      | ' ' | '\n' => switch (text.[i - 1]) {
        | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' => {
          let i0 = startOfLident(text, i - 3);
          if (i0 > 0 && text.[i0 - 1] == '<') {
            Some((labels, String.sub(text, i0, i - i0)))
          } else { loop(labels, i - 1) }
        }
        | _ => loop(labels, i - 1)
      }
      | _ => if (i >= 1 && text.[i] == '/' && text.[i - 1] == '*') {
          loop(labels, findOpenComment(text, i - 2))
        } else {
          loop(labels, i - 1)
        }
      }
    } else {
      None;
    }
  };
  loop([], String.length(text) - 1) |?>> ((labels, lident)) => (Array.of_list(labels), lident);
};

type completable = Nothing | Labeled(string) | Lident(string);

let findCompletable = (text, offset) => {
  Log.log("Finding completable");
  /* if (hasUnterminatedCommentOrString(text, offset)) {
    Log.log("Unterminated comment or string, can't do it. Sorry");
    Nothing
  } else { */
    /* Log.log("Not unterminated"); */
    /** TODO handle being in the middle of an identifier */
    let rec loop = i => {
      i < 0 ? Lident(String.sub(text, i + 1, offset - (i + 1))) : switch (text.[i]) {
      | '~' => Labeled(String.sub(text, i + 1, offset - (i + 1)))
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' => loop(i - 1)
      | _ => {
        i == offset - 1 ? Nothing : Lident(String.sub(text, i + 1, offset - (i + 1)))
      }
      }
    };
    loop(offset - 1)
  /* } */
};


let findOpens = (text, current) => {
  let opens = ref([]);
  let add = o => opens := [o, ...opens^];

  let maybeOpen = i0 => {
    let rec loop = i => {
      if (i < 4) {
        0
      } else {
        switch (text.[i]) {
        | 'a'..'z' | 'A'..'Z' | '.' | '_' | '0'..'9' => loop(i - 1)
        | ' ' => {
          let at = skipWhite(text, i - 1);
          if (at >= 3 &&
            text.[at - 3] == 'o' &&
            text.[at - 2] == 'p' &&
            text.[at - 1] == 'e' &&
            text.[at] == 'n'
            ) {
            add(String.sub(text, i + 1, i0 + 1 - (i + 1)));
            at - 4
          } else {
            at
          }
        }
        | _ => i
        }
      }
    };
    loop(i0 - 1)
  };

  let rec loop = i => {
    if (i > 1) {
      switch (text.[i]) {
      | '}' => loop(findBackSkippingCommentsAndStrings(text, '{', '}', i - 1, 0))
      | ']' => loop(findBackSkippingCommentsAndStrings(text, '[', ']', i - 1, 0))
      | ')' => loop(findBackSkippingCommentsAndStrings(text, '(', ')', i - 1, 0))
      | '"' => loop(findBack(text, '"', i - 1))
      | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' => loop(maybeOpen(i))
      | '(' when text.[i - 1] == '.' => switch (text.[i - 2]) {
        | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' => {
          let i0 = startOfLident(text, i - 3);
          add(String.sub(text, i0, i - i0 - 1));
        }
        | _ => loop(i - 1)
      }
      | _ => if (i > 1 && text.[i] == '/' && text.[i - 1] == '*') {
          loop(findOpenComment(text, i - 2))
        } else {
          loop(i - 1)
        }
      }
    }
  };
  loop(current - 1) |> ignore;
  opens^;
};

let offsetOfLine = (text, line) => {
  let ln = String.length(text);
  let rec loop = (i, lno) => i >= ln ? None : (switch (text.[i]) {
  | '\n' => lno == line - 1 ? Some(i + 1) : loop(i + 1, lno + 1)
  | _ => loop(i + 1, lno)
  });
  line == 0 ? Some(0) : loop(0, 0)
};

let positionToOffset = (text, (line, character)) => {
  offsetOfLine(text, line) |?>> (bol => bol + character);
};

let offsetToPosition = (text, offset) => {
  let ln = String.length(text);
  let rec loop = (i, bol, lno) => i == offset ? Some((lno, i - bol)) : (i >= ln ? None : (
    text.[i] == '\n' ? loop(i + 1, i + 1, lno + 1) : loop(i + 1, bol, lno)
  ));
  loop(0, 0, 0)
};
