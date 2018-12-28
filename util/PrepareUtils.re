
let addToPath = ((name, inner), more) => (name, inner @ [more]);
let toFullPath = (pathType, (name, inner)) => (name, inner, pathType);

let filterNil = (fn, items) => List.fold_left(
  (items, item) => switch (fn(item)) {
  | None => items
  | Some(item) => [item, ...items]
  },
  [],
  items
);


let findStars = line => {
  let l = String.length(line);
  let rec loop = i => {
    if (i >= l - 1) {
      None
    } else if (line.[i] == '*' && line.[i + 1] == ' ') {
      Some(i + 2)
    } else if (line.[i] != ' ') {
      None
    } else {
      loop(i + 1)
    }
  };
  loop(0)
};

let combine = (one, two) => switch (one, two) {
| (None, None) => None
| (Some(a), None) => Some(a)
| (None, Some(b)) => Some(b)
| (Some(a), Some(b)) => a == b ? Some(a) : Some(0)
};

let trimFirst = (num, string) => {
  let length = String.length(string);
  length > num ? String.sub(string, num, length - num) : ""
};

let cleanOffStars = doc => {
  let lines = Str.split(Str.regexp_string("\n"), doc);
  let rec loop = (lines) => {
    switch lines {
    | [] => None
    | [one] => (String.trim(one) == "") ? None : findStars(one)
    | [one, ...rest] => (String.trim(one) == "") ? loop(rest) : combine(findStars(one), loop(rest))
    }
  };
  let num = loop(lines);
  switch num {
  | None | Some(0) => doc
  | Some(num) => switch lines {
    | [] | [_] => doc
    | [one, ...rest] => {
      if (findStars(one) != None) {
        trimFirst(num, one)
      } else {
        String.trim(one)
      }
    } ++ "\n" ++ String.concat("\n", rest |> List.map(trimFirst(num)))
    }
  }
};

let rec hasNoDoc = attributes => {
  switch attributes {
  | [] => false
  | [({Asttypes.txt: "nodoc"}, _), ..._] => true
  | [_, ...rest] => hasNoDoc(rest)
  }
};

let foldOpt = (fn, items, base) => List.fold_left((items, item) => switch (fn(item)) { | None => items | Some(x) => [x, ...items]}, base, items);


let either = (a, b) => switch (a, b) {
| (Some(a), _) => Some(a)
| (_, Some(b)) => Some(b)
| _ => None
};

let mapFst = (fn, (a, b)) => (fn(a), b);

let eitherFirst = (opt, (opt2, second)) => {
  (either(opt, opt2), second)
};

let compose = (filter, opter, value) => if (filter(value)) {
  opter(value)
} else {
  None
};