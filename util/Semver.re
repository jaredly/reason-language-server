type predicate = 
  | GreaterEqual
  | Greater
  | Lesser
  | LesserEqual
  | Approximate
  | Compatible
  | Exact;

type t = {
  predicate,
  major: string,
  minor: string,
  patch: string,
  metadata: string
};

let none = {
  predicate: Exact,
  major: "0",
  minor: "0",
  patch: "0",
  metadata: ""
}

let major = ({major}) => major;
let minor = ({major}) => major;
let patch = ({patch}) => patch;
let metadata = ({metadata}) => metadata;

let parseNumber = (txt, startIx, len) => {  
  let rec loop = (ix) => {
    if(ix < len) {
      let c = String.unsafe_get(txt, ix);
      if (c >= '0' && c <= '9'){
        loop(ix + 1)
      } else  {
        let version = String.sub(txt, startIx, ix  - startIx);
        (version, c == '.' ? ix + 1 : ix)
      };
    } else {
      let version = String.sub(txt, startIx, len - startIx);
      (version, ix)
    }
  };

  loop(startIx)
};

let parse = (version: string) => {
  if(String.length(version) < 1) {
    none
  } else {
    let (predicate, nextIx) = switch(String.unsafe_get(version, 0)) {
    | '>' => 
      String.unsafe_get(version, 1) == '='
        ? (GreaterEqual, 2)
        : (Greater, 1)
    | '<' =>
      String.unsafe_get(version, 1) == '='
        ? (LesserEqual, 2)
        : (Lesser, 1)
    | '^' => (Compatible, 1)
    | '~' => (Approximate, 1)
    |  _ => (Exact, 0)
    };

    let len = String.length(version);
    let (major, nextIx) = parseNumber(version, nextIx, len);
    let (minor, nextIx) = parseNumber(version, nextIx, len);
    let (patch, nextIx) = parseNumber(version, nextIx, len);
    let metadata = nextIx < len
      ? String.sub(version, nextIx, len - nextIx)
      : "";

    {
      predicate,
      major,
      minor,
      patch,
      metadata
    }
  }
};