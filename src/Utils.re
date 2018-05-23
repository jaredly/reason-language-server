
let startsWith = (s, prefix) => {
  let p = String.length(prefix);
  p <= String.length(s) && String.sub(s, 0, p) == prefix
};

let sliceToEnd = (s, start) => {
  let l = String.length(s);
  start <= l ? String.sub(s, start, l - start) : s
};

let chopPrefix = (s, prefix) => sliceToEnd(s, String.length(prefix));