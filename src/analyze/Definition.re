/* TODO move these to utils */

let maybeFound = (fn, a) =>
  switch (fn(a)) {
  | exception Not_found => None
  | x => Some(x)
  };

let inRange = ((l, c), ((l0, c0), (l1, c1))) => {
  let l = l + 1;
  (l0 < l || l0 == l && c0 <= c) && (l1 == (-1) && c1 == (-1) || l1 > l || l1 == l && c1 > c)
};

let dig = Query.dig;

let handleConstructor = (path, txt) => {
  let typeName =
    switch path {
    | Path.Pdot(path, typename, _) => typename
    | Pident({Ident.name}) => name
    | _ => assert false
    };
  Longident.(
    switch txt {
    | Longident.Lident(name) => (name, Lident(typeName))
    | Ldot(left, name) => (name, Ldot(left, typeName))
    | Lapply(left, _) => assert false
    }
  )
};
