type value = int
module S = Belt_SortArrayInt
module N = Belt_internalAVLset
module A = Belt_Array
type t = value N.t
let rec has (t : t) (x : value) =
  match N.toOpt t with
  | None  -> false
  | Some n ->
      let v = N.value n in
      (x = v) || (has (if x < v then N.left n else N.right n) x)
let rec compareAux e1 e2 =
  match (e1, e2) with
  | (h1::t1,h2::t2) ->
      let ((k1 : value),k2) = ((N.value h1), (N.value h2)) in
      if k1 = k2
      then
        compareAux (N.stackAllLeft (N.right h1) t1)
          (N.stackAllLeft (N.right h2) t2)
      else if k1 < k2 then (-1) else 1
  | (_,_) -> 0
let cmp s1 s2 =
  let (len1,len2) = ((N.size s1), (N.size s2)) in
  if len1 = len2
  then compareAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 [])
  else if len1 < len2 then (-1) else 1
let eq (s1 : t) s2 = (cmp s1 s2) = 0
let rec subset (s1 : t) (s2 : t) =
  match let open N in ((toOpt s1), (toOpt s2)) with
  | (None ,_) -> true
  | (_,None ) -> false
  | (Some t1,Some t2) ->
      let (l1,v1,r1) = let open N in ((left t1), (value t1), (right t1)) in
      let (l2,v2,r2) = let open N in ((left t2), (value t2), (right t2)) in
      if v1 = v2
      then (subset l1 l2) && (subset r1 r2)
      else
        if v1 < v2
        then (subset (let open N in create l1 v1 empty) l2) && (subset r1 s2)
        else (subset (let open N in create empty v1 r1) r2) && (subset l1 s2)
let rec get (n : t) (x : value) =
  match N.toOpt n with
  | None  -> None
  | Some t ->
      let v = N.value t in
      if x = v then Some v else get (if x < v then N.left t else N.right t) x
let rec getUndefined (n : t) (x : value) =
  match N.toOpt n with
  | None  -> Js.undefined
  | Some t ->
      let v = N.value t in
      if x = v
      then Js.Undefined.return v
      else getUndefined (if x < v then N.left t else N.right t) x
let rec getExn (n : t) (x : value) =
  match N.toOpt n with
  | None  ->
      Js.Exn.raiseError
        "File \"../others/internal_set.cppo.ml\", line 90, characters 14-20"
  | Some t ->
      let v = N.value t in
      if x = v then v else getExn (if x < v then N.left t else N.right t) x
let rec addMutate t (x : value) =
  match N.toOpt t with
  | None  -> N.singleton x
  | Some nt ->
      let k = N.value nt in
      if x = k
      then t
      else
        (let (l,r) = let open N in ((left nt), (right nt)) in
         if x < k
         then N.leftSet nt (addMutate l x)
         else N.rightSet nt (addMutate r x);
         N.return (N.balMutate nt))
let fromArray (xs : value array) =
  let len = A.length xs in
  if len = 0
  then N.empty
  else
    (let next = ref (S.strictlySortedLength xs) in
     let result =
       ref
         (if (!next) >= 0
          then N.fromSortedArrayAux xs 0 (!next)
          else
            (next := (- (!next));
             N.fromSortedArrayRevAux xs ((!next) - 1) (!next))) in
     for i = !next to len - 1 do
       result := (addMutate (!result) (A.getUnsafe xs i))
     done;
     !result)