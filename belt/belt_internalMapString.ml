type key = string
module N = Belt_internalAVLtree
module A = Belt_Array
module S = Belt_SortArray
type 'a t = (key,'a) N.t
let rec add t (x : key) (data : _) =
  match N.toOpt t with
  | None  -> N.singleton x data
  | Some n ->
      let k = N.key n in
      if x = k
      then N.return (N.updateValue n data)
      else
        (let v = N.value n in
         if x < k
         then N.bal (add (N.left n) x data) k v (N.right n)
         else N.bal (N.left n) k v (add (N.right n) x data))
let rec get n (x : key) =
  match N.toOpt n with
  | None  -> None
  | Some n ->
      let v = N.key n in
      if x = v
      then Some (N.value n)
      else get (if x < v then N.left n else N.right n) x
let rec getUndefined n (x : key) =
  match N.toOpt n with
  | None  -> Js.undefined
  | Some n ->
      let v = N.key n in
      if x = v
      then Js.Undefined.return (N.value n)
      else getUndefined (if x < v then N.left n else N.right n) x
let rec getExn n (x : key) =
  match N.toOpt n with
  | None  ->
      Js.Exn.raiseError
        "File \"../others/internal_map.cppo.ml\", line 51, characters 14-20"
  | Some n ->
      let v = N.key n in
      if x = v
      then N.value n
      else getExn (if x < v then N.left n else N.right n) x
let rec getWithDefault n (x : key) def =
  match N.toOpt n with
  | None  -> def
  | Some n ->
      let v = N.key n in
      if x = v
      then N.value n
      else getWithDefault (if x < v then N.left n else N.right n) x def
let rec has n (x : key) =
  match N.toOpt n with
  | None  -> false
  | Some n ->
      let v = N.key n in
      (x = v) || (has (if x < v then N.left n else N.right n) x)
let rec remove n (x : key) =
  match N.toOpt n with
  | None  -> n
  | Some n ->
      let (l,v,r) = let open N in ((left n), (key n), (right n)) in
      if x = v
      then
        (match ((N.toOpt l), (N.toOpt r)) with
         | (None ,_) -> r
         | (_,None ) -> l
         | (_,Some rn) ->
             let (kr,vr) = ((ref (N.key rn)), (ref (N.value rn))) in
             let r = N.removeMinAuxWithRef rn kr vr in N.bal l (!kr) (!vr) r)
      else
        if x < v
        then let open N in bal (remove l x) v (value n) r
        else let open N in bal l v (value n) (remove r x)
let rec splitAux (x : key) (n : _ N.node) =
  (let (l,v,d,r) = let open N in ((left n), (key n), (value n), (right n)) in
   if x = v
   then (l, (Some d), r)
   else
     if x < v
     then
       (match N.toOpt l with
        | None  -> let open N in (empty, None, (return n))
        | Some l ->
            let (ll,pres,rl) = splitAux x l in (ll, pres, (N.join rl v d r)))
     else
       (match N.toOpt r with
        | None  -> let open N in ((return n), None, empty)
        | Some r ->
            let (lr,pres,rr) = splitAux x r in ((N.join l v d lr), pres, rr)) : 
  (_ t* _ option* _ t))
let rec split (x : key) n =
  match N.toOpt n with
  | None  -> let open N in (empty, None, empty)
  | Some n -> splitAux x n
let rec mergeU s1 s2 f =
  match let open N in ((toOpt s1), (toOpt s2)) with
  | (None ,None ) -> N.empty
  | (Some n,_) when
      let open N in
        (height n) >=
          (match N.toOpt s2 with | None  -> 0 | Some n -> N.height n)
      ->
      let (l1,v1,d1,r1) =
        let open N in ((left n), (key n), (value n), (right n)) in
      let (l2,d2,r2) = split v1 s2 in
      N.concatOrJoin (mergeU l1 l2 f) v1 (f v1 (Some d1) d2) (mergeU r1 r2 f)
  | (_,Some n) ->
      let (l2,v2,d2,r2) =
        let open N in ((left n), (key n), (value n), (right n)) in
      let (l1,d1,r1) = split v2 s1 in
      N.concatOrJoin (mergeU l1 l2 f) v2 (f v2 d1 (Some d2)) (mergeU r1 r2 f)
  | _ -> assert false
let merge s1 s2 f = mergeU s1 s2 (fun a  -> fun b  -> fun c  -> f a b c)
let rec compareAux e1 e2 vcmp =
  match (e1, e2) with
  | (h1::t1,h2::t2) ->
      let c = Pervasives.compare (N.key h1 : key) (N.key h2) in
      if c = 0
      then
        let cx = vcmp (N.value h1) (N.value h2) in
        (if cx = 0
         then
           compareAux (N.stackAllLeft (N.right h1) t1)
             (N.stackAllLeft (N.right h2) t2) vcmp
         else cx)
      else c
  | (_,_) -> 0
let cmpU s1 s2 cmp =
  let (len1,len2) = ((N.size s1), (N.size s2)) in
  if len1 = len2
  then compareAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 []) cmp
  else if len1 < len2 then (-1) else 1
let cmp s1 s2 f = cmpU s1 s2 (fun a  -> fun b  -> f a b)
let rec eqAux e1 e2 eq =
  match (e1, e2) with
  | (h1::t1,h2::t2) ->
      if ((N.key h1 : key) = (N.key h2)) && (eq (N.value h1) (N.value h2))
      then
        eqAux (N.stackAllLeft (N.right h1) t1)
          (N.stackAllLeft (N.right h2) t2) eq
      else false
  | (_,_) -> true
let eqU s1 s2 eq =
  let (len1,len2) = ((N.size s1), (N.size s2)) in
  if len1 = len2
  then eqAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 []) eq
  else false
let eq s1 s2 f = eqU s1 s2 (fun a  -> fun b  -> f a b)
let rec addMutate (t : _ t) x data =
  (match N.toOpt t with
   | None  -> N.singleton x data
   | Some nt ->
       let k = N.key nt in
       if x = k
       then (N.keySet nt x; N.valueSet nt data; N.return nt)
       else
         (let (l,r) = ((N.left nt), (N.right nt)) in
          if x < k
          then (let ll = addMutate l x data in N.leftSet nt ll)
          else N.rightSet nt (addMutate r x data);
          N.return (N.balMutate nt)) : _ t)
let fromArray (xs : (key* _) array) =
  let len = A.length xs in
  if len = 0
  then N.empty
  else
    (let next =
       ref
         (S.strictlySortedLengthU xs (fun (x0,_)  -> fun (y0,_)  -> x0 < y0)) in
     let result =
       ref
         (if (!next) >= 0
          then N.fromSortedArrayAux xs 0 (!next)
          else
            (next := (- (!next));
             N.fromSortedArrayRevAux xs ((!next) - 1) (!next))) in
     for i = !next to len - 1 do
       (let (k,v) = A.getUnsafe xs i in result := (addMutate (!result) k v))
     done;
     !result)