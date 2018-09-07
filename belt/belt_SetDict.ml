module N = Belt_internalAVLset
module A = Belt_Array
type ('k,'id) t = 'k N.t
type ('key,'id) cmp = ('key,'id) Belt_Id.cmp
let rec add (t : _ t) x ~cmp  =
  (match N.toOpt t with
   | None  -> N.singleton x
   | Some nt ->
       let k = N.value nt in
       let c = (Belt_Id.getCmpInternal cmp) x k in
       if c = 0
       then t
       else
         (let (l,r) = let open N in ((left nt), (right nt)) in
          if c < 0
          then let ll = add ~cmp l x in (if ll == l then t else N.bal ll k r)
          else (let rr = add ~cmp r x in if rr == r then t else N.bal l k rr)) : 
  _ t)
let rec remove (t : _ t) x ~cmp  =
  (match N.toOpt t with
   | None  -> t
   | Some n ->
       let (l,v,r) = let open N in ((left n), (value n), (right n)) in
       let c = (Belt_Id.getCmpInternal cmp) x v in
       if c = 0
       then
         (match ((N.toOpt l), (N.toOpt r)) with
          | (None ,_) -> r
          | (_,None ) -> l
          | (_,Some rn) ->
              let v = ref (N.value rn) in
              let r = N.removeMinAuxWithRef rn v in N.bal l (!v) r)
       else
         if c < 0
         then
           (let ll = remove ~cmp l x in if ll == l then t else N.bal ll v r)
         else
           (let rr = remove ~cmp r x in if rr == r then t else N.bal l v rr) : 
  _ t)
let mergeMany h arr ~cmp  =
  let len = A.length arr in
  let v = ref h in
  for i = 0 to len - 1 do
    (let key = A.getUnsafe arr i in v := (add (!v) ~cmp key))
  done;
  !v
let removeMany h arr ~cmp  =
  let len = A.length arr in
  let v = ref h in
  for i = 0 to len - 1 do
    (let key = A.getUnsafe arr i in v := (remove (!v) ~cmp key))
  done;
  !v
let rec splitAuxNoPivot ~cmp  (n : _ N.node) x =
  (let (l,v,r) = let open N in ((left n), (value n), (right n)) in
   let c = (Belt_Id.getCmpInternal cmp) x v in
   if c = 0
   then (l, r)
   else
     if c < 0
     then
       (match N.toOpt l with
        | None  -> (N.empty, (N.return n))
        | Some l ->
            let (ll,rl) = splitAuxNoPivot ~cmp l x in
            (ll, (N.joinShared rl v r)))
     else
       (match N.toOpt r with
        | None  -> ((N.return n), N.empty)
        | Some r ->
            let (lr,rr) = splitAuxNoPivot ~cmp r x in
            ((N.joinShared l v lr), rr)) : (_* _))
let rec splitAuxPivot ~cmp  (n : _ N.node) x pres =
  (let (l,v,r) = let open N in ((left n), (value n), (right n)) in
   let c = (Belt_Id.getCmpInternal cmp) x v in
   if c = 0
   then (pres := true; (l, r))
   else
     if c < 0
     then
       (match N.toOpt l with
        | None  -> (N.empty, (N.return n))
        | Some l ->
            let (ll,rl) = splitAuxPivot ~cmp l x pres in
            (ll, (N.joinShared rl v r)))
     else
       (match N.toOpt r with
        | None  -> ((N.return n), N.empty)
        | Some r ->
            let (lr,rr) = splitAuxPivot ~cmp r x pres in
            ((N.joinShared l v lr), rr)) : (_* _))
let split (t : _ t) x ~cmp  =
  match N.toOpt t with
  | None  -> ((N.empty, N.empty), false)
  | Some n ->
      let pres = ref false in
      let v = splitAuxPivot ~cmp n x pres in (v, (!pres))
let rec union (s1 : _ t) (s2 : _ t) ~cmp  =
  (match let open N in ((toOpt s1), (toOpt s2)) with
   | (None ,_) -> s2
   | (_,None ) -> s1
   | (Some n1,Some n2) ->
       let (h1,h2) = let open N in ((height n1), (height n2)) in
       if h1 >= h2
       then
         (if h2 = 1
          then add ~cmp s1 (N.value n2)
          else
            (let (l1,v1,r1) =
               let open N in ((left n1), (value n1), (right n1)) in
             let (l2,r2) = splitAuxNoPivot ~cmp n2 v1 in
             N.joinShared (union ~cmp l1 l2) v1 (union ~cmp r1 r2)))
       else
         if h1 = 1
         then add s2 ~cmp (N.value n1)
         else
           (let (l2,v2,r2) =
              let open N in ((left n2), (value n2), (right n2)) in
            let (l1,r1) = splitAuxNoPivot ~cmp n1 v2 in
            N.joinShared (union ~cmp l1 l2) v2 (union ~cmp r1 r2)) : 
  _ t)
let rec intersect (s1 : _ t) (s2 : _ t) ~cmp  =
  match let open N in ((toOpt s1), (toOpt s2)) with
  | (None ,_)|(_,None ) -> N.empty
  | (Some n1,Some n2) ->
      let (l1,v1,r1) = let open N in ((left n1), (value n1), (right n1)) in
      let pres = ref false in
      let (l2,r2) = splitAuxPivot ~cmp n2 v1 pres in
      let ll = intersect ~cmp l1 l2 in
      let rr = intersect ~cmp r1 r2 in
      if !pres then N.joinShared ll v1 rr else N.concatShared ll rr
let rec diff s1 s2 ~cmp  =
  match let open N in ((toOpt s1), (toOpt s2)) with
  | (None ,_)|(_,None ) -> s1
  | (Some n1,Some n2) ->
      let (l1,v1,r1) = let open N in ((left n1), (value n1), (right n1)) in
      let pres = ref false in
      let (l2,r2) = splitAuxPivot ~cmp n2 v1 pres in
      let ll = diff ~cmp l1 l2 in
      let rr = diff ~cmp r1 r2 in
      if !pres then N.concatShared ll rr else N.joinShared ll v1 rr
let empty = N.empty
let fromArray = N.fromArray
let isEmpty = N.isEmpty
let cmp = N.cmp
let eq = N.eq
let has = N.has
let forEachU = N.forEachU
let forEach = N.forEach
let reduceU = N.reduceU
let reduce = N.reduce
let everyU = N.everyU
let every = N.every
let someU = N.someU
let some = N.some
let size = N.size
let toList = N.toList
let toArray = N.toArray
let minimum = N.minimum
let maximum = N.maximum
let maxUndefined = N.maxUndefined
let minUndefined = N.minUndefined
let get = N.get
let getExn = N.getExn
let getUndefined = N.getUndefined
let fromSortedArrayUnsafe = N.fromSortedArrayUnsafe
let subset = N.subset
let keep = N.keepShared
let keepU = N.keepSharedU
let partitionU = N.partitionSharedU
let partition = N.partitionShared
let checkInvariantInternal = N.checkInvariantInternal