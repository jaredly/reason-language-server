module Int = Belt_MutableMapInt
module String = Belt_MutableMapString
module N = Belt_internalAVLtree
module A = Belt_Array
type ('key,'id) id = ('key,'id) Belt_Id.comparable
type ('key,'id) cmp = ('key,'id) Belt_Id.cmp
module S =
  struct
    include
      (struct
         type ('k,'v,'id) t = {
           cmp: ('k,'id) cmp;
           mutable data: ('k,'v) N.t;}
         let t: cmp:('k,'id) cmp -> data:('k,'v) N.t -> ('k,'v,'id) t =
           fun ~cmp  -> fun ~data  -> { cmp; data }
         let cmp: ('k,'v,'id) t -> ('k,'id) cmp = fun o  -> o.cmp
         let dataSet: ('k,'v,'id) t -> ('k,'v) N.t -> unit =
           fun o  -> fun v  -> o.data <- v
         let data: ('k,'v,'id) t -> ('k,'v) N.t = fun o  -> o.data
       end :
        sig
          type ('k,'v,'id) t
          val t : cmp:('k,'id) cmp -> data:('k,'v) N.t -> ('k,'v,'id) t
          val cmp : ('k,'v,'id) t -> ('k,'id) cmp
          val dataSet : ('k,'v,'id) t -> ('k,'v) N.t -> unit
          val data : ('k,'v,'id) t -> ('k,'v) N.t
        end)
  end
type ('k,'v,'id) t = ('k,'v,'id) S.t
let rec removeMutateAux nt x ~cmp  =
  let k = N.key nt in
  let c = (Belt_Id.getCmpInternal cmp) x k in
  if c = 0
  then
    let (l,r) = let open N in ((left nt), (right nt)) in
    match let open N in ((toOpt l), (toOpt r)) with
    | (Some _,Some nr) ->
        (N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
         N.return (N.balMutate nt))
    | (None ,Some _) -> r
    | ((Some _|None ),None ) -> l
  else
    if c < 0
    then
      (match N.toOpt (N.left nt) with
       | None  -> N.return nt
       | Some l ->
           (N.leftSet nt (removeMutateAux ~cmp l x);
            N.return (N.balMutate nt)))
    else
      (match N.toOpt (N.right nt) with
       | None  -> N.return nt
       | Some r ->
           (N.rightSet nt (removeMutateAux ~cmp r x);
            N.return (N.balMutate nt)))
let remove d k =
  let oldRoot = S.data d in
  match N.toOpt oldRoot with
  | None  -> ()
  | Some oldRoot2 ->
      let newRoot = removeMutateAux ~cmp:(S.cmp d) oldRoot2 k in
      if newRoot != oldRoot then S.dataSet d newRoot
let rec removeArrayMutateAux t xs i len ~cmp  =
  if i < len
  then
    let ele = A.getUnsafe xs i in
    let u = removeMutateAux t ele ~cmp in
    match N.toOpt u with
    | None  -> N.empty
    | Some t -> removeArrayMutateAux t xs (i + 1) len ~cmp
  else N.return t
let removeMany d xs =
  let oldRoot = S.data d in
  match N.toOpt oldRoot with
  | None  -> ()
  | Some nt ->
      let len = A.length xs in
      let newRoot = removeArrayMutateAux nt xs 0 len ~cmp:(S.cmp d) in
      if newRoot != oldRoot then S.dataSet d newRoot
let rec updateDone t x f ~cmp  =
  match N.toOpt t with
  | None  ->
      (match f None with | Some data -> N.singleton x data | None  -> t)
  | Some nt ->
      let k = N.key nt in
      let c = (Belt_Id.getCmpInternal cmp) x k in
      if c = 0
      then
        (match f (Some (N.value nt)) with
         | None  ->
             let (l,r) = ((N.left nt), (N.right nt)) in
             (match ((N.toOpt l), (N.toOpt r)) with
              | (Some _,Some nr) ->
                  (N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
                   N.return (N.balMutate nt))
              | (None ,Some _) -> r
              | ((Some _|None ),None ) -> l)
         | Some data -> (N.valueSet nt data; N.return nt))
      else
        (let (l,r) = let open N in ((left nt), (right nt)) in
         if c < 0
         then (let ll = updateDone l x f ~cmp in N.leftSet nt ll)
         else N.rightSet nt (updateDone r x f ~cmp);
         N.return (N.balMutate nt))
let updateU t x f =
  let oldRoot = S.data t in
  let newRoot = updateDone oldRoot x f ~cmp:(S.cmp t) in
  if newRoot != oldRoot then S.dataSet t newRoot
let update t x f = updateU t x (fun a  -> f a)
let make (type key) (type identity) ~id:(id : (key,identity) id)  =
  let module M = (val id) in S.t ~cmp:M.cmp ~data:N.empty
let clear m = S.dataSet m N.empty
let isEmpty d = N.isEmpty (S.data d)
let minKey m = N.minKey (S.data m)
let minKeyUndefined m = N.minKeyUndefined (S.data m)
let maxKey m = N.maxKey (S.data m)
let maxKeyUndefined m = N.maxKeyUndefined (S.data m)
let minimum m = N.minimum (S.data m)
let minUndefined m = N.minUndefined (S.data m)
let maximum m = N.maximum (S.data m)
let maxUndefined m = N.maxUndefined (S.data m)
let forEachU d f = N.forEachU (S.data d) f
let forEach d f = forEachU d (fun a  -> fun b  -> f a b)
let reduceU d acc cb = N.reduceU (S.data d) acc cb
let reduce d acc cb = reduceU d acc (fun a  -> fun b  -> fun c  -> cb a b c)
let everyU d p = N.everyU (S.data d) p
let every d p = everyU d (fun a  -> fun b  -> p a b)
let someU d p = N.someU (S.data d) p
let some d p = someU d (fun a  -> fun b  -> p a b)
let size d = N.size (S.data d)
let toList d = N.toList (S.data d)
let toArray d = N.toArray (S.data d)
let keysToArray d = N.keysToArray (S.data d)
let valuesToArray d = N.valuesToArray (S.data d)
let fromSortedArrayUnsafe (type key) (type identity)
  ~id:(id : (key,identity) id)  xs =
  (let module M = (val id) in
     S.t ~data:(N.fromSortedArrayUnsafe xs) ~cmp:M.cmp : _ t)
let checkInvariantInternal d = N.checkInvariantInternal (S.data d)
let cmpU m1 m2 cmp =
  N.cmpU ~kcmp:(S.cmp m1) ~vcmp:cmp (S.data m1) (S.data m2)
let cmp m1 m2 cmp = cmpU m1 m2 (fun a  -> fun b  -> cmp a b)
let eqU m1 m2 cmp = N.eqU ~kcmp:(S.cmp m1) ~veq:cmp (S.data m1) (S.data m2)
let eq m1 m2 cmp = eqU m1 m2 (fun a  -> fun b  -> cmp a b)
let mapU m f = S.t ~cmp:(S.cmp m) ~data:(N.mapU (S.data m) f)
let map m f = mapU m (fun a  -> f a)
let mapWithKeyU m f = S.t ~cmp:(S.cmp m) ~data:(N.mapWithKeyU (S.data m) f)
let mapWithKey m f = mapWithKeyU m (fun a  -> fun b  -> f a b)
let get m x = N.get ~cmp:(S.cmp m) (S.data m) x
let getUndefined m x = N.getUndefined ~cmp:(S.cmp m) (S.data m) x
let getWithDefault m x def = N.getWithDefault ~cmp:(S.cmp m) (S.data m) x def
let getExn m x = N.getExn ~cmp:(S.cmp m) (S.data m) x
let has m x = N.has ~cmp:(S.cmp m) (S.data m) x
let fromArray (type k) (type identity) data ~id:(id : (k,identity) id)  =
  let module M = (val id) in
    let cmp = M.cmp in S.t ~cmp ~data:(N.fromArray ~cmp data)
let set m e v =
  let oldRoot = S.data m in
  let newRoot = N.updateMutate ~cmp:(S.cmp m) oldRoot e v in
  if newRoot != oldRoot then S.dataSet m newRoot
let mergeManyAux t xs ~cmp  =
  let v = ref t in
  for i = 0 to (A.length xs) - 1 do
    (let (key,value) = A.getUnsafe xs i in
     v := (N.updateMutate (!v) key value ~cmp))
  done;
  !v
let mergeMany d xs =
  let oldRoot = S.data d in
  let newRoot = mergeManyAux oldRoot xs ~cmp:(S.cmp d) in
  if newRoot != oldRoot then S.dataSet d newRoot