include
  (struct
     type 'value node =
       {
       mutable value: 'value;
       mutable height: int;
       mutable left: 'value t;
       mutable right: 'value t;}
     and 'value t = 'value node Js.null
     let node:
       value:'value ->
         height:int -> left:'value t -> right:'value t -> 'value node
       =
       fun ~value  ->
         fun ~height  ->
           fun ~left  -> fun ~right  -> { value; height; left; right }
     let valueSet: 'value node -> 'value -> unit =
       fun o  -> fun v  -> o.value <- v
     let value: 'value node -> 'value = fun o  -> o.value
     let heightSet: 'value node -> int -> unit =
       fun o  -> fun v  -> o.height <- v
     let height: 'value node -> int = fun o  -> o.height
     let leftSet: 'value node -> 'value t -> unit =
       fun o  -> fun v  -> o.left <- v
     let left: 'value node -> 'value t = fun o  -> o.left
     let rightSet: 'value node -> 'value t -> unit =
       fun o  -> fun v  -> o.right <- v
     let right: 'value node -> 'value t = fun o  -> o.right
   end :
    sig
      type 'value node
      and 'value t = 'value node Js.null
      val node :
        value:'value ->
          height:int -> left:'value t -> right:'value t -> 'value node
      val valueSet : 'value node -> 'value -> unit
      val value : 'value node -> 'value
      val heightSet : 'value node -> int -> unit
      val height : 'value node -> int
      val leftSet : 'value node -> 'value t -> unit
      val left : 'value node -> 'value t
      val rightSet : 'value node -> 'value t -> unit
      val right : 'value node -> 'value t
    end)
module A = Belt_Array
module S = Belt_SortArray
let toOpt = Js.toOpt
let return: 'a -> 'a Js.null = Js.Null.return
let empty = Js.empty
let unsafeCoerce: 'a Js.null -> 'a = Js.Null.getUnsafe
type ('a,'b) cmp = ('a,'b) Belt_Id.cmp
let treeHeight (n : _ t) =
  match toOpt n with | None  -> 0 | Some n -> height n
let rec copy n =
  match toOpt n with
  | None  -> n
  | Some n ->
      let (l,r) = ((left n), (right n)) in
      return @@
        (node ~left:(copy l) ~right:(copy r) ~value:(value n)
           ~height:(height n))
let create (l : _ t) v (r : _ t) =
  let hl = match toOpt l with | None  -> 0 | Some n -> height n in
  let hr = match toOpt r with | None  -> 0 | Some n -> height n in
  return @@
    (node ~left:l ~value:v ~right:r
       ~height:(if hl >= hr then hl + 1 else hr + 1))
let singleton x =
  return @@ (node ~left:empty ~value:x ~right:empty ~height:1)
let heightGe l r =
  match ((toOpt l), (toOpt r)) with
  | (_,None ) -> true
  | (Some hl,Some hr) -> (height hl) >= (height hr)
  | (None ,Some _) -> false
let bal l v r =
  let hl = match toOpt l with | None  -> 0 | Some n -> height n in
  let hr = match toOpt r with | None  -> 0 | Some n -> height n in
  if hl > (hr + 2)
  then
    let (ll,lv,lr) =
      let __ocaml_internal_obj = unsafeCoerce l in
      ((left __ocaml_internal_obj), (value __ocaml_internal_obj),
        (right __ocaml_internal_obj)) in
    (if heightGe ll lr
     then create ll lv (create lr v r)
     else
       (let (lrl,lrv,lrr) =
          let __ocaml_internal_obj = unsafeCoerce lr in
          ((left __ocaml_internal_obj), (value __ocaml_internal_obj),
            (right __ocaml_internal_obj)) in
        create (create ll lv lrl) lrv (create lrr v r)))
  else
    if hr > (hl + 2)
    then
      (let (rl,rv,rr) =
         let __ocaml_internal_obj = unsafeCoerce r in
         ((left __ocaml_internal_obj), (value __ocaml_internal_obj),
           (right __ocaml_internal_obj)) in
       if heightGe rr rl
       then create (create l v rl) rv rr
       else
         (let (rll,rlv,rlr) =
            let __ocaml_internal_obj = unsafeCoerce rl in
            ((left __ocaml_internal_obj), (value __ocaml_internal_obj),
              (right __ocaml_internal_obj)) in
          create (create l v rll) rlv (create rlr rv rr)))
    else
      return @@
        (node ~left:l ~value:v ~right:r
           ~height:(if hl >= hr then hl + 1 else hr + 1))
let rec min0Aux n =
  match toOpt (left n) with | None  -> value n | Some n -> min0Aux n
let minimum n =
  match toOpt n with | None  -> None | Some n -> Some (min0Aux n)
let minUndefined n =
  match toOpt n with
  | None  -> Js.undefined
  | Some n -> Js.Undefined.return (min0Aux n)
let rec max0Aux n =
  match toOpt (right n) with | None  -> value n | Some n -> max0Aux n
let maximum n =
  match toOpt n with | None  -> None | Some n -> Some (max0Aux n)
let maxUndefined n =
  match toOpt n with
  | None  -> Js.undefined
  | Some n -> Js.Undefined.return (max0Aux n)
let rec removeMinAuxWithRef n v =
  let (ln,rn,kn) = ((left n), (right n), (value n)) in
  match toOpt ln with
  | None  -> (v := kn; rn)
  | Some ln -> bal (removeMinAuxWithRef ln v) kn rn
let isEmpty n = match toOpt n with | Some _ -> false | None  -> true
let rec stackAllLeft v s =
  match toOpt v with | None  -> s | Some x -> stackAllLeft (left x) (x :: s)
let rec forEachU n f =
  match toOpt n with
  | None  -> ()
  | Some n -> (forEachU (left n) f; f (value n); forEachU (right n) f)
let forEach n f = forEachU n (fun a  -> f a)
let rec reduceU s accu f =
  match toOpt s with
  | None  -> accu
  | Some n ->
      let (l,k,r) = ((left n), (value n), (right n)) in
      reduceU r (f (reduceU l accu f) k) f
let reduce s accu f = reduceU s accu (fun a  -> fun b  -> f a b)
let rec everyU n p =
  match toOpt n with
  | None  -> true
  | Some n -> (p (value n)) && ((everyU (left n) p) && (everyU (right n) p))
let every n p = everyU n (fun a  -> p a)
let rec someU n p =
  match toOpt n with
  | None  -> false
  | Some n -> (p (value n)) || ((someU (left n) p) || (someU (right n) p))
let some n p = someU n (fun a  -> p a)
let rec addMinElement n v =
  match toOpt n with
  | None  -> singleton v
  | Some n -> bal (addMinElement (left n) v) (value n) (right n)
let rec addMaxElement n v =
  match toOpt n with
  | None  -> singleton v
  | Some n -> bal (left n) (value n) (addMaxElement (right n) v)
let rec joinShared ln v rn =
  match ((toOpt ln), (toOpt rn)) with
  | (None ,_) -> addMinElement rn v
  | (_,None ) -> addMaxElement ln v
  | (Some l,Some r) ->
      let lh = height l in
      let rh = height r in
      if lh > (rh + 2)
      then bal (left l) (value l) (joinShared (right l) v rn)
      else
        if rh > (lh + 2)
        then bal (joinShared ln v (left r)) (value r) (right r)
        else create ln v rn
let concatShared t1 t2 =
  match ((toOpt t1), (toOpt t2)) with
  | (None ,_) -> t2
  | (_,None ) -> t1
  | (_,Some t2n) ->
      let v = ref (value t2n) in
      let t2r = removeMinAuxWithRef t2n v in joinShared t1 (!v) t2r
let rec partitionSharedU n p =
  match toOpt n with
  | None  -> (empty, empty)
  | Some n ->
      let value = value n in
      let (lt,lf) = partitionSharedU (left n) p in
      let pv = p value in
      let (rt,rf) = partitionSharedU (right n) p in
      if pv
      then ((joinShared lt value rt), (concatShared lf rf))
      else ((concatShared lt rt), (joinShared lf value rf))
let partitionShared n p = partitionSharedU n (fun a  -> p a)
let rec lengthNode n =
  let (l,r) = ((left n), (right n)) in
  let sizeL = match toOpt l with | None  -> 0 | Some l -> lengthNode l in
  let sizeR = match toOpt r with | None  -> 0 | Some r -> lengthNode r in
  (1 + sizeL) + sizeR
let size n = match toOpt n with | None  -> 0 | Some n -> lengthNode n
let rec toListAux n accu =
  match toOpt n with
  | None  -> accu
  | Some n -> toListAux (left n) ((value n) :: (toListAux (right n) accu))
let toList s = toListAux s []
let rec checkInvariantInternal (v : _ t) =
  match toOpt v with
  | None  -> ()
  | Some n ->
      let (l,r) = ((left n), (right n)) in
      let diff = (treeHeight l) - (treeHeight r) in
      (if Pervasives.not ((diff <= 2) && (diff >= (-2)))
       then Js.Exn.raiseError "File \"\", line 306, characters 6-12";
       checkInvariantInternal l;
       checkInvariantInternal r)
let rec fillArray n i arr =
  let (l,v,r) = ((left n), (value n), (right n)) in
  let next = match toOpt l with | None  -> i | Some l -> fillArray l i arr in
  A.setUnsafe arr next v;
  (let rnext = next + 1 in
   match toOpt r with | None  -> rnext | Some r -> fillArray r rnext arr)
include
  (struct
     type cursor = {
       mutable forward: int;
       mutable backward: int;}
     let cursor: forward:int -> backward:int -> cursor =
       fun ~forward  -> fun ~backward  -> { forward; backward }
     let forwardSet: cursor -> int -> unit =
       fun o  -> fun v  -> o.forward <- v
     let forward: cursor -> int = fun o  -> o.forward
     let backwardSet: cursor -> int -> unit =
       fun o  -> fun v  -> o.backward <- v
     let backward: cursor -> int = fun o  -> o.backward
   end :
    sig
      type cursor
      val cursor : forward:int -> backward:int -> cursor
      val forwardSet : cursor -> int -> unit
      val forward : cursor -> int
      val backwardSet : cursor -> int -> unit
      val backward : cursor -> int
    end)
let rec fillArrayWithPartition n cursor arr p =
  let (l,v,r) = ((left n), (value n), (right n)) in
  (match toOpt l with
   | None  -> ()
   | Some l -> fillArrayWithPartition l cursor arr p);
  if p v
  then
    (let c = forward cursor in A.setUnsafe arr c v; forwardSet cursor (c + 1))
  else
    (let c = backward cursor in
     A.setUnsafe arr c v; backwardSet cursor (c - 1));
  (match toOpt r with
   | None  -> ()
   | Some r -> fillArrayWithPartition r cursor arr p)
let rec fillArrayWithFilter n i arr p =
  let (l,v,r) = ((left n), (value n), (right n)) in
  let next =
    match toOpt l with | None  -> i | Some l -> fillArrayWithFilter l i arr p in
  let rnext = if p v then (A.setUnsafe arr next v; next + 1) else next in
  match toOpt r with
  | None  -> rnext
  | Some r -> fillArrayWithFilter r rnext arr p
let toArray n =
  match toOpt n with
  | None  -> [||]
  | Some n ->
      let size = lengthNode n in
      let v = A.makeUninitializedUnsafe size (value n) in
      (ignore (fillArray n 0 v : int); v)
let rec fromSortedArrayRevAux arr off len =
  match len with
  | 0 -> empty
  | 1 -> singleton (A.getUnsafe arr off)
  | 2 ->
      let (x0,x1) =
        let open A in ((getUnsafe arr off), (getUnsafe arr (off - 1))) in
      return @@ (node ~left:(singleton x0) ~value:x1 ~height:2 ~right:empty)
  | 3 ->
      let (x0,x1,x2) =
        let open A in
          ((getUnsafe arr off), (getUnsafe arr (off - 1)),
            (getUnsafe arr (off - 2))) in
      return @@
        (node ~left:(singleton x0) ~right:(singleton x2) ~value:x1 ~height:2)
  | _ ->
      let nl = len / 2 in
      let left = fromSortedArrayRevAux arr off nl in
      let mid = A.getUnsafe arr (off - nl) in
      let right = fromSortedArrayRevAux arr ((off - nl) - 1) ((len - nl) - 1) in
      create left mid right
let rec fromSortedArrayAux arr off len =
  match len with
  | 0 -> empty
  | 1 -> singleton (A.getUnsafe arr off)
  | 2 ->
      let (x0,x1) =
        let open A in ((getUnsafe arr off), (getUnsafe arr (off + 1))) in
      return @@ (node ~left:(singleton x0) ~value:x1 ~height:2 ~right:empty)
  | 3 ->
      let (x0,x1,x2) =
        let open A in
          ((getUnsafe arr off), (getUnsafe arr (off + 1)),
            (getUnsafe arr (off + 2))) in
      return @@
        (node ~left:(singleton x0) ~right:(singleton x2) ~value:x1 ~height:2)
  | _ ->
      let nl = len / 2 in
      let left = fromSortedArrayAux arr off nl in
      let mid = A.getUnsafe arr (off + nl) in
      let right = fromSortedArrayAux arr ((off + nl) + 1) ((len - nl) - 1) in
      create left mid right
let fromSortedArrayUnsafe arr = fromSortedArrayAux arr 0 (A.length arr)
let rec keepSharedU n p =
  match toOpt n with
  | None  -> empty
  | Some n ->
      let (l,v,r) = ((left n), (value n), (right n)) in
      let newL = keepSharedU l p in
      let pv = p v in
      let newR = keepSharedU r p in
      if pv
      then
        (if (l == newL) && (r == newR)
         then return n
         else joinShared newL v newR)
      else concatShared newL newR
let keepShared n p = keepSharedU n (fun a  -> p a)
let keepCopyU n p =
  (match toOpt n with
   | None  -> empty
   | Some n ->
       let size = lengthNode n in
       let v = A.makeUninitializedUnsafe size (value n) in
       let last = fillArrayWithFilter n 0 v p in fromSortedArrayAux v 0 last : 
  _ t)
let keepCopy n p = keepCopyU n (fun x  -> p x)
let partitionCopyU n p =
  match toOpt n with
  | None  -> (empty, empty)
  | Some n ->
      let size = lengthNode n in
      let v = A.makeUninitializedUnsafe size (value n) in
      let backward = size - 1 in
      let cursor = cursor ~forward:0 ~backward in
      (fillArrayWithPartition n cursor v p;
       (let forwardLen = forward cursor in
        ((fromSortedArrayAux v 0 forwardLen),
          (fromSortedArrayRevAux v backward (size - forwardLen)))))
let partitionCopy n p = partitionCopyU n (fun a  -> p a)
let rec has (t : _ t) x ~cmp  =
  match toOpt t with
  | None  -> false
  | Some n ->
      let v = value n in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      (c = 0) || (has ~cmp (if c < 0 then left n else right n) x)
let rec compareAux e1 e2 ~cmp  =
  match (e1, e2) with
  | (h1::t1,h2::t2) ->
      let c = (Belt_Id.getCmpInternal cmp) (value h1) (value h2) in
      if c = 0
      then
        compareAux ~cmp (stackAllLeft (right h1) t1)
          (stackAllLeft (right h2) t2)
      else c
  | (_,_) -> 0
let cmp s1 s2 ~cmp  =
  let (len1,len2) = ((size s1), (size s2)) in
  if len1 = len2
  then compareAux ~cmp (stackAllLeft s1 []) (stackAllLeft s2 [])
  else if len1 < len2 then (-1) else 1
let eq s1 s2 ~cmp:c  = (cmp ~cmp:c s1 s2) = 0
let rec subset (s1 : _ t) (s2 : _ t) ~cmp  =
  match ((toOpt s1), (toOpt s2)) with
  | (None ,_) -> true
  | (_,None ) -> false
  | (Some t1,Some t2) ->
      let (l1,v1,r1) = ((left t1), (value t1), (right t1)) in
      let (l2,v2,r2) = ((left t2), (value t2), (right t2)) in
      let c = (Belt_Id.getCmpInternal cmp) v1 v2 in
      if c = 0
      then (subset ~cmp l1 l2) && (subset ~cmp r1 r2)
      else
        if c < 0
        then (subset ~cmp (create l1 v1 empty) l2) && (subset ~cmp r1 s2)
        else (subset ~cmp (create empty v1 r1) r2) && (subset ~cmp l1 s2)
let rec get (n : _ t) x ~cmp  =
  match toOpt n with
  | None  -> None
  | Some t ->
      let v = value t in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0
      then Some v
      else get ~cmp (if c < 0 then left t else right t) x
let rec getUndefined (n : _ t) x ~cmp  =
  match toOpt n with
  | None  -> Js.Undefined.empty
  | Some t ->
      let v = value t in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0
      then Js.Undefined.return v
      else getUndefined ~cmp (if c < 0 then left t else right t) x
let rec getExn (n : _ t) x ~cmp  =
  match toOpt n with
  | None  -> Js.Exn.raiseError "File \"\", line 548, characters 14-20"
  | Some t ->
      let v = value t in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0 then v else getExn ~cmp (if c < 0 then left t else right t) x
let rotateWithLeftChild k2 =
  let k1 = unsafeCoerce (left k2) in
  leftSet k2 (right k1);
  rightSet k1 (return k2);
  (let (hlk2,hrk2) = ((treeHeight (left k2)), (treeHeight (right k2))) in
   heightSet k2 ((Pervasives.max hlk2 hrk2) + 1);
   (let (hlk1,hk2) = ((treeHeight (left k1)), (height k2)) in
    heightSet k1 ((Pervasives.max hlk1 hk2) + 1); k1))
let rotateWithRightChild k1 =
  let k2 = unsafeCoerce (right k1) in
  rightSet k1 (left k2);
  leftSet k2 (return k1);
  (let (hlk1,hrk1) = ((treeHeight (left k1)), (treeHeight (right k1))) in
   heightSet k1 ((Pervasives.max hlk1 hrk1) + 1);
   (let (hrk2,hk1) = ((treeHeight (right k2)), (height k1)) in
    heightSet k2 ((Pervasives.max hrk2 hk1) + 1); k2))
let doubleWithLeftChild k3 =
  let v = return (rotateWithRightChild (unsafeCoerce (left k3))) in
  leftSet k3 v; rotateWithLeftChild k3[@@ocaml.doc " "]
let doubleWithRightChild k2 =
  let v = return (rotateWithLeftChild (unsafeCoerce (right k2))) in
  rightSet k2 v; rotateWithRightChild k2
let heightUpdateMutate t =
  let (hlt,hrt) = ((treeHeight (left t)), (treeHeight (right t))) in
  heightSet t ((Pervasives.max hlt hrt) + 1); t
let balMutate nt =
  let (l,r) = ((left nt), (right nt)) in
  let (hl,hr) = ((treeHeight l), (treeHeight r)) in
  if hl > (2 + hr)
  then
    let (ll,lr) =
      let __ocaml_internal_obj = unsafeCoerce l in
      ((left __ocaml_internal_obj), (right __ocaml_internal_obj)) in
    (if heightGe ll lr
     then heightUpdateMutate (rotateWithLeftChild nt)
     else heightUpdateMutate (doubleWithLeftChild nt))
  else
    if hr > (2 + hl)
    then
      (let (rl,rr) =
         let __ocaml_internal_obj = unsafeCoerce r in
         ((left __ocaml_internal_obj), (right __ocaml_internal_obj)) in
       if heightGe rr rl
       then heightUpdateMutate (rotateWithRightChild nt)
       else heightUpdateMutate (doubleWithRightChild nt))
    else (heightSet nt ((max hl hr) + 1); nt)
let rec addMutate ~cmp  (t : _ t) x =
  match toOpt t with
  | None  -> singleton x
  | Some nt ->
      let k = value nt in
      let c = (Belt_Id.getCmpInternal cmp) x k in
      if c = 0
      then t
      else
        (let (l,r) = ((left nt), (right nt)) in
         if c < 0
         then (let ll = addMutate ~cmp l x in leftSet nt ll)
         else rightSet nt (addMutate ~cmp r x);
         return (balMutate nt))
let fromArray (xs : _ array) ~cmp  =
  let len = A.length xs in
  if len = 0
  then empty
  else
    (let next =
       ref
         (S.strictlySortedLengthU xs
            (fun x  -> fun y  -> ((Belt_Id.getCmpInternal cmp) x y) < 0)) in
     let result =
       ref
         (if (!next) >= 0
          then fromSortedArrayAux xs 0 (!next)
          else
            (next := (- (!next));
             fromSortedArrayRevAux xs ((!next) - 1) (!next))) in
     for i = !next to len - 1 do
       result := (addMutate ~cmp (!result) (A.getUnsafe xs i))
     done;
     !result)
let rec removeMinAuxWithRootMutate nt n =
  let (rn,ln) = ((right n), (left n)) in
  match toOpt ln with
  | None  -> (valueSet nt (value n); rn)
  | Some ln ->
      (leftSet n (removeMinAuxWithRootMutate nt ln); return (balMutate n))