[@@@ocaml.text
  " Almost rewritten  by authors of BuckleScript                       "]
include
  (struct
     type ('k,'v) node =
       {
       mutable key: 'k;
       mutable value: 'v;
       mutable height: int;
       mutable left: ('k,'v) t;
       mutable right: ('k,'v) t;}
     and ('key,'a) t = ('key,'a) node Js.null
     let node:
       key:'k ->
         value:'v ->
           height:int -> left:('k,'v) t -> right:('k,'v) t -> ('k,'v) node
       =
       fun ~key  ->
         fun ~value  ->
           fun ~height  ->
             fun ~left  -> fun ~right  -> { key; value; height; left; right }
     let keySet: ('k,'v) node -> 'k -> unit = fun o  -> fun v  -> o.key <- v
     let key: ('k,'v) node -> 'k = fun o  -> o.key
     let valueSet: ('k,'v) node -> 'v -> unit =
       fun o  -> fun v  -> o.value <- v
     let value: ('k,'v) node -> 'v = fun o  -> o.value
     let heightSet: ('k,'v) node -> int -> unit =
       fun o  -> fun v  -> o.height <- v
     let height: ('k,'v) node -> int = fun o  -> o.height
     let leftSet: ('k,'v) node -> ('k,'v) t -> unit =
       fun o  -> fun v  -> o.left <- v
     let left: ('k,'v) node -> ('k,'v) t = fun o  -> o.left
     let rightSet: ('k,'v) node -> ('k,'v) t -> unit =
       fun o  -> fun v  -> o.right <- v
     let right: ('k,'v) node -> ('k,'v) t = fun o  -> o.right
   end :
    sig
      type ('k,'v) node
      and ('key,'a) t = ('key,'a) node Js.null
      val node :
        key:'k ->
          value:'v ->
            height:int -> left:('k,'v) t -> right:('k,'v) t -> ('k,'v) node
      val keySet : ('k,'v) node -> 'k -> unit
      val key : ('k,'v) node -> 'k
      val valueSet : ('k,'v) node -> 'v -> unit
      val value : ('k,'v) node -> 'v
      val heightSet : ('k,'v) node -> int -> unit
      val height : ('k,'v) node -> int
      val leftSet : ('k,'v) node -> ('k,'v) t -> unit
      val left : ('k,'v) node -> ('k,'v) t
      val rightSet : ('k,'v) node -> ('k,'v) t -> unit
      val right : ('k,'v) node -> ('k,'v) t
    end)
type ('k,'id) cmp = ('k,'id) Belt_Id.cmp
module A = Belt_Array
module S = Belt_SortArray
let toOpt: 'a Js.null -> 'a option = Js.toOpt
let return a = Js.Null.return a
let empty: 'a Js.null = Js.empty
let unsafeCoerce a = Js.Null.getUnsafe a
let treeHeight (n : _ t) =
  match toOpt n with | None  -> 0 | Some n -> height n
let rec copy n =
  match toOpt n with
  | None  -> n
  | Some n ->
      let (l,r) = ((left n), (right n)) in
      return @@
        (node ~left:(copy l) ~right:(copy r) ~value:(value n) ~key:(key n)
           ~height:(height n))
let create l x d r =
  let (hl,hr) = ((treeHeight l), (treeHeight r)) in
  return @@
    (node ~left:l ~key:x ~value:d ~right:r
       ~height:(if hl >= hr then hl + 1 else hr + 1))
let singleton x d =
  return @@ (node ~left:empty ~key:x ~value:d ~right:empty ~height:1)
let heightGe l r =
  match ((toOpt l), (toOpt r)) with
  | (_,None ) -> true
  | (Some hl,Some hr) -> (height hl) >= (height hr)
  | (None ,Some _) -> false
let updateValue n newValue =
  if (value n) == newValue
  then n
  else
    node ~left:(left n) ~right:(right n) ~key:(key n) ~value:newValue
      ~height:(height n)
let bal l x d r =
  let hl = match toOpt l with | None  -> 0 | Some n -> height n in
  let hr = match toOpt r with | None  -> 0 | Some n -> height n in
  if hl > (hr + 2)
  then
    let (ll,lv,ld,lr) =
      let __ocaml_internal_obj = unsafeCoerce l in
      ((left __ocaml_internal_obj), (key __ocaml_internal_obj),
        (value __ocaml_internal_obj), (right __ocaml_internal_obj)) in
    (if (treeHeight ll) >= (treeHeight lr)
     then create ll lv ld (create lr x d r)
     else
       (let (lrl,lrv,lrd,lrr) =
          let __ocaml_internal_obj = unsafeCoerce lr in
          ((left __ocaml_internal_obj), (key __ocaml_internal_obj),
            (value __ocaml_internal_obj), (right __ocaml_internal_obj)) in
        create (create ll lv ld lrl) lrv lrd (create lrr x d r)))
  else
    if hr > (hl + 2)
    then
      (let (rl,rv,rd,rr) =
         let __ocaml_internal_obj = unsafeCoerce r in
         ((left __ocaml_internal_obj), (key __ocaml_internal_obj),
           (value __ocaml_internal_obj), (right __ocaml_internal_obj)) in
       if (treeHeight rr) >= (treeHeight rl)
       then create (create l x d rl) rv rd rr
       else
         (let (rll,rlv,rld,rlr) =
            let __ocaml_internal_obj = unsafeCoerce rl in
            ((left __ocaml_internal_obj), (key __ocaml_internal_obj),
              (value __ocaml_internal_obj), (right __ocaml_internal_obj)) in
          create (create l x d rll) rlv rld (create rlr rv rd rr)))
    else
      return @@
        (node ~left:l ~key:x ~value:d ~right:r
           ~height:(if hl >= hr then hl + 1 else hr + 1))
let rec minKey0Aux n =
  match toOpt (left n) with | None  -> key n | Some n -> minKey0Aux n
let minKey n =
  match toOpt n with | None  -> None | Some n -> Some (minKey0Aux n)
let minKeyUndefined n =
  match toOpt n with
  | None  -> Js.undefined
  | Some n -> Js.Undefined.return (minKey0Aux n)
let rec maxKey0Aux n =
  match toOpt (right n) with | None  -> key n | Some n -> maxKey0Aux n
let maxKey n =
  match toOpt n with | None  -> None | Some n -> Some (maxKey0Aux n)
let maxKeyUndefined n =
  match toOpt n with
  | None  -> Js.undefined
  | Some n -> Js.Undefined.return (maxKey0Aux n)
let rec minKV0Aux n =
  match toOpt (left n) with
  | None  -> ((key n), (value n))
  | Some n -> minKV0Aux n
let minimum n =
  match toOpt n with | None  -> None | Some n -> Some (minKV0Aux n)
let minUndefined n =
  match toOpt n with
  | None  -> Js.undefined
  | Some n -> Js.Undefined.return (minKV0Aux n)
let rec maxKV0Aux n =
  match toOpt (right n) with
  | None  -> ((key n), (value n))
  | Some n -> maxKV0Aux n
let maximum n =
  match toOpt n with | None  -> None | Some n -> Some (maxKV0Aux n)
let maxUndefined n =
  match toOpt n with
  | None  -> Js.undefined
  | Some n -> Js.Undefined.return (maxKV0Aux n)
let rec removeMinAuxWithRef n kr vr =
  let (ln,rn,kn,vn) = ((left n), (right n), (key n), (value n)) in
  match toOpt ln with
  | None  -> (kr := kn; vr := vn; rn)
  | Some ln -> bal (removeMinAuxWithRef ln kr vr) kn vn rn
let isEmpty x = match toOpt x with | None  -> true | Some _ -> false
let rec stackAllLeft v s =
  match toOpt v with | None  -> s | Some x -> stackAllLeft (left x) (x :: s)
let rec forEachU n f =
  match toOpt n with
  | None  -> ()
  | Some n ->
      (forEachU (left n) f; f (key n) (value n); forEachU (right n) f)
let forEach n f = forEachU n (fun a  -> fun b  -> f a b)
let rec mapU n f =
  match toOpt n with
  | None  -> empty
  | Some n ->
      let newLeft = mapU (left n) f in
      let newD = f (value n) in
      let newRight = mapU (right n) f in
      return @@
        (node ~left:newLeft ~key:(key n) ~value:newD ~right:newRight
           ~height:(height n))
let map n f = mapU n (fun a  -> f a)
let rec mapWithKeyU n f =
  match toOpt n with
  | None  -> empty
  | Some n ->
      let key = key n in
      let newLeft = mapWithKeyU (left n) f in
      let newD = f key (value n) in
      let newRight = mapWithKeyU (right n) f in
      return @@
        (node ~left:newLeft ~key ~value:newD ~right:newRight
           ~height:(height n))
let mapWithKey n f = mapWithKeyU n (fun a  -> fun b  -> f a b)
let rec reduceU m accu f =
  match toOpt m with
  | None  -> accu
  | Some n ->
      let (l,v,d,r) = ((left n), (key n), (value n), (right n)) in
      reduceU r (f (reduceU l accu f) v d) f
let reduce m accu f = reduceU m accu (fun a  -> fun b  -> fun c  -> f a b c)
let rec everyU n p =
  match toOpt n with
  | None  -> true
  | Some n ->
      (p (key n) (value n)) && ((everyU (left n) p) && (everyU (right n) p))
let every n p = everyU n (fun a  -> fun b  -> p a b)
let rec someU n p =
  match toOpt n with
  | None  -> false
  | Some n ->
      (p (key n) (value n)) || ((someU (left n) p) || (someU (right n) p))
let some n p = someU n (fun a  -> fun b  -> p a b)
let rec addMinElement n k v =
  match toOpt n with
  | None  -> singleton k v
  | Some n -> bal (addMinElement (left n) k v) (key n) (value n) (right n)
let rec addMaxElement n k v =
  match toOpt n with
  | None  -> singleton k v
  | Some n -> bal (left n) (key n) (value n) (addMaxElement (right n) k v)
let rec join ln v d rn =
  match ((toOpt ln), (toOpt rn)) with
  | (None ,_) -> addMinElement rn v d
  | (_,None ) -> addMaxElement ln v d
  | (Some l,Some r) ->
      let (ll,lv,ld,lr,lh) =
        ((left l), (key l), (value l), (right l), (height l)) in
      let (rl,rv,rd,rr,rh) =
        ((left r), (key r), (value r), (right r), (height r)) in
      if lh > (rh + 2)
      then bal ll lv ld (join lr v d rn)
      else
        if rh > (lh + 2)
        then bal (join ln v d rl) rv rd rr
        else create ln v d rn
let concat t1 t2 =
  match ((toOpt t1), (toOpt t2)) with
  | (None ,_) -> t2
  | (_,None ) -> t1
  | (_,Some t2n) ->
      let (kr,vr) = ((ref (key t2n)), (ref (value t2n))) in
      let t2r = removeMinAuxWithRef t2n kr vr in join t1 (!kr) (!vr) t2r
let concatOrJoin t1 v d t2 =
  match d with | Some d -> join t1 v d t2 | None  -> concat t1 t2
let rec keepSharedU n p =
  match toOpt n with
  | None  -> empty
  | Some n ->
      let (v,d) = ((key n), (value n)) in
      let newLeft = keepSharedU (left n) p in
      let pvd = p v d in
      let newRight = keepSharedU (right n) p in
      if pvd then join newLeft v d newRight else concat newLeft newRight
let keepShared n p = keepSharedU n (fun a  -> fun b  -> p a b)
let rec keepMapU n p =
  match toOpt n with
  | None  -> empty
  | Some n ->
      let (v,d) = ((key n), (value n)) in
      let newLeft = keepMapU (left n) p in
      let pvd = p v d in
      let newRight = keepMapU (right n) p in
      (match pvd with
       | None  -> concat newLeft newRight
       | Some d -> join newLeft v d newRight)
let keepMap n p = keepMapU n (fun a  -> fun b  -> p a b)
let rec partitionSharedU n p =
  match toOpt n with
  | None  -> (empty, empty)
  | Some n ->
      let (key,value) = ((key n), (value n)) in
      let (lt,lf) = partitionSharedU (left n) p in
      let pvd = p key value in
      let (rt,rf) = partitionSharedU (right n) p in
      if pvd
      then ((join lt key value rt), (concat lf rf))
      else ((concat lt rt), (join lf key value rf))
let partitionShared n p = partitionSharedU n (fun a  -> fun b  -> p a b)
let rec lengthNode n =
  let (l,r) = ((left n), (right n)) in
  let sizeL = match toOpt l with | None  -> 0 | Some l -> lengthNode l in
  let sizeR = match toOpt r with | None  -> 0 | Some r -> lengthNode r in
  (1 + sizeL) + sizeR
let size n = match toOpt n with | None  -> 0 | Some n -> lengthNode n
let rec toListAux n accu =
  match toOpt n with
  | None  -> accu
  | Some n ->
      let (l,r,k,v) = ((left n), (right n), (key n), (value n)) in
      toListAux l ((k, v) :: (toListAux r accu))
let toList s = toListAux s []
let rec checkInvariantInternal (v : _ t) =
  match toOpt v with
  | None  -> ()
  | Some n ->
      let (l,r) = ((left n), (right n)) in
      let diff = (treeHeight l) - (treeHeight r) in
      (if Pervasives.not ((diff <= 2) && (diff >= (-2)))
       then Js.Exn.raiseError "File \"\", line 374, characters 6-12";
       checkInvariantInternal l;
       checkInvariantInternal r)
let rec fillArrayKey n i arr =
  let (l,v,r) = ((left n), (key n), (right n)) in
  let next = match toOpt l with | None  -> i | Some l -> fillArrayKey l i arr in
  A.setUnsafe arr next v;
  (let rnext = next + 1 in
   match toOpt r with | None  -> rnext | Some r -> fillArrayKey r rnext arr)
let rec fillArrayValue n i arr =
  let (l,r) = ((left n), (right n)) in
  let next =
    match toOpt l with | None  -> i | Some l -> fillArrayValue l i arr in
  A.setUnsafe arr next (value n);
  (let rnext = next + 1 in
   match toOpt r with | None  -> rnext | Some r -> fillArrayValue r rnext arr)
let rec fillArray n i arr =
  let (l,v,r) = ((left n), (key n), (right n)) in
  let next = match toOpt l with | None  -> i | Some l -> fillArray l i arr in
  A.setUnsafe arr next (v, (value n));
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
  let (l,v,r) = ((left n), (key n), (right n)) in
  (match toOpt l with
   | None  -> ()
   | Some l -> fillArrayWithPartition l cursor arr p);
  if p v
  then
    (let c = forward cursor in
     A.setUnsafe arr c (v, (value n)); forwardSet cursor (c + 1))
  else
    (let c = backward cursor in
     A.setUnsafe arr c (v, (value n)); backwardSet cursor (c - 1));
  (match toOpt r with
   | None  -> ()
   | Some r -> fillArrayWithPartition r cursor arr p)
let rec fillArrayWithFilter n i arr p =
  let (l,v,r) = ((left n), (key n), (right n)) in
  let next =
    match toOpt l with | None  -> i | Some l -> fillArrayWithFilter l i arr p in
  let rnext =
    if p v then (A.setUnsafe arr next (v, (value n)); next + 1) else next in
  match toOpt r with
  | None  -> rnext
  | Some r -> fillArrayWithFilter r rnext arr p
let toArray n =
  match toOpt n with
  | None  -> [||]
  | Some n ->
      let size = lengthNode n in
      let v = A.makeUninitializedUnsafe size ((key n), (value n)) in
      (ignore (fillArray n 0 v : int); v)
let keysToArray n =
  match toOpt n with
  | None  -> [||]
  | Some n ->
      let size = lengthNode n in
      let v = A.makeUninitializedUnsafe size (key n) in
      (ignore (fillArrayKey n 0 v : int); v)
let valuesToArray n =
  match toOpt n with
  | None  -> [||]
  | Some n ->
      let size = lengthNode n in
      let v = A.makeUninitializedUnsafe size (value n) in
      (ignore (fillArrayValue n 0 v : int); v)
let rec fromSortedArrayRevAux arr off len =
  match len with
  | 0 -> empty
  | 1 -> let (k,v) = A.getUnsafe arr off in singleton k v
  | 2 ->
      let ((x0,y0),(x1,y1)) =
        let open A in ((getUnsafe arr off), (getUnsafe arr (off - 1))) in
      return @@
        (node ~left:(singleton x0 y0) ~key:x1 ~value:y1 ~height:2
           ~right:empty)
  | 3 ->
      let ((x0,y0),(x1,y1),(x2,y2)) =
        let open A in
          ((getUnsafe arr off), (getUnsafe arr (off - 1)),
            (getUnsafe arr (off - 2))) in
      return @@
        (node ~left:(singleton x0 y0) ~right:(singleton x2 y2) ~key:x1
           ~value:y1 ~height:2)
  | _ ->
      let nl = len / 2 in
      let left = fromSortedArrayRevAux arr off nl in
      let (midK,midV) = A.getUnsafe arr (off - nl) in
      let right = fromSortedArrayRevAux arr ((off - nl) - 1) ((len - nl) - 1) in
      create left midK midV right
let rec fromSortedArrayAux arr off len =
  match len with
  | 0 -> empty
  | 1 -> let (k,v) = A.getUnsafe arr off in singleton k v
  | 2 ->
      let ((x0,y0),(x1,y1)) =
        let open A in ((getUnsafe arr off), (getUnsafe arr (off + 1))) in
      return @@
        (node ~left:(singleton x0 y0) ~key:x1 ~value:y1 ~height:2
           ~right:empty)
  | 3 ->
      let ((x0,y0),(x1,y1),(x2,y2)) =
        let open A in
          ((getUnsafe arr off), (getUnsafe arr (off + 1)),
            (getUnsafe arr (off + 2))) in
      return @@
        (node ~left:(singleton x0 y0) ~right:(singleton x2 y2) ~key:x1
           ~value:y1 ~height:2)
  | _ ->
      let nl = len / 2 in
      let left = fromSortedArrayAux arr off nl in
      let (midK,midV) = A.getUnsafe arr (off + nl) in
      let right = fromSortedArrayAux arr ((off + nl) + 1) ((len - nl) - 1) in
      create left midK midV right
let fromSortedArrayUnsafe arr = fromSortedArrayAux arr 0 (A.length arr)
let rec compareAux e1 e2 ~kcmp  ~vcmp  =
  match (e1, e2) with
  | (h1::t1,h2::t2) ->
      let c = (Belt_Id.getCmpInternal kcmp) (key h1) (key h2) in
      if c = 0
      then
        let cx = vcmp (value h1) (value h2) in
        (if cx = 0
         then
           compareAux ~kcmp ~vcmp (stackAllLeft (right h1) t1)
             (stackAllLeft (right h2) t2)
         else cx)
      else c
  | (_,_) -> 0
let rec eqAux e1 e2 ~kcmp  ~veq  =
  match (e1, e2) with
  | (h1::t1,h2::t2) ->
      if
        (((Belt_Id.getCmpInternal kcmp) (key h1) (key h2)) = 0) &&
          (veq (value h1) (value h2))
      then
        eqAux ~kcmp ~veq (stackAllLeft (right h1) t1)
          (stackAllLeft (right h2) t2)
      else false
  | (_,_) -> true
let cmpU s1 s2 ~kcmp  ~vcmp  =
  let (len1,len2) = ((size s1), (size s2)) in
  if len1 = len2
  then compareAux (stackAllLeft s1 []) (stackAllLeft s2 []) ~kcmp ~vcmp
  else if len1 < len2 then (-1) else 1
let cmp s1 s2 ~kcmp  ~vcmp  =
  cmpU s1 s2 ~kcmp ~vcmp:(fun a  -> fun b  -> vcmp a b)
let eqU s1 s2 ~kcmp  ~veq  =
  let (len1,len2) = ((size s1), (size s2)) in
  if len1 = len2
  then eqAux (stackAllLeft s1 []) (stackAllLeft s2 []) ~kcmp ~veq
  else false
let eq s1 s2 ~kcmp  ~veq  =
  eqU s1 s2 ~kcmp ~veq:(fun a  -> fun b  -> veq a b)
let rec get n x ~cmp  =
  match toOpt n with
  | None  -> None
  | Some n ->
      let v = key n in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0
      then Some (value n)
      else get ~cmp (if c < 0 then left n else right n) x
let rec getUndefined n x ~cmp  =
  match toOpt n with
  | None  -> Js.undefined
  | Some n ->
      let v = key n in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0
      then Js.Undefined.return (value n)
      else getUndefined ~cmp (if c < 0 then left n else right n) x
let rec getExn n x ~cmp  =
  match toOpt n with
  | None  -> Js.Exn.raiseError "File \"\", line 609, characters 6-12"
  | Some n ->
      let v = key n in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0
      then value n
      else getExn ~cmp (if c < 0 then left n else right n) x
let rec getWithDefault n x def ~cmp  =
  match toOpt n with
  | None  -> def
  | Some n ->
      let v = key n in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      if c = 0
      then value n
      else getWithDefault ~cmp (if c < 0 then left n else right n) x def
let rec has n x ~cmp  =
  match toOpt n with
  | None  -> false
  | Some n ->
      let v = key n in
      let c = (Belt_Id.getCmpInternal cmp) x v in
      (c = 0) || (has ~cmp (if c < 0 then left n else right n) x)
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
  let v = rotateWithRightChild (unsafeCoerce (left k3)) in
  leftSet k3 (return v); rotateWithLeftChild k3
let doubleWithRightChild k2 =
  let v = rotateWithLeftChild (unsafeCoerce (right k2)) in
  rightSet k2 (return v); rotateWithRightChild k2
let heightUpdateMutate t =
  let (hlt,hrt) = ((treeHeight (left t)), (treeHeight (right t))) in
  heightSet t ((Pervasives.max hlt hrt) + 1); t
let balMutate nt =
  let (l,r) = ((left nt), (right nt)) in
  let (hl,hr) = ((treeHeight l), (treeHeight r)) in
  if hl > (2 + hr)
  then
    let l = unsafeCoerce l in
    let (ll,lr) = ((left l), (right l)) in
    (if heightGe ll lr
     then heightUpdateMutate (rotateWithLeftChild nt)
     else heightUpdateMutate (doubleWithLeftChild nt))
  else
    if hr > (2 + hl)
    then
      (let r = unsafeCoerce r in
       let (rl,rr) = ((left r), (right r)) in
       if heightGe rr rl
       then heightUpdateMutate (rotateWithRightChild nt)
       else heightUpdateMutate (doubleWithRightChild nt))
    else (heightSet nt ((max hl hr) + 1); nt)
let rec updateMutate (t : _ t) x data ~cmp  =
  match toOpt t with
  | None  -> singleton x data
  | Some nt ->
      let k = key nt in
      let c = (Belt_Id.getCmpInternal cmp) x k in
      if c = 0
      then (valueSet nt data; return nt)
      else
        (let (l,r) = ((left nt), (right nt)) in
         if c < 0
         then (let ll = updateMutate ~cmp l x data in leftSet nt ll)
         else rightSet nt (updateMutate ~cmp r x data);
         return (balMutate nt))
let fromArray (xs : _ array) ~cmp  =
  let len = A.length xs in
  if len = 0
  then empty
  else
    (let next =
       ref
         (S.strictlySortedLengthU xs
            (fun (x0,_)  ->
               fun (y0,_)  -> ((Belt_Id.getCmpInternal cmp) x0 y0) < 0)) in
     let result =
       ref
         (if (!next) >= 0
          then fromSortedArrayAux xs 0 (!next)
          else
            (next := (- (!next));
             fromSortedArrayRevAux xs ((!next) - 1) (!next))) in
     for i = !next to len - 1 do
       (let (k,v) = A.getUnsafe xs i in
        result := (updateMutate ~cmp (!result) k v))
     done;
     !result)
let rec removeMinAuxWithRootMutate nt n =
  let (rn,ln) = ((right n), (left n)) in
  match toOpt ln with
  | None  -> (keySet nt (key n); rn)
  | Some ln ->
      (leftSet n (removeMinAuxWithRootMutate nt ln); return (balMutate n))