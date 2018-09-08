[@@@ocaml.text
  "  Adapted by Authors of BuckleScript 2017                           "]
module C = Belt_internalBucketsType
include
  (struct
     type ('a,'b) bucket =
       {
       mutable key: 'a;
       mutable value: 'b;
       mutable next: ('a,'b) bucket C.opt;}
     and ('hash,'eq,'a,'b) t = ('hash,'eq,('a,'b) bucket) C.container
     let bucket:
       key:'a -> value:'b -> next:('a,'b) bucket C.opt -> ('a,'b) bucket =
       fun ~key  -> fun ~value  -> fun ~next  -> { key; value; next }
     let keySet: ('a,'b) bucket -> 'a -> unit =
       fun o  -> fun v  -> o.key <- v
     let key: ('a,'b) bucket -> 'a = fun o  -> o.key
     let valueSet: ('a,'b) bucket -> 'b -> unit =
       fun o  -> fun v  -> o.value <- v
     let value: ('a,'b) bucket -> 'b = fun o  -> o.value
     let nextSet: ('a,'b) bucket -> ('a,'b) bucket C.opt -> unit =
       fun o  -> fun v  -> o.next <- v
     let next: ('a,'b) bucket -> ('a,'b) bucket C.opt = fun o  -> o.next
   end :
    sig
      type ('a,'b) bucket
      and ('hash,'eq,'a,'b) t = ('hash,'eq,('a,'b) bucket) C.container
      val bucket :
        key:'a -> value:'b -> next:('a,'b) bucket C.opt -> ('a,'b) bucket
      val keySet : ('a,'b) bucket -> 'a -> unit
      val key : ('a,'b) bucket -> 'a
      val valueSet : ('a,'b) bucket -> 'b -> unit
      val value : ('a,'b) bucket -> 'b
      val nextSet : ('a,'b) bucket -> ('a,'b) bucket C.opt -> unit
      val next : ('a,'b) bucket -> ('a,'b) bucket C.opt
    end)
module A = Belt_Array
let rec copy (x : _ t) =
  (C.container ~hash:(C.hash x) ~eq:(C.eq x) ~size:(C.size x)
     ~buckets:(copyBuckets (C.buckets x)) : _ t)
and copyBuckets (buckets : _ bucket C.opt array) =
  let len = A.length buckets in
  let newBuckets =
    if len > 0
    then A.makeUninitializedUnsafe len (A.getUnsafe buckets 0)
    else [||] in
  for i = 0 to len - 1 do
    A.setUnsafe newBuckets i (copyBucket (A.getUnsafe buckets i))
  done;
  newBuckets
and copyBucket c =
  match C.toOpt c with
  | None  -> c
  | Some c ->
      let head = bucket ~key:(key c) ~value:(value c) ~next:C.emptyOpt in
      (copyAuxCont (next c) head; C.return head)
and copyAuxCont c prec =
  match C.toOpt c with
  | None  -> ()
  | Some nc ->
      let ncopy = bucket ~key:(key nc) ~value:(value nc) ~next:C.emptyOpt in
      (nextSet prec (C.return ncopy); copyAuxCont (next nc) ncopy)
let rec bucketLength accu buckets =
  match C.toOpt buckets with
  | None  -> accu
  | Some cell -> bucketLength (accu + 1) (next cell)
let rec do_bucket_iter ~f  buckets =
  match C.toOpt buckets with
  | None  -> ()
  | Some cell -> (f (key cell) (value cell); do_bucket_iter ~f (next cell))
let forEachU h f =
  let d = C.buckets h in
  for i = 0 to (A.length d) - 1 do do_bucket_iter f (A.getUnsafe d i) done
let forEach h f = forEachU h (fun a  -> fun b  -> f a b)
let rec do_bucket_fold ~f  b accu =
  match C.toOpt b with
  | None  -> accu
  | Some cell ->
      do_bucket_fold ~f (next cell) (f accu (key cell) (value cell))
let reduceU h init f =
  let d = C.buckets h in
  let accu = ref init in
  for i = 0 to (A.length d) - 1 do
    accu := (do_bucket_fold ~f (A.getUnsafe d i) (!accu))
  done;
  !accu
let reduce h init f = reduceU h init (fun a  -> fun b  -> fun c  -> f a b c)
let getMaxBucketLength h =
  A.reduceU (C.buckets h) 0
    (fun m  -> fun b  -> let len = bucketLength 0 b in Pervasives.max m len)
let getBucketHistogram h =
  let mbl = getMaxBucketLength h in
  let histo = A.makeByU (mbl + 1) (fun _  -> 0) in
  A.forEachU (C.buckets h)
    (fun b  ->
       let l = bucketLength 0 b in
       A.setUnsafe histo l ((A.getUnsafe histo l) + 1));
  histo
let logStats h =
  let histogram = getBucketHistogram h in
  Printf.printf "{\n\tbindings: %d,\n\tbuckets: %d\n\thistogram: %s\n}"
    (C.size h) (A.length (C.buckets h))
    (A.reduceU histogram "" (fun acc  -> fun x  -> acc ^ (string_of_int x)))
let rec filterMapInplaceBucket f h i prec cell =
  let n = next cell in
  match f (key cell) (value cell) with
  | None  ->
      (C.sizeSet h ((C.size h) - 1);
       (match C.toOpt n with
        | Some nextCell -> filterMapInplaceBucket f h i prec nextCell
        | None  ->
            (match C.toOpt prec with
             | None  -> A.setUnsafe (C.buckets h) i prec
             | Some cell -> nextSet cell n)))
  | Some data ->
      let bucket = C.return cell in
      ((match C.toOpt prec with
        | None  -> A.setUnsafe (C.buckets h) i bucket
        | Some c -> nextSet cell bucket);
       valueSet cell data;
       (match C.toOpt n with
        | None  -> nextSet cell n
        | Some nextCell -> filterMapInplaceBucket f h i bucket nextCell))
  [@@ocaml.doc " iterate the Buckets, in place remove the elements "]
let keepMapInPlaceU h f =
  let h_buckets = C.buckets h in
  for i = 0 to (A.length h_buckets) - 1 do
    let v = A.getUnsafe h_buckets i in
    match C.toOpt v with
    | None  -> ()
    | Some v -> filterMapInplaceBucket f h i C.emptyOpt v
  done
let keepMapInPlace h f = keepMapInPlaceU h (fun a  -> fun b  -> f a b)
let rec fillArray i arr cell =
  A.setUnsafe arr i ((key cell), (value cell));
  (match C.toOpt (next cell) with
   | None  -> i + 1
   | Some v -> fillArray (i + 1) arr v)
let toArray h =
  let d = C.buckets h in
  let current = ref 0 in
  let arr = ref None in
  for i = 0 to (A.length d) - 1 do
    (let cell = A.getUnsafe d i in
     match C.toOpt cell with
     | None  -> ()
     | Some cell ->
         let arr =
           match !arr with
           | None  ->
               let a =
                 A.makeUninitializedUnsafe (C.size h)
                   ((key cell), (value cell)) in
               (arr := (Some a); a)
           | Some arr -> arr in
         current := (fillArray (!current) arr cell))
  done;
  (match !arr with | None  -> [||] | Some arr -> arr)
let rec fillArrayMap i arr cell f =
  A.setUnsafe arr i (f cell);
  (match C.toOpt (next cell) with
   | None  -> i + 1
   | Some v -> fillArrayMap (i + 1) arr v f)
let linear h f =
  let d = C.buckets h in
  let current = ref 0 in
  let arr = ref None in
  for i = 0 to (A.length d) - 1 do
    (let cell = A.getUnsafe d i in
     match C.toOpt cell with
     | None  -> ()
     | Some cell ->
         let arr =
           match !arr with
           | None  ->
               let a = A.makeUninitializedUnsafe (C.size h) (f cell) in
               (arr := (Some a); a)
           | Some arr -> arr in
         current := (fillArrayMap (!current) arr cell f))
  done;
  (match !arr with | None  -> [||] | Some arr -> arr)
let keysToArray h = linear h (fun x  -> key x)
let valuesToArray h = linear h (fun x  -> value x)
let toArray h = linear h (fun x  -> ((key x), (value x)))