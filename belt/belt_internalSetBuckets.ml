module C = Belt_internalBucketsType
include
  (struct
     type 'a bucket = {
       mutable key: 'a;
       mutable next: 'a bucket C.opt;}
     and ('hash,'eq,'a) t = ('hash,'eq,'a bucket) C.container
     let bucket: key:'a -> next:'a bucket C.opt -> 'a bucket =
       fun ~key  -> fun ~next  -> { key; next }
     let keySet: 'a bucket -> 'a -> unit = fun o  -> fun v  -> o.key <- v
     let key: 'a bucket -> 'a = fun o  -> o.key
     let nextSet: 'a bucket -> 'a bucket C.opt -> unit =
       fun o  -> fun v  -> o.next <- v
     let next: 'a bucket -> 'a bucket C.opt = fun o  -> o.next
   end :
    sig
      type 'a bucket
      and ('hash,'eq,'a) t = ('hash,'eq,'a bucket) C.container
      val bucket : key:'a -> next:'a bucket C.opt -> 'a bucket
      val keySet : 'a bucket -> 'a -> unit
      val key : 'a bucket -> 'a
      val nextSet : 'a bucket -> 'a bucket C.opt -> unit
      val next : 'a bucket -> 'a bucket C.opt
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
      let head = bucket ~key:(key c) ~next:C.emptyOpt in
      (copyAuxCont (next c) head; C.return head)
and copyAuxCont c prec =
  match C.toOpt c with
  | None  -> ()
  | Some nc ->
      let ncopy = bucket ~key:(key nc) ~next:C.emptyOpt in
      (nextSet prec (C.return ncopy); copyAuxCont (next nc) ncopy)
let rec bucketLength accu buckets =
  match C.toOpt buckets with
  | None  -> accu
  | Some cell -> bucketLength (accu + 1) (next cell)
let rec doBucketIter ~f  buckets =
  match C.toOpt buckets with
  | None  -> ()
  | Some cell -> (f (key cell); doBucketIter ~f (next cell))
let forEachU h f =
  let d = C.buckets h in
  for i = 0 to (A.length d) - 1 do doBucketIter f (A.getUnsafe d i) done
let forEach h f = forEachU h (fun a  -> f a)
let rec fillArray i arr cell =
  A.setUnsafe arr i (key cell);
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
               let a = A.makeUninitializedUnsafe (C.size h) (key cell) in
               (arr := (Some a); a)
           | Some arr -> arr in
         current := (fillArray (!current) arr cell))
  done;
  (match !arr with | None  -> [||] | Some arr -> arr)
let rec doBucketFold ~f  b accu =
  match C.toOpt b with
  | None  -> accu
  | Some cell -> doBucketFold ~f (next cell) (f accu (key cell))
let reduceU h init f =
  let d = C.buckets h in
  let accu = ref init in
  for i = 0 to (A.length d) - 1 do
    accu := (doBucketFold ~f (A.getUnsafe d i) (!accu))
  done;
  !accu
let reduce h init f = reduceU h init (fun a  -> fun b  -> f a b)
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