type 'a opt = 'a Js.undefined
include
  (struct
     type ('hash,'eq,'c) container =
       {
       mutable size: int;
       mutable buckets: 'c opt array;
       hash: 'hash;
       eq: 'eq;}
     let container:
       size:int ->
         buckets:'c opt array ->
           hash:'hash -> eq:'eq -> ('hash,'eq,'c) container
       =
       fun ~size  ->
         fun ~buckets  ->
           fun ~hash  -> fun ~eq  -> { size; buckets; hash; eq }
     let sizeSet: ('hash,'eq,'c) container -> int -> unit =
       fun o  -> fun v  -> o.size <- v
     let size: ('hash,'eq,'c) container -> int = fun o  -> o.size
     let bucketsSet: ('hash,'eq,'c) container -> 'c opt array -> unit =
       fun o  -> fun v  -> o.buckets <- v
     let buckets: ('hash,'eq,'c) container -> 'c opt array =
       fun o  -> o.buckets
     let hash: ('hash,'eq,'c) container -> 'hash = fun o  -> o.hash
     let eq: ('hash,'eq,'c) container -> 'eq = fun o  -> o.eq
   end :
    sig
      type ('hash,'eq,'c) container
      val container :
        size:int ->
          buckets:'c opt array ->
            hash:'hash -> eq:'eq -> ('hash,'eq,'c) container
      val sizeSet : ('hash,'eq,'c) container -> int -> unit
      val size : ('hash,'eq,'c) container -> int
      val bucketsSet : ('hash,'eq,'c) container -> 'c opt array -> unit
      val buckets : ('hash,'eq,'c) container -> 'c opt array
      val hash : ('hash,'eq,'c) container -> 'hash
      val eq : ('hash,'eq,'c) container -> 'eq
    end)
module A = Belt_Array
let toOpt = Js.undefinedToOption
let return = Js.Undefined.return
let emptyOpt = Js.undefined
let rec power_2_above x n =
  if x >= n then x else if (x * 2) < x then x else power_2_above (x * 2) n
let make ~hash  ~eq  ~hintSize  =
  let s = power_2_above 16 hintSize in
  container ~size:0 ~buckets:(A.makeUninitialized s) ~hash ~eq
let clear h =
  sizeSet h 0;
  (let h_buckets = buckets h in
   let len = A.length h_buckets in
   for i = 0 to len - 1 do A.setUnsafe h_buckets i emptyOpt done)
let isEmpty h = (size h) = 0