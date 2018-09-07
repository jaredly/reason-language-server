module Int = Belt_SetInt
module String = Belt_SetString
module Dict = Belt_SetDict
module A = Belt_Array
type ('value,'id) id = ('value,'id) Belt_Id.comparable
type ('value,'id) cmp = ('value,'id) Belt_Id.cmp
module S =
  struct
    include
      (struct
         type ('value,'id) t =
           {
           cmp: ('value,'id) cmp;
           data: ('value,'id) Dict.t;}
         let t:
           cmp:('value,'id) cmp -> data:('value,'id) Dict.t -> ('value,'id) t
           = fun ~cmp  -> fun ~data  -> { cmp; data }
         let cmp: ('value,'id) t -> ('value,'id) cmp = fun o  -> o.cmp
         let data: ('value,'id) t -> ('value,'id) Dict.t = fun o  -> o.data
       end :
        sig
          type ('value,'id) t
          val t :
            cmp:('value,'id) cmp ->
              data:('value,'id) Dict.t -> ('value,'id) t
          val cmp : ('value,'id) t -> ('value,'id) cmp
          val data : ('value,'id) t -> ('value,'id) Dict.t
        end)
  end
type ('value,'id) t = ('value,'id) S.t
let fromArray (type value) (type identity) data
  ~id:(id : (value,identity) id)  =
  let module M = (val id) in
    let cmp = M.cmp in S.t ~cmp ~data:(Dict.fromArray ~cmp data)
let remove m e =
  let (cmp,data) = ((S.cmp m), (S.data m)) in
  let newData = Dict.remove ~cmp data e in
  if newData == data then m else S.t ~cmp ~data:newData
let add m e =
  let (cmp,data) = ((S.cmp m), (S.data m)) in
  let newData = Dict.add ~cmp data e in
  if newData == data then m else S.t ~cmp ~data:newData
let mergeMany m e =
  let cmp = S.cmp m in S.t ~cmp ~data:(Dict.mergeMany ~cmp (S.data m) e)
let removeMany m e =
  let cmp = S.cmp m in S.t ~cmp ~data:(Dict.removeMany ~cmp (S.data m) e)
let union m n =
  let cmp = S.cmp m in S.t ~data:(Dict.union ~cmp (S.data m) (S.data n)) ~cmp
let intersect m n =
  let cmp = S.cmp m in
  S.t ~data:(Dict.intersect ~cmp (S.data m) (S.data n)) ~cmp
let diff m n =
  let cmp = S.cmp m in S.t ~cmp ~data:(Dict.diff ~cmp (S.data m) (S.data n))
let subset m n = let cmp = S.cmp m in Dict.subset ~cmp (S.data m) (S.data n)
let split m e =
  let cmp = S.cmp m in
  let ((l,r),b) = Dict.split ~cmp (S.data m) e in
  (((S.t ~cmp ~data:l), (S.t ~cmp ~data:r)), b)
let make (type value) (type identity) ~id:(id : (value,identity) id)  =
  let module M = (val id) in S.t ~cmp:M.cmp ~data:Dict.empty
let isEmpty m = Dict.isEmpty (S.data m)
let cmp m n = let cmp = S.cmp m in Dict.cmp ~cmp (S.data m) (S.data n)
let eq m n = Dict.eq ~cmp:(S.cmp m) (S.data m) (S.data n)
let forEachU m f = Dict.forEachU (S.data m) f
let forEach m f = forEachU m (fun a  -> f a)
let reduceU m acc f = Dict.reduceU (S.data m) acc f
let reduce m acc f = reduceU m acc (fun a  -> fun b  -> f a b)
let everyU m f = Dict.everyU (S.data m) f
let every m f = everyU m (fun a  -> f a)
let someU m f = Dict.someU (S.data m) f
let some m f = someU m (fun a  -> f a)
let keepU m f = S.t ~cmp:(S.cmp m) ~data:(Dict.keepU (S.data m) f)
let keep m f = keepU m (fun a  -> f a)
let partitionU m f =
  let (l,r) = Dict.partitionU (S.data m) f in
  let cmp = S.cmp m in ((S.t ~data:l ~cmp), (S.t ~data:r ~cmp))
let partition m f = partitionU m (fun a  -> f a)
let size m = Dict.size (S.data m)
let toList m = Dict.toList (S.data m)
let toArray m = Dict.toArray (S.data m)
let minimum m = Dict.minimum (S.data m)
let minUndefined m = Dict.minUndefined (S.data m)
let maximum m = Dict.maximum (S.data m)
let maxUndefined m = Dict.maxUndefined (S.data m)
let get m e = Dict.get ~cmp:(S.cmp m) (S.data m) e
let getUndefined m e = Dict.getUndefined ~cmp:(S.cmp m) (S.data m) e
let getExn m e = Dict.getExn ~cmp:(S.cmp m) (S.data m) e
let has m e = Dict.has ~cmp:(S.cmp m) (S.data m) e
let fromSortedArrayUnsafe (type value) (type identity) xs
  ~id:(id : (value,identity) id)  =
  let module M = (val id) in
    S.t ~cmp:M.cmp ~data:(Dict.fromSortedArrayUnsafe xs)
let getData = S.data
let getId (type value) (type identity) (m : (value,identity) t) =
  (let module T =
     struct
       type nonrec identity = identity[@@nonrec ]
       type nonrec t = value[@@nonrec ]
       let cmp = S.cmp m
     end in (module T) : (value,identity) id)
let packIdData (type value) (type identity) ~id:(id : (value,identity) id) 
  ~data  = let module M = (val id) in S.t ~cmp:M.cmp ~data
let checkInvariantInternal d = Dict.checkInvariantInternal (S.data d)