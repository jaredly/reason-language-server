type ('a,'id) hash = 'a -> int
type ('a,'id) eq = 'a -> 'a -> bool
type ('a,'id) cmp = 'a -> 'a -> int
let getHashInternal: ('a,'id) hash -> 'a -> int = Obj.magic
let getEqInternal: ('a,'id) eq -> 'a -> 'a -> bool = Obj.magic
let getCmpInternal: ('a,'id) cmp -> 'a -> 'a -> int = Obj.magic
module type Comparable  =
  sig type identity type t val cmp : (t,identity) cmp end
type ('key,'id) comparable =
  (module Comparable with type t = 'key and type identity = 'id)
module MakeComparableU(M:sig type t val cmp : t -> t -> int end) =
  struct type identity
         type t = M.t
         let cmp = M.cmp end
module MakeComparable(M:sig type t val cmp : t -> t -> int end) =
  struct
    type identity
    type t = M.t
    let cmp = let cmp = M.cmp in fun a  -> fun b  -> cmp a b
  end
let comparableU (type key) ~cmp  =
  let module N = MakeComparableU(struct type t = key
                                        let cmp = cmp end) in ((module
    N) : (module Comparable with type t = key))
let comparable (type key) ~cmp  =
  let module N = MakeComparable(struct type t = key
                                       let cmp = cmp end) in ((module
    N) : (module Comparable with type t = key))
module type Hashable  =
  sig
    type identity
    type t
    val hash : (t,identity) hash
    val eq : (t,identity) eq
  end
type ('key,'id) hashable =
  (module Hashable with type t = 'key and type identity = 'id)
module MakeHashableU(M:sig type t val hash : t -> int val eq : t -> t -> bool
                       end) =
  struct type identity
         type t = M.t
         let hash = M.hash
         let eq = M.eq end
module MakeHashable(M:sig type t val hash : t -> int val eq : t -> t -> bool
                      end) =
  struct
    type identity
    type t = M.t
    let hash = let hash = M.hash in fun a  -> hash a
    let eq = let eq = M.eq in fun a  -> fun b  -> eq a b
  end
let hashableU (type key) ~hash  ~eq  =
  let module N =
    MakeHashableU(struct type t = key
                         let hash = hash
                         let eq = eq end) in ((module
    N) : (module Hashable with type t = key))
let hashable (type key) ~hash  ~eq  =
  let module N =
    MakeHashable(struct type t = key
                        let hash = hash
                        let eq = eq end) in ((module
    N) : (module Hashable with type t = key))