module A = Belt_Array
include
  (struct
     type 'a node = {
       content: 'a;
       mutable next: 'a cell;}
     and 'a cell = 'a node Js.null
     and 'a t =
       {
       mutable length: int;
       mutable first: 'a cell;
       mutable last: 'a cell;}
     let node: content:'a -> next:'a cell -> 'a node =
       fun ~content  -> fun ~next  -> { content; next }
     let content: 'a node -> 'a = fun o  -> o.content
     let nextSet: 'a node -> 'a cell -> unit =
       fun o  -> fun v  -> o.next <- v
     let next: 'a node -> 'a cell = fun o  -> o.next
     let t: length:int -> first:'a cell -> last:'a cell -> 'a t =
       fun ~length  -> fun ~first  -> fun ~last  -> { length; first; last }
     let lengthSet: 'a t -> int -> unit = fun o  -> fun v  -> o.length <- v
     let length: 'a t -> int = fun o  -> o.length
     let firstSet: 'a t -> 'a cell -> unit = fun o  -> fun v  -> o.first <- v
     let first: 'a t -> 'a cell = fun o  -> o.first
     let lastSet: 'a t -> 'a cell -> unit = fun o  -> fun v  -> o.last <- v
     let last: 'a t -> 'a cell = fun o  -> o.last
   end :
    sig
      type 'a node
      and 'a cell = 'a node Js.null
      and 'a t
      val node : content:'a -> next:'a cell -> 'a node
      val content : 'a node -> 'a
      val nextSet : 'a node -> 'a cell -> unit
      val next : 'a node -> 'a cell
      val t : length:int -> first:'a cell -> last:'a cell -> 'a t
      val lengthSet : 'a t -> int -> unit
      val length : 'a t -> int
      val firstSet : 'a t -> 'a cell -> unit
      val first : 'a t -> 'a cell
      val lastSet : 'a t -> 'a cell -> unit
      val last : 'a t -> 'a cell
    end)
let null = Js.null
let return = Js.Null.return
let make () = t ~length:0 ~first:null ~last:null
let clear q = lengthSet q 0; firstSet q null; lastSet q null
let add q x =
  let cell = return @@ (node ~content:x ~next:null) in
  match Js.nullToOption (last q) with
  | None  -> (lengthSet q 1; firstSet q cell; lastSet q cell)
  | Some last ->
      (lengthSet q ((length q) + 1); nextSet last cell; lastSet q cell)
let peek q =
  match Js.nullToOption (first q) with
  | None  -> None
  | Some v -> Some (content v)
let peekUndefined q =
  match Js.nullToOption (first q) with
  | None  -> Js.undefined
  | Some v -> Js.Undefined.return (content v)
let peekExn q =
  match Js.nullToOption (first q) with
  | None  -> Js.Exn.raiseError "File \"\", line 74, characters 14-20"
  | Some v -> content v
let pop q =
  match Js.nullToOption (first q) with
  | None  -> None
  | Some x ->
      let next = next x in
      if next = Js.null
      then (clear q; Some (content x))
      else (lengthSet q ((length q) - 1); firstSet q next; Some (content x))
let popExn q =
  match Js.nullToOption (first q) with
  | None  -> Js.Exn.raiseError "File \"\", line 95, characters 14-20"
  | Some x ->
      let next = next x in
      if next = Js.null
      then (clear q; content x)
      else (lengthSet q ((length q) - 1); firstSet q next; content x)
let popUndefined q =
  match Js.nullToOption (first q) with
  | None  -> Js.undefined
  | Some x ->
      let next = next x in
      if next = Js.null
      then (clear q; Js.Undefined.return (content x))
      else
        (lengthSet q ((length q) - 1);
         firstSet q next;
         Js.Undefined.return (content x))
let rec copyAux qRes prev cell =
  match Js.nullToOption cell with
  | None  -> (lastSet qRes prev; qRes)
  | Some x ->
      let content = content x in
      let res = return @@ (node ~content ~next:null) in
      ((match Js.nullToOption prev with
        | None  -> firstSet qRes res
        | Some p -> nextSet p res);
       copyAux qRes res (next x))
let copy q =
  copyAux (t ~length:(length q) ~first:null ~last:null) null (first q)
let rec copyMapAux qRes prev cell f =
  match Js.nullToOption cell with
  | None  -> (lastSet qRes prev; qRes)
  | Some x ->
      let content = f (content x) in
      let res = return @@ (node ~content ~next:null) in
      ((match Js.nullToOption prev with
        | None  -> firstSet qRes res
        | Some p -> nextSet p res);
       copyMapAux qRes res (next x) f)
let mapU q f =
  copyMapAux (t ~length:(length q) ~first:null ~last:null) null (first q) f
let map q f = mapU q (fun a  -> f a)
let isEmpty q = (length q) = 0
let size q = length q
let rec iterAux cell f =
  match Js.nullToOption cell with
  | None  -> ()
  | Some x -> (f (content x); iterAux (next x) f)
let forEachU q f = iterAux (first q) f
let forEach q f = forEachU q (fun a  -> f a)
let rec foldAux f accu cell =
  match Js.nullToOption cell with
  | None  -> accu
  | Some x -> let accu = f accu (content x) in foldAux f accu (next x)
let reduceU q accu f = foldAux f accu (first q)
let reduce q accu f = reduceU q accu (fun a  -> fun b  -> f a b)
let transfer q1 q2 =
  if (length q1) > 0
  then
    match Js.nullToOption (last q2) with
    | None  ->
        (lengthSet q2 (length q1);
         firstSet q2 (first q1);
         lastSet q2 (last q1);
         clear q1)
    | Some l ->
        (lengthSet q2 ((length q2) + (length q1));
         nextSet l (first q1);
         lastSet q2 (last q1);
         clear q1)
let rec fillAux i arr cell =
  match Js.nullToOption cell with
  | None  -> ()
  | Some x -> (A.setUnsafe arr i (content x); fillAux (i + 1) arr (next x))
let toArray x =
  let v =
    match Js.Null.toOption (first x) with
    | None  -> [||]
    | Some y -> A.makeUninitializedUnsafe (length x) (content y) in
  fillAux 0 v (first x); v
let fromArray arr =
  let q = make () in
  for i = 0 to (A.length arr) - 1 do add q (A.getUnsafe arr i) done; q