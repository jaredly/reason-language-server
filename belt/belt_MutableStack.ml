include
  (struct
     type 'a t = {
       mutable root: 'a opt_cell;}
     and 'a opt_cell = 'a cell Js.null
     and 'a cell = {
       head: 'a;
       tail: 'a opt_cell;}
     let t: root:'a opt_cell -> 'a t = fun ~root  -> { root }
     let rootSet: 'a t -> 'a opt_cell -> unit =
       fun o  -> fun v  -> o.root <- v
     let root: 'a t -> 'a opt_cell = fun o  -> o.root
     let cell: head:'a -> tail:'a opt_cell -> 'a cell =
       fun ~head  -> fun ~tail  -> { head; tail }
     let head: 'a cell -> 'a = fun o  -> o.head
     let tail: 'a cell -> 'a opt_cell = fun o  -> o.tail
   end :
    sig
      type 'a t
      and 'a opt_cell = 'a cell Js.null
      and 'a cell
      val t : root:'a opt_cell -> 'a t
      val rootSet : 'a t -> 'a opt_cell -> unit
      val root : 'a t -> 'a opt_cell
      val cell : head:'a -> tail:'a opt_cell -> 'a cell
      val head : 'a cell -> 'a
      val tail : 'a cell -> 'a opt_cell
    end)
let make () = t ~root:Js.null
let clear s = rootSet s Js.null
let copy (s : _ t) = (t ~root:(root s) : _ t)
let push s x = rootSet s (Js.Null.return @@ (cell ~head:x ~tail:(root s)))
let topUndefined (s : 'a t) =
  match Js.nullToOption (root s) with
  | None  -> Js.undefined
  | Some x -> Js.Undefined.return (head x)
let top s =
  match Js.nullToOption (root s) with
  | None  -> None
  | Some x -> Some (head x)
let isEmpty s = (root s) = Js.null
let popUndefined s =
  match Js.nullToOption (root s) with
  | None  -> Js.undefined
  | Some x -> (rootSet s (tail x); Js.Undefined.return (head x))
let pop s =
  match Js.nullToOption (root s) with
  | None  -> None
  | Some x -> (rootSet s (tail x); Some (head x))
let rec lengthAux (x : _ cell) acc =
  match Js.nullToOption (tail x) with
  | None  -> acc + 1
  | Some x -> lengthAux x (acc + 1)
let size s =
  match Js.nullToOption (root s) with | None  -> 0 | Some x -> lengthAux x 0
let rec iterAux (s : _ opt_cell) f =
  match Js.nullToOption s with
  | None  -> ()
  | Some x -> (f (head x); iterAux (tail x) f)
let forEachU s f = iterAux (root s) f
let forEach s f = forEachU s (fun x  -> f x)
let dynamicPopIterU s f =
  let cursor = ref (root s) in
  while (!cursor) != Js.null do
    let v = Js.Null.getUnsafe (!cursor) in
    rootSet s (tail v); f (head v); cursor := (root s) done
let dynamicPopIter s f = dynamicPopIterU s (fun x  -> f x)