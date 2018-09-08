let getExn =
  function
  | Some x -> x
  | None  -> Js.Exn.raiseError "File \"\", line 28, characters 14-20"
let mapWithDefaultU opt default f =
  match opt with | Some x -> f x | None  -> default
let mapWithDefault opt default f =
  mapWithDefaultU opt default (fun x  -> f x)
let mapU opt f = match opt with | Some x -> Some (f x) | None  -> None
let map opt f = mapU opt (fun x  -> f x)
let flatMapU opt f = match opt with | Some x -> f x | None  -> None
let flatMap opt f = flatMapU opt (fun x  -> f x)
let getWithDefault opt default =
  match opt with | Some x -> x | None  -> default
let isSome = function | Some _ -> true | None  -> false
let isNone = function | Some _ -> false | None  -> true
let eqU a b f =
  match (a, b) with
  | (Some a,Some b) -> f a b
  | (None ,Some _)|(Some _,None ) -> false
  | (None ,None ) -> true
let eq a b f = eqU a b (fun x  -> fun y  -> f x y)
let cmpU a b f =
  match (a, b) with
  | (Some a,Some b) -> f a b
  | (None ,Some _) -> (-1)
  | (Some _,None ) -> 1
  | (None ,None ) -> 0
let cmp a b f = cmpU a b (fun x  -> fun y  -> f x y)