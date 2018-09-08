type ('a,'b) t = ('a,'b) result =
  | Ok of 'a
  | Error of 'b
let getExn =
  function
  | Ok x -> x
  | Error _ -> Js.Exn.raiseError "File \"\", line 31, characters 17-23"
let mapWithDefaultU opt default f =
  match opt with | Ok x -> f x | Error _ -> default
let mapWithDefault opt default f =
  mapWithDefaultU opt default (fun x  -> f x)
let mapU opt f = match opt with | Ok x -> Ok (f x) | Error y -> Error y
let map opt f = mapU opt (fun x  -> f x)
let flatMapU opt f = match opt with | Ok x -> f x | Error y -> Error y
let flatMap opt f = flatMapU opt (fun x  -> f x)
let getWithDefault opt default =
  match opt with | Ok x -> x | Error _ -> default
let isOk = function | Ok _ -> true | Error _ -> false
let isError = function | Ok _ -> false | Error _ -> true
let eqU a b f =
  match (a, b) with
  | (Ok a,Ok b) -> f a b
  | (Error _,Ok _)|(Ok _,Error _) -> false
  | (Error _,Error _) -> true
let eq a b f = eqU a b (fun x  -> fun y  -> f x y)
let cmpU a b f =
  match (a, b) with
  | (Ok a,Ok b) -> f a b
  | (Error _,Ok _) -> (-1)
  | (Ok _,Error _) -> 1
  | (Error _,Error _) -> 0
let cmp a b f = cmpU a b (fun x  -> fun y  -> f x y)