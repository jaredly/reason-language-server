let forEachU s f action = for i = s to f do (action i : unit) done
let forEach s f action = forEachU s f (fun a  -> action a)
let rec everyU s f p = if s > f then true else (p s) && (everyU (s + 1) f p)
let every s f p = everyU s f (fun a  -> p a)
let rec everyByAux s f ~step  p =
  if s > f then true else (p s) && (everyByAux (s + step) f ~step p)
let everyByU s f ~step  p = if step > 0 then everyByAux s f ~step p else true
let everyBy s f ~step  p = everyByU s f ~step (fun a  -> p a)
let rec someU s f p = if s > f then false else (p s) || (someU (s + 1) f p)
let some s f p = someU s f (fun a  -> p a)
let rec someByAux s f ~step  p =
  if s > f then false else (p s) || (someByAux (s + step) f ~step p)
let someByU s f ~step  p = if step > 0 then someByAux s f ~step p else false
let someBy s f ~step  p = someByU s f ~step (fun a  -> p a)