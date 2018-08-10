
module S = Set.Make({type t = int; let compare = compare;});


module M = {
    let x = 10;
};

let x = M.x;