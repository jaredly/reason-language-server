
let get = (k, obj) => Json.get(k, obj) |> Result.orError("No key " ++ k);
let string = (obj) => Json.string(obj) |> Result.orError("Not a string");
let number = (obj) => Json.number(obj) |> Result.orError("Not a number");
let array = obj => Json.array(obj) |> Result.orError("Expected an array");

let extend = (obj, attributes) => Json.obj(obj) |> Result.orError("Expected an object")