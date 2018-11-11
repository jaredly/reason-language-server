

open Vendor;
open Util;

let deserialize_Belt__Belt_HashMapInt____t = (valueTransform, json) => {
  Ok(Belt.HashMap.Int.make(~hintSize=10))
};

let serialize_Belt__Belt_HashMapInt____t = (valueTransform, map) => Json.Null;

let deserialize_Stdlib__hashtbl____t = (keyTransform, valueTransform, json) => {
  let%try items = RJson.array(json);
  let count = List.length(items);
  let tbl = Hashtbl.create(count);
  let rec loop = items => {
    switch items {
      | [] => Ok(tbl)
      | [one, ...rest] =>
        let%try items = RJson.array(one);
        switch items {
          | [key, value] =>
            let%try key = keyTransform(key);
            let%try value = valueTransform(value);
            Hashtbl.replace(tbl, key, value);
            loop(rest)
          | _ => Error("Invalid object format")
        }
    }
  };
  loop(items);
};

let serialize_Stdlib__hashtbl____t = (keyTransform, valueTransform, tbl) => {
  Vendor.Json.Array(Hashtbl.fold((key, value, result) => {
    [Vendor.Json.Array([keyTransform(key), valueTransform(value)]), ...result]
  }, tbl, []))
};