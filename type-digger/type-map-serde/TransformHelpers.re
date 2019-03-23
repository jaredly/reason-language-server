

open Vendor;
open Util;

let wrapError = value => switch value {
  | Error(m) => Error([m])
  | Ok(v) => Ok(v)
};

let deserialize_Parsetree____structure = json => {
  let%try text = RJson.string(json);
  let lexbuf = Stdlib.Lexing.from_string(text);
  Ok(Parser.implementation(Lexer.token, lexbuf))
} |> wrapError;

let serialize_Parsetree____structure = structure => {
  Pprintast.structure(Stdlib.Format.str_formatter, structure);
  Json.String(Stdlib.Format.flush_str_formatter());
};

let deserialize_Parsetree____signature = json => {
  let%try text = RJson.string(json);
  let lexbuf = Stdlib.Lexing.from_string(text);
  Ok(Parser.interface(Lexer.token, lexbuf))
} |> wrapError;

let serialize_Parsetree____signature = data => {
  Pprintast.signature(Stdlib.Format.str_formatter, data);
  Json.String(Stdlib.Format.flush_str_formatter());
};

let deserialize_Parsetree____pattern = json => {
  let%try text = RJson.string(json);
  let lexbuf = Stdlib.Lexing.from_string(text);
  Ok(Parser.parse_pattern(Lexer.token, lexbuf))
} |> wrapError;

let serialize_Parsetree____pattern = data => {
  Pprintast.pattern(Stdlib.Format.str_formatter, data);
  Json.String(Stdlib.Format.flush_str_formatter());
};

let deserialize_Parsetree____core_type = json => {
  let%try text = RJson.string(json);
  let lexbuf = Stdlib.Lexing.from_string(text);
  Ok(Parser.parse_core_type(Lexer.token, lexbuf))
} |> wrapError;

let serialize_Parsetree____core_type = data => {
  Pprintast.core_type(Stdlib.Format.str_formatter, data);
  Json.String(Stdlib.Format.flush_str_formatter());
};

let deserialize_Parsetree____expression = json => {
  let%try text = RJson.string(json);
  let lexbuf = Stdlib.Lexing.from_string(text);
  Ok(Parser.parse_expression(Lexer.token, lexbuf))
} |> wrapError;

let serialize_Parsetree____expression = data => {
  Pprintast.expression(Stdlib.Format.str_formatter, data);
  Json.String(Stdlib.Format.flush_str_formatter());
};

let migrate_Stdlib__hashtbl__t = (keyMigrator, valueMigrator, data) => {
  let result = Hashtbl.create(10);
  data |> Stdlib.Hashtbl.iter((key, value) => Hashtbl.replace(result, keyMigrator(key), valueMigrator(value)));
  result
};

let deserialize_Belt__Belt_HashMapInt____t = (valueTransform, json) => {
  let map = Belt.HashMap.Int.make(~hintSize=10);
  let%try items = RJson.obj(json) |> wrapError;
  let rec loop = items => {
    switch items {
      | [] => Ok(map)
      | [(k, v), ...rest] => switch (int_of_string(k)) {
        | exception _ => Error(["Expected integer key: " ++ k])
        | key => {
          let%try value = valueTransform(v);
          map->Belt.HashMap.Int.set(key, value);
          loop(rest)
        }
      }
    }
  };
  loop(items)
};

let serialize_Belt__Belt_HashMapInt____t = (_valueTransform, _map) => {
  /* map->Belt.HashMap.Int.toArray; */
  /* map->Belt.HashMap.Int.reduce([], (result, key, value) => {
    [("one", Json.Null), ...result]
  })->Json.Object; */
  Json.Null
};

let deserialize_Stdlib__hashtbl____t = (keyTransform, valueTransform, json) => {
  let%try items = RJson.array(json) |> wrapError;
  let count = List.length(items);
  let tbl = Hashtbl.create(count);
  let rec loop = items => {
    switch items {
      | [] => Ok(tbl)
      | [one, ...rest] =>
        let%try items = RJson.array(one) |> wrapError;
        switch items {
          | [key, value] =>
            let%try key = keyTransform(key);
            let%try value = valueTransform(value);
            Hashtbl.replace(tbl, key, value);
            loop(rest)
          | _ => Error(["Invalid object format"])
        }
    }
  };
  loop(items);
};

let serialize_Stdlib__hashtbl____t = (keyTransform, valueTransform, tbl) => {
  Vendor.Json.Array(Hashtbl.fold((key, value, result) => {
    [Vendor.Json.Array([keyTransform(key), valueTransform(value)]), ...result]
  }, tbl, [])->Belt.List.sort(compare))
};
