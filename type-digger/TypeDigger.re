
Printexc.record_backtrace(true);
open Lib;
module Json = Vendor.Json;
open DigTypes;
/* Log.spamError := true; */

let toJson = (base, src, name) => {
  let state = TopTypes.forRootPath(base);
  let uri = Utils.toUri(Filename.concat(base, src));
  let%try tbl = GetTypeMap.forInitialType(~state, uri, name);

  let json = Rpc.J.o(Hashtbl.fold(((moduleName, path, name), v, items) => {
    [(moduleName ++ ":" ++ String.concat(".", path) ++ ":" ++ name, 
    SerializeSimplerType.declToJson(SerializeSimplerType.sourceToJson, v)
    ), ...items]
  }, tbl, []));
  Files.writeFile("out.json", Json.stringifyPretty(json)) |> ignore;
  /* print_endline(); */
  Ok(())
};

let toSerializer = (base, src, name) => {
  let state = TopTypes.forRootPath(base);
  let uri = Utils.toUri(Filename.concat(base, src));
  let%try tbl = GetTypeMap.forInitialType(~state, uri, name);

  /* let decls = Hashtbl.fold((())) */
  Ok(())
};

switch (Sys.argv) {
  | [|_, src, name|] => {
    switch (toJson(Sys.getcwd(), src, name)) {
      | Result.Ok(()) => print_endline("Success")
      | Result.Error(message) => print_endline("Failed: " ++ message)
    }
  }
  | _ => failwith("Bad args")
}