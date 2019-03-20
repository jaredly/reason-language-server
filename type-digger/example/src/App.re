/* stuff */

[@bs.module "fs"] external readFileSync: string => string = "";
[@bs.module "fs"] external writeFileSync: (string, string) => unit = "";

/* TODO check to see if a previous _version is parseable */
writeFileSync(
  "data." ++ Types.version ++ ".json",
  Js.Json.stringify(Serde.serializeHousehold(Types.example)),
);

assert(
  Serde.deserializeHousehold(Serde.serializeHousehold(Types.example))
  == Ok(Types.example),
);

Types.all_versions->List.forEach(version => {
  let raw = readFileSync("data." ++ version ++ ".json");
  let json = Js.Json.parseExn(raw);
  switch (Serde.deserializeHousehold(json)) {
    | Ok(result) => Js.log4(version, "Good", Js.Json.stringifyAny(result), Js.Json.stringify(json))
    | Error(error) => Js.log3(version, "Failed", error)
  }
})

/*
This was just checking the quality of error messagse.

let rawGood = [%bs.raw {|
[6,{"people":[{"name":"Me","age":10,"coords":[5,6]}],"pets":[["Dog"],["Mouse"]],"visitors":[{
  "name":"Friend","age":11.5,"coords":[1,6]
}],"county":{"name":"Bearland","contents":5,"isClosed":false}}]
|}];

let rawBad = [%bs.raw {|
[6,{"people":[{"name":"Me","age":10,"coords":[5,6]}],"pets":[["Dog"],["Mouse"]],"visitors":[{
  "name":"Friend","age":11.5,"coords":["hi",6]
}],"county":{"name":"Bearland","contents":5,"isClosed":false}}]
|}];

switch (Serde.deserializeHousehold(rawBad)) {
  | Ok(_) => assert(false)
  | Error(error) => Js.log2("Error message: ", Belt.List.toArray(error))
} */
