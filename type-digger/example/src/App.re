/* stuff */

[@bs.module "fs"] external readFileSync: string => string = "";
[@bs.module "fs"] external writeFileSync: (string, string) => unit = "";

writeFileSync(
  "data." ++ string_of_int(Types.v) ++ ".json",
  Js.Json.stringify(Serde.serializeHousehold(Types.example)),
);

Js.log(
  Serde.deserializeHousehold(Serde.serializeHousehold(Types.example))
  == Ok(Types.example),
);

for (i in 1 to Types.v) {
  let raw = readFileSync("data." ++ string_of_int(i) ++ ".json");
  let json = Js.Json.parseExn(raw);
  switch (Serde.deserializeHousehold(json)) {
    | Ok(result) => Js.log3(i, "Good", result)
    | Error(error) => Js.log3(i, "Failed", error)
  }

}
