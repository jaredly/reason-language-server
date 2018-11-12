/* stuff */

[@bs.module "fs"] external readFileSync: string => string = "";
[@bs.module "fs"] external writeFileSync: (string, string) => unit = "";

writeFileSync(
  "data." ++ string_of_int(Types.Current.v) ++ ".json",
  Js.Json.stringify(Serde.serializeHousehold(Types.Current.example)),
);

Js.log(
  Serde.deserializeHousehold(Serde.serializeHousehold(Types.Current.example))
  == Ok(Types.Current.example),
);

