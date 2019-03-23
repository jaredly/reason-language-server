let lockfileToJson = Serde.serializeSerializableLockfile;
let lockfileFromJson = Serde.deserializeSerializableLockfile;

let configToJson = Serde.serializeT;
let configFromJson = Serde.deserializeT;

let checkVersion = (~upvert, ~configPath, config, json) => {
  let version = Serde.parseVersion(json);
  switch (version) {
  | Ok((version, _)) when version != Serde.currentVersion =>
    if (upvert) {
      Util.Files.writeFileExn(configPath, Vendor.Json.stringifyPretty(configToJson(config)));
    } else {
      print_endline("Notice: You're using an outdated config schema. To upvert, pass --upvert.");
    }
  | _ => ()
  };
};

let dumpExpr = expr => {
  // failwith("a")
  Serde.serializeSimpleExpr(
    expr,
    // Serde.serialize_SharedTypes__SimpleType__expr(Serde.serialize_TypeMap__DigTypes____typeSource(Serde.serialize_TypeMap__DigTypes____shortReference));
  );
};

let dumpDecl = expr => {
  // failwith("a")
  Serde.serializeSimpleDeclaration(
    expr,
    // Serde.serialize_SharedTypes__SimpleType__expr(Serde.serialize_TypeMap__DigTypes____typeSource(Serde.serialize_TypeMap__DigTypes____shortReference));
  );
};

let parseVersion = Serde.parseVersion;

module Config = Config;