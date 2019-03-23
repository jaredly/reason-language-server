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

let dumpExpr = Serde.serializeSimpleExpr;
let dumpDecl = Serde.serializeSimpleDeclaration;
let parseVersion = Serde.parseVersion;

module Config = Config;