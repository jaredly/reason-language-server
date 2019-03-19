
let lockfileToJson = Serde.serializeSerializableLockfile;
let lockfileFromJson = Serde.deserializeSerializableLockfile;

let configToJson = Serde.serializeT;
let configFromJson = Serde.deserializeT;

let dumpExpr = (_) => {
  failwith("a")
  // Serde.serialize_SharedTypes__SimpleType__expr(Serde.serialize_TypeMap__DigTypes____typeSource(Serde.serialize_TypeMap__DigTypes____shortReference));
};

module Config = Config;