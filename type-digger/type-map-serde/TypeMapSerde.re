
let lockfileToJson = Serde.serialize_TypeMapSerde__Config____serializableLockfile;
let lockfileFromJson = Serde.deserialize_TypeMapSerde__Config____serializableLockfile;

let configToJson = Serde.serialize_TypeMapSerde__Config____t;
let configFromJson = Serde.deserialize_TypeMapSerde__Config____t;

let dumpExpr = Serde.serialize_SharedTypes__SimpleType__expr(Serde.serialize_TypeMap__DigTypes____typeSource(Serde.serialize_TypeMap__DigTypes____shortReference));

module Config = Config;