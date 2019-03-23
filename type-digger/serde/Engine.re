
module type T = {
  let declDeserializer:
    (
      ~renames: list((string, string)),
      ~moduleName: string,
      ~modulePath: list(string),
      ~name: string,
      ~inner: option(Parsetree.expression),
      SharedTypes.SimpleType.declaration(
        TypeMap.DigTypes.typeSource(TypeMap.DigTypes.shortReference),
      )
    ) =>
    Parsetree.value_binding;
  let declSerializer:
    (
      ~renames: list((string, string)),
      ~moduleName: string,
      ~modulePath: list(string),
      ~name: string,
      SharedTypes.SimpleType.declaration(
        TypeMap.DigTypes.typeSource((string, list(string), string)),
      )
    ) =>
    Parsetree.value_binding;
  let serializeTransformer: MakeSerializer.transformer(TypeMap.DigTypes.typeSource(TypeMap.DigTypes.shortReference));
  let deserializeTransformer: MakeDeserializer.transformer(TypeMap.DigTypes.typeSource(TypeMap.DigTypes.shortReference));
};