open DigUtils;

let lockTypes = (~currentVersion, version, typeMap, lockedDeep) => {
  hashList(typeMap)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name) as ref, (_attributes, decl))) => {
    let alias = if (currentVersion == version ||
      lockedDeep[currentVersion]->Upgrade.hashFind(ref) == lockedDeep[version]->Upgrade.hashFind(ref)
    ) {
      Some(Serde.OutputType.unflatten([moduleName] @ modulePath @ [name]));
    } else if (version > 1 &&
      lockedDeep[version - 1]->Upgrade.hashFind(ref) == lockedDeep[version]->Upgrade.hashFind(ref)
    ) {
      Some(Ldot(Lident(versionModuleName(version - 1)), Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name)))
    } else {
      None
    }
    Serde.OutputType.outputDeclaration(~alias, moduleName, modulePath, name, Serde.OutputType.showSource, decl)
  });
};
