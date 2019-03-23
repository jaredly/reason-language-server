open DigUtils;

let lockTypes = (~currentVersion, version, typeMap, lockedDeep) => {
  hashList(typeMap)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name) as ref, (_attributes, decl))) => {
      let alias =
        if (currentVersion == version
            || lockedDeep[currentVersion]->Upgrade.hashFind(ref)
            == lockedDeep[version]->Upgrade.hashFind(ref)) {
          Some(Serde.OutputType.unflatten([moduleName] @ modulePath @ [name]));
        } else if (version > 1
                   && lockedDeep[version - 1]->Upgrade.hashFind(ref)
                   == lockedDeep[version]->Upgrade.hashFind(ref)) {
          Some(
            Ldot(
              Lident(typesModuleName(version - 1)),
              Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name),
            ),
          );
        } else {
          None;
        };
      Serde.OutputType.outputDeclaration(
        ~alias,
        moduleName,
        modulePath,
        name,
        Serde.OutputType.showSource,
        decl,
      );
    });
};

let makeModule = (~currentVersion, ~lockedDeep, ~lockfile, version, typeMap) => {
  makeModule(
    typesModuleName(version),
    [
      Ast_helper.Str.type_(Recursive, lockTypes(~currentVersion, version, typeMap, lockedDeep)),
      ...if (version > 1) {
           let pastTypeMap = lockfile->TypeMapSerde.Config.Locked.getVersion(version - 1).typeMap;
           [
             Ast_helper.Str.value(
               Recursive,
               hashList(typeMap)
               ->Belt.List.sort(compare)
               ->Belt.List.keepMap((((moduleName, modulePath, name) as ref, decl)) =>
                   switch (pastTypeMap->Upgrade.hashFind(ref)) {
                   | Some(dPast) =>
                     Some(
                       Upgrade.makeUpgrader(
                         version,
                         pastTypeMap,
                         lockedDeep,
                         ~moduleName,
                         ~modulePath,
                         ~name,
                         decl,
                         dPast,
                       ),
                     )
                   | None => None
                   }
                 ),
             ),
           ];
         } else {
           [];
         },
    ],
  );
};