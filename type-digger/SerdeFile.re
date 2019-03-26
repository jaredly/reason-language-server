open TypeMapSerde.Config;
let loc = Location.none;
open DigUtils;
open Longident;
let mknoloc = Location.mknoloc;

let expIdent = lident => Ast_helper.Exp.ident(Location.mknoloc(lident));

let makeSubModules = (~config, ~state) => {
  let modules = config.entries
  ->Belt.List.map(({file, type_, publicName}) => {
      let publicName =
        switch (publicName) {
        | None => type_
        | Some(name) => name
        };
      let uri = Utils.toUri(Filename.concat(Sys.getcwd(), file));
      let%try_force (moduleName, modulePath, name) =
        TypeMap.GetTypeMap.fileToReference(~state, uri, type_);
      let lockedTypeName = Serde.OutputType.makeLockedTypeName(moduleName, modulePath, name);
      open Ast_helper;
      Str.module_(
        Mb.mk(
          Location.mknoloc(String.capitalize_ascii(publicName)),
          {
            pmod_desc: Pmod_structure([%str 
              type t = [%t Typ.constr(mknoloc(Ldot(Lident(typesModuleName(config.version)), lockedTypeName)), [])];
              let serialize = [%e expIdent(Lident("serialize" ++ String.capitalize_ascii(publicName)))];
              let deserialize = [%e expIdent(Lident("deserialize" ++ String.capitalize_ascii(publicName)))];
             ]),
            pmod_loc: Location.none,
            pmod_attributes: []
          }
        )
    )
  });
  [Ast_helper.Str.module_(
    Ast_helper.Mb.mk(
      mknoloc("Modules"),
      {
        pmod_desc: Pmod_structure(modules),
        pmod_loc: Location.none,
        pmod_attributes: []
      }
    )
  )]
};

let makeConverters = (~config, ~state) =>
  config.entries
  ->Belt.List.map(({file, type_, publicName}) => {
      let publicName =
        switch (publicName) {
        | None => type_
        | Some(name) => name
        };
      let uri = Utils.toUri(Filename.concat(Sys.getcwd(), file));
      let%try_force (moduleName, modulePath, name) =
        TypeMap.GetTypeMap.fileToReference(~state, uri, type_);
      let des = Serde.MakeDeserializer.transformerName(~moduleName, ~modulePath, ~name);
      let ser = Serde.MakeSerializer.transformerName(~moduleName, ~modulePath, ~name);
      let fullName = Serde.MakeDeserializer.fullName(~moduleName, ~modulePath, ~name);
      open Longident;

      let makeUpgrader = (current, target) => {
        open Parsetree;
        let rec loop = current =>
          if (current === target) {
            %expr
            Belt.Result.Ok(data);
          } else {
            %expr
            {
              let data =
                [%e
                  expIdent(Ldot(Lident(typesModuleName(current + 1)), "migrate_" ++ fullName))
                ](
                  data,
                );
              %e
              loop(current + 1);
            };
          };
        loop(current);
      };

      Ast_helper.Str.value(
        Asttypes.Nonrecursive,
        [
          Ast_helper.Vb.mk(
            Ast_helper.Pat.var(Location.mknoloc("serialize" ++ String.capitalize_ascii(publicName))),
            [%expr
              data =>
                wrapWithVersion(
                  currentVersion,
                  [%e
                    Ast_helper.Exp.ident(
                      Location.mknoloc(
                        Longident.Ldot(
                          Longident.Lident(versionModuleName(config.version)),
                          ser,
                        ),
                      ),
                    )
                  ](
                    data,
                  ),
                )
            ],
          ),
          Ast_helper.Vb.mk(
            Ast_helper.Pat.var(Location.mknoloc("deserialize" ++ String.capitalize_ascii(publicName))),
            [%expr
              data =>
                switch (parseVersion(data)) {
                | Belt.Result.Error(err) => Belt.Result.Error([err])
                | [@implicit_arity] Ok(version, data) =>
                  %e
                  Ast_helper.Exp.match(
                    [%expr version],
                    {
                      let rec loop = n =>
                        if (n < 1) {
                          [
                            Ast_helper.Exp.case(
                              [%pat? _],
                              [%expr
                                Belt.Result.Error([
                                  "Unexpected version " ++ string_of_int(version),
                                ])
                              ],
                            ),
                          ];
                        } else {
                          [
                            Ast_helper.Exp.case(
                              Ast_helper.Pat.constant(
                                Parsetree.Pconst_integer(string_of_int(n), None),
                              ),
                              switch%expr (
                                [%e expIdent(Ldot(Lident(versionModuleName(n)), des))](data)
                              ) {
                              | Belt.Result.Error(error) => Belt.Result.Error(error)
                              | Ok(data) =>
                                %e
                                makeUpgrader(n, config.version)
                              },
                            ),
                            ...loop(n - 1),
                          ];
                        };
                      loop(config.version);
                    },
                  )
                }
            ],
          ),
        ],
      );
    });

let makeDeserializers = (~helpers, maker, tbl, lockedDeep, version) => {
  hashList(tbl)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name) as ref, (attributes, decl))) => {
      let mine = lockedDeep[version]->Hashtbl.find(ref);
      let prevVersion =
        version == 1
          ? None
          : {
            switch (lockedDeep[version - 1]->Upgrade.hashFind(ref)) {
            | Some(prev) when prev == mine => Some(version - 1)
            | _ => None
            };
          };
      switch (prevVersion) {
      | None =>
        maker(
          ~helpers,
          ~renames=attributes->Lockfile.getRenames,
          ~moduleName,
          ~modulePath,
          ~name,
          ~inner=None,
          decl,
        )
      | Some(prevVersion) =>
        let tname = Serde.MakeDeserializer.transformerName(~moduleName, ~modulePath, ~name);
        let inner =
          Ast_helper.Exp.ident(
            Location.mknoloc(
              Longident.Ldot(Longident.Lident(versionModuleName(prevVersion)), tname),
            ),
          );
        maker(
          ~helpers,
          ~renames=attributes->Lockfile.getRenames,
          ~moduleName,
          ~modulePath,
          ~name,
          ~inner=Some(inner),
          decl,
        );
      // Ast_helper.Vb.mk(
      //   Ast_helper.Pat.var(Location.mknoloc(tname)),
      //   inner
      // )
      };
    });
};

let makeModule = (moduleName, contents) =>
  Ast_helper.Str.module_(
    Ast_helper.Mb.mk(
      Location.mknoloc(moduleName),
      Ast_helper.Mod.mk(Parsetree.Pmod_structure(contents)),
    ),
  );

let makeFns = (~helpers, maker, tbl) => {
  hashList(tbl)
  ->Belt.List.sort(compare)
  ->Belt.List.map((((moduleName, modulePath, name), (attributes, decl))) =>
      maker(~helpers, ~renames=attributes->Lockfile.getRenames, ~moduleName, ~modulePath, ~name, decl)
    );
};

let makeFullModule =
    (~engine, ~helpers, ~currentVersion, ~lockedDeep, ~lockfile, version, {Locked.typeMap}) => {
  /* TODO respect engineVersion */
  module Engine = (val engine: Serde.Engine.T);
  let fns = makeDeserializers(~helpers, Engine.declDeserializer, typeMap, lockedDeep, version);
  let fns = version == currentVersion ? fns @ makeFns(~helpers, Engine.declSerializer, typeMap) : fns;

  makeModule(
    versionModuleName(version),
    [
      Ast_helper.Str.open_(
        Ast_helper.Opn.mk(Location.mknoloc(Longident.Lident(typesModuleName(version)))),
      ),
      Ast_helper.Str.value(Recursive, fns),
    ],
  );
};