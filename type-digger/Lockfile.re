
open TypeMapSerde.Config;
module Json = Vendor.Json;
module Locked = TypeMapSerde.Config.Locked;

let getStringPayload = payload => switch (Upgrade.getExpr(payload)) {
  | Some({Parsetree.pexp_desc: Pexp_constant(Pconst_string(text, _))}) => Some(text)
  | _ => None
};

let getRenames = attributes =>
  attributes->Belt.List.keepMap((({Location.txt}, payload)) =>
    switch (Util.Utils.split_on_char('.', txt)) {
    | ["rename", name] =>
      switch (getStringPayload(payload)) {
      | None => None
      | Some(string) => Some((name, string))
      }
    | _ => None
    }
  );

let compareAttributes = (one, two) => {
  let one = one->getRenames->Belt.List.sort(compare);
  let two = two->getRenames->Belt.List.sort(compare)
  one == two
};

let compareTypes = (oldType: SharedTypes.SimpleType.declaration('source), newType: SharedTypes.SimpleType.declaration('source)) => {
  oldType.name == newType.name &&
  oldType.variables == newType.variables && {
    // print_endline("Comparing");
    switch (oldType.body, newType.body) {
      | (Variant(olds), Variant(news)) =>
        olds->Belt.List.every(one => news->Belt.List.has(one, (==)))
      | _ => oldType.body == newType.body
    }
  }
};

/** Checks that all types in the old map are the same in the new map */
let allTypesPreserved = (oldLockedTypes, newLockedTypes) => {
  Hashtbl.fold((k, (attributes, v), good) => {
    good && Hashtbl.mem(newLockedTypes, k) && {
      let (attributes2, v2) = Hashtbl.find(newLockedTypes, k);
      compareTypes(v, v2) && compareAttributes(attributes, attributes2)
    }
  }, oldLockedTypes, true)
};


let parseLockfile = (~override, config, lockedEntries, currentTypeMap, lockFilePath) => {
  switch (Files.readFileResult(lockFilePath)) {
    | Error(_) => {
      Locked.versions: [|{
        typeMap: currentTypeMap,
        entries: lockedEntries,
      }|],
    }
    | Ok(contents) => {
      let json = Json.parse(contents);
      let%try_force lockfile = switch (TypeMapSerde.lockfileFromJson(json)) {
        | Error(m) => Error(String.concat("::", m))
        | Ok(v) => Ok(v)
      };
      let latestVersion = Locked.getLatestVersion(lockfile);
      if (latestVersion == config.version) {
        if (
          !allTypesPreserved(lockfile->Locked.getVersion(config.version).typeMap, currentTypeMap) &&
          !override
        ) {
          failwith("Types do not match lockfile! You must increment the version number in your types.json")
        } else {
          lockfile->Locked.updateVersion(~typeMap=currentTypeMap, ~entries=lockedEntries)
        }
      } else if (latestVersion + 1 == config.version) {
        lockfile->Locked.addVersion(~typeMap=currentTypeMap, ~entries=lockedEntries)
      } else {
        failwith("Version must be incremented by one")
      }
    }
  };
};
