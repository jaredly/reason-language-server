

Runtime options:

- --override : replace the current version in the lockfile, even if incompatible

# `types.json` format

#### Example `types.json`:

```
{
  "version": 1,
  "engines": {
    "bs.json": "./src/Serde.re"
  },
  "entries": [
    {
      "file": "./src/Types.re",
      "type": "config"
    },
    {
      "file": "./src/Node.re",
      "type": "t",
      "publicName": "node"
    }
  ]
}
```

#### Full spec:

```
{
  "version": 1, // the current version of your types. Must be monotonically increasing integer
  // optional if there's only one engine (the types get inlined into the engine output file)
  "typesOutput": "./path/to/typesoutput.re",
  "engines": {
    "rex-json": "./path/to/outfile.re",
    "bs.json": "./path/to/outfile_bs.re",
  },
  "entries": [
    {
      "file": "path/to/source.re",
      "type": "typename", // or "Submodule.typename"
      // optional if "typename" is not in a submodule.
      "publicName": "someOtherName",
      // optional. defaults to the `global-engines` value
      "engines": ["rex-json"],
      // defaults to true. set to false to not create historical deserializers for this type.
      "history": true,
      // optional. defaults to the global minVersion
      "minVersion": 1,
    }
  ],
  // optional
  "minVersion": 1, // the minimum version to create deserializers & migrators for.
  // optional, defaults to "all of the engines". Use this to exclude an engine from most of the entries.
  "globalEngines": ["rex-json"],
}
```

# How does it work?

Type-digger has two phases. 1) generate/update lockfile. 2) generate serialization, migration, and deserialization code for all known types & versions.

## Generate/update lockfile

A lockfile consists of an array of "locked type maps", each corresponding to a "version" of your types.
A "locked type map" is a map from a "module path" (like `MyModule.SubModule.typename`) to a serialization of the type declaration, including any `@attributes`.

Type-digger first creates a "locked type map" for the current state of your types, starting with your "entry types", and recursively following any type references down to their definitions.
Then, if there's a current lockfile & the version in `types.json` has not been incremented, it checks for incompatible changes & errors out in that case. If the changes are compatible (see below), it overwrites the locked type map, and if the version number has been incremented since the last type Type-digger was run, it appends the type map to the array.
The whole array is then written out to the lockfile.

## Generate code!







# Engine versioning

In order for an engine to be able to modify the serialization representation, lockfiles also include the versions of the engines used.



When you run type-digger, it
- takes a snapshot of the currently defined types
- compares to the lockfile if it exists
  - if the lockfile's version is the same as the types.json version, and the current types are incompatible with the lockfiles types, then error out (the version in types.json must be incremented)

## What type changes are "compatible"
(e.g. don't require a version bump)

- *adding* a constructor to a variant type
- *adding* a row to a polymorphic variant type (TODO not yet supported)
- *adding* a new "entry" type

# How upgrading and configuration and compatability works.

Goal: A version of type-digger can produce compatible serde for any previous version.
Goal: A version of domain serde can *deserialize* data from any of its previous versions. (from entries that are marked as caring about history)


Example:

Say we have a types.lock.json with the following:

- v1 "rex-json-1" with domain types version D1
- v2 "rex-json-2" with domain types version D1
- v3 "rex-json-2" with domain types version D2
- v4 "rex-json-2" with domain types version D3

We need to produce a
- "serializeDog" only for the current types
- "deserializeDog" that can handle
  - D3 (easy)
  - D2
  - D1 @ rex-json-2
  - D1 @ rex-json-1

Structure of the output file might be something like:


```

module V3 (and V2 and V1) = {
  module LockedTypes = {
    ... locked types, not connected to currents
    ... welll actually I want the ones that haven't changed to be connected to the currents
  }
  ... deserializers
  ... migrators
}

module V4 = {
  module LockedTypes = {
    ... locked current types
  };
  ... serializers
  ... deserializers
  ... migrators
}

let serializeDog = dog => {
  wrapWithVersion(
    version,
    V4.serializeDog(dog)
  )
};

let deserializeDog = json => {
  let%Try (version, payload) = parseVersion(json);
  switch version {
    | 4 => V4.deserializeDog(payload)
    | 3 => V3.deserializeDog(payload)->V4.migrateDog
    | 2 => V2.deserializeDog(payload)->V3.migrateDog->V4.migrateDog
    | 1 => V1.deserializeDog(payload)
              ->V2.migrateDog->V3.migrateDog->V4.migrateDog
    | _ => Error("Unrecognized version")
  }
};

```



So if V4.dog === V1.dog, then I want them to be aliased to each other.
And if V3.cat === V4.cat, and V2.cat === V1.cat, I want those aliased.
So that the migrate functions are free.

Ummmm so maybe I'll produce a hash of the deep type? That sounds reasonable.
