
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
  let (version, payload) = parseVersion(json);
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
