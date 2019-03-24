
module V1 = {
  type person = {
    name: string,
    age: int,
    coords: (float, float),
    parents: option((person, person))
  };

  type pet =
    | Dog
    | Cat

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10, coords: (1., 2.), parents: None}], pets: [Dog]}
};

module V2 = {
  [@migrate.age person => float_of_int(person.age * 7)]
  type person = {
    name: string,
    age: float,
    coords: (float, float),
    parents: option((person, person))
  };

  type pet =
    | Dog
    | Cat

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10., coords: (1., 3.), parents: None}], pets: [Dog]}
};

module V2_1 = {
  [@migrate.age person => float_of_int(person.age * 7)]
  type person = {
    name: string,
    age: float,
    coords: (float, float),
    parents: option((person, person))
  };

  type pet =
    | Dog
    | Cat
    | Mouse

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10., coords: (1., 3.), parents: None}], pets: [Dog]}
};

module V3 = {
  type person = {
    name: string,
    age: float,
    coords: (float, float)
  };

  type pet =
    | Dog
    | Cat
    | Mouse

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10., coords: (2., 4.)}], pets: [Dog]}
};

module V4 = {
  type person = V3.person = {
    name: string,
    age: float,
    coords: (float, float)
  };

  type dogBreed =
    | Schnouser
    | Lab
    | Retriever
    | Poodle;

  [@migrate.Dog (Dog) => Dog(None)]
  type pet =
    | Dog(option(dogBreed))
    | Cat
    | Mouse

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10., coords: (5., 6.)}], pets: [Dog(Some(Schnouser))]}
};

module V5 = {
  type person = V3.person = {
    name: string,
    age: float,
    coords: (float, float)
  };

  [@migrate.Schnouser (Schnouser) => Schnouser("white")]
  type dogBreed =
    | Schnouser(string)
    | Lab
    | Retriever
    | Poodle

  type pet =
    | Dog(option(dogBreed))
    | Cat
    | Mouse
  
  type named('a) = {
    name: string,
    contents: 'a
  };

  [@migrate.county household => {name: "Nowhere", contents: 0}]
  type household = {
    people: list(person),
    pets: list(pet),
    county: named(int)
  };

  let example = {
    people: [{name: "Me", age: 10., coords: (5., 6.)}],
    pets: [Dog(Some(Schnouser("black")))],
    county: {name: "Bearland", contents: 5}
  };
};

module V6 = {
  type person = {
    name: string,
    age: float,
    coords: (float, float)
  };

  [@migrate.Dog (Dog(dogBreed)) => Dog]
  [@rename.Dog "a-cat"]
  type pet =
    | Dog
    | Cat
    | Mouse;

  [@migrate (contentsMigrator, named) => {name: named.name, contents: contentsMigrator(named.contents), isClosed: false}]
  [@rename.name "the name"]
  type named('a) = {
    name: string,
    contents: 'a,
    isClosed: bool,
  };

  type what('a) = Now('a);

  [@migrate.visitors household => []]
  [@migrate.what household => Now("4")]
  type household = {
    people: list(person),
    pets: list(pet),
    what: what(string),
    visitors: list(person),
    county: named(int)
  };

  let example = {
    people: [{name: "Me", age: 10., coords: (5., 6.)}],
    pets: [Dog, Mouse],
    what: Now("5"),
    visitors: [{name: "Friend", age: 11.5, coords: (1., 6.)}],
    county: {name: "Bearland", contents: 5, isClosed: false}
  };
};

/*
Issues
- can't export parameterized types

 */

module V7 = {
  [@migrate.thing (_) => `one]
  type person = {
    name: string,
    age: float,
    thing: [`one | `two(int) | `three(float, string)],
    coords: (float, float)
  };

  [@rename.Dog "a-cat"]
  type pet =
    | Dog
    | Cat
    | Mouse;

  [@rename.name "the name"]
  [@migrate.other (_) => None]
  type named('a) = {
    name: string,
    contents: 'a,
    isClosed: bool,
    other: option('a)
  };

  [@migrate.Now (Now(a)) => Now(_migrator_a(a), 5)]
  type what('a) = Now('a, int);

  /* designed to trigger record key collision */
  type one = {key: string};
  type two = {key: string};

  [@migrate.one h => Now({key: "one"}, 5)]
  [@migrate.two h => Now({key: "two"}, 5)]
  type household = {
    one: what(one),
    two: what(two),
    people: list(person),
    pets: list(pet),
    what: what(string),
    visitors: list(person),
    county: named(int)
  };

  let example = {
    one: Now({key: "one"}, 5),
    two: Now({key: "two"}, 5),
    people: [{name: "Me", age: 10., coords: (5., 6.), thing: `one}],
    pets: [Dog, Mouse],
    what: Now("5", 5),
    visitors: [{name: "Friend", age: 11.5, coords: (1., 6.), thing: `one}],
    county: {name: "Bearland", contents: 5, isClosed: false, other: None}
  };
};

module All = {
  type normalRecord = {
    a: int,
    b: string,
    c: (int, (string, float)),
    d: array(int),
    e: list(float),
    f: option(int)
  };
  type recursive = A | B(recursive);
  type parameterizedRecord('a, 'b) = {
    a: 'a,
    b: 'b,
    c: (int, float),
    d: recursive,
  };
  type normalVariant = A | B | C(int) | D(normalRecord);
  type parameterizedVariant('a, 'b) = PA | PB('a) | PC('a, 'b) | PD(parameterizedRecord('a, 'b)) | PE(normalVariant) | PF(normalRecord);
  type top = {
    contents: parameterizedVariant(int, array(float)),
    title: string,
  };
  type rename = top;
};
