
module V1 = {
  let v = 1;
  type person = {
    name: string,
    age: int,
  };

  type pet =
    | Dog
    | Cat

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10}], pets: [Dog]}
};

module V2 = {
  let v = 2;
  [@migrate.age person => float_of_int(person.age)]
  /* [@migrate person => {name: person.name, age: float_of_int(person.age)}] */
  type person = {
    name: string,
    age: float,
  };

  type pet =
    | Dog
    | Cat

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog]}
};

module V3 = {
  let v = 3;
  type person = {
    name: string,
    age: float,
  };

  type pet =
    | Dog
    | Cat
    | Mouse

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog]}
};

module V4 = {
  let v = 4;
  type person = V3.person = {
    name: string,
    age: float,
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

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog(Some(Schnouser))]}
};

module V5 = {
  let v = 5;
  type person = V3.person = {
    name: string,
    age: float,
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

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog(Some(Schnouser("black")))]}
};

module V6 = {
  let v = 6;
  type person = V3.person = {
    name: string,
    age: float,
  };

  [@migrate.Dog (Dog(dogBreed)) => Dog]
  type pet =
    | Dog
    | Cat
    | Mouse;

  [@migrate.visitors household => []]
  type household = {
    people: list(person),
    pets: list(pet),
    visitors: list(person),
  };

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog, Mouse], visitors: [{name: "Friend", age: 11.5}]}
};
