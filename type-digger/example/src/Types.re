
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
  type person = {
    name: string,
    age: float,
  };

  type dogBreed =
    | Schnouser
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

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog(None)]}
};

module Current = { include V4 };