

module V1 = {
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
};

module V2 = {
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
};

module V3 = {
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
};

module V4 = {
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
};

module Current = { include V4 };