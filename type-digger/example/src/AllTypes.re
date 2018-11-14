
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
  [@upgrade person => {name: person.name, age: float_of_int(person.age)}]
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



/*

type person
type pet
type *household

type person
type dogBreed
type pet
type *household



*/



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

  [@upgrade pet => switch pet {
    | Dog => Dog(None)
    | Cat => Cat
    | Mouse => Mouse
  }]
  type pet =
    | Dog(option(dogBreed))
    | Cat
    | Mouse

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog(None)]}

  /* let upgrade_Current_pet: V3.pet => pet = data => {
    switch data {
      | Dog => Dog(None)
      | Cat => Cat
      | Mouse => Mouse
    }
  };

  let upgrade_Current_person: V3.person => person = data => data;

  let upgrade_Current_household: V3.household => household = ({people, pets}) => {
    people: people->Belt.List.map(upgrade_Current_person),
    pets: pets->Belt.List.map(upgrade_Current_pet),
  }; */

};

module V5 = {
  let v = 5;
  type person = V3.person = {
    name: string,
    age: float,
  };

  [@upgrade breed => switch breed {
    | Schnouser => Schnouser("white")
    | Lab => Lab
    | Retriever => Retriever
    | Poodle => Poodle
  }]
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

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog(None)]}

  /* let upgrade_Current_dogBreed: V4.dogBreed => dogBreed = data => switch data {
    | Schnouser => Schnouser("white")
    | Lab => Lab
    | Retriever => Retriever
    | Poodle => Poodle
  };

  let upgrade_Current_pet: V4.pet => pet = data => {
    switch data {
      | Dog(Some(dogBreed)) => Dog(Some(upgrade_Current_dogBreed(dogBreed)))
      | Dog(None) => Dog(None)
      | Cat => Cat
      | Mouse => Mouse
    }
  };

  let upgrade_Current_person: V4.person => person = data => data;

  let upgrade_Current_household: V4.household => household = ({people, pets}) => {
    people: people->Belt.List.map(upgrade_Current_person),
    pets: pets->Belt.List.map(upgrade_Current_pet),
  }; */
};

module V6 = {
  let v = 6;
  type person = V3.person = {
    name: string,
    age: float,
  };

  [@upgrade (pet => switch pet {
  | Dog(dogBreed) => Dog
  | Cat => Cat
  | Mouse => Mouse
  })]
  type pet =
    | Dog
    | Cat
    | Mouse

  type household = {
    people: list(person),
    pets: list(pet),
  };

  let example = {people: [{name: "Me", age: 10.}], pets: [Dog]}

  /* let upgrade_Current_pet: V5.pet => pet = data => {
    switch data {
      | Dog(_) => Dog
      | Cat => Cat
      | Mouse => Mouse
    }
  };

  let upgrade_Current_person: V5.person => person = data => data;

  let upgrade_Current_household: V5.household => household = ({people, pets}) => {
    people: people->Belt.List.map(upgrade_Current_person),
    pets: pets->Belt.List.map(upgrade_Current_pet),
  }; */
};
