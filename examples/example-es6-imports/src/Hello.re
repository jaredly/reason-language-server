let%opt.thing awesome = 10;
let%o awesome = 10;
let%oo awesome = 10;

let rec x = 10;

let x = 10
and y = 30;

let someLongName = 10;

%bs.raw "import things from 'hello'";
%bs.raw {|import moreThings from 'other'|};

let otherLongName = "string";

let x = [%bs.obj {a: 3}];

let r = Other.something;

let l = More.outer + More.n + Other.inner;

let n = More.n;

More.party;
string_of_bool;

Belt.Array.blit;
Js.Console.error;


/* let m = {More.a: 2, b: 32.}; */

module Something = {
  open Other;

  let m = {name: "Me", age: 0};
  let animal = Things(10);
  let other = Things(2);
  let me: animals = People("Hie");
  let x = something + 10;
  let r = m.name;

  let awesome = 20;
  if (true) {
    ()
  }
};

open Something;

let y = x + 10;

switch me {
| Things(n) => ()
| _ => ()
};


let z = x * x;

let aThing = 10 + Other.something;

/** Some docs about this **awesome** thing. */
let awesome = 100 + m.age;

let thing = "thing";

let transform = (x, y) => x ++ string_of_float(y);

let z = transform("hello ", 5.);

let zzz = 1;

let more = 20;

/** Something here */
let added = 10 + awesome;

open Other;

open Hashtbl;

/** Some more documentation about this */
let awesome = x => x + 2;

let a = [
  "hello",
  "my fine" ++ "folks",
  "in boonville"
];

let div = (~x, ~y, ~children, ()) => 10;

 let something = animal => switch animal {
   | blank => ()
 };

 something(animal);

let someFunction = (memorableName, {contents}) => {
  let innerMemorable = 20;
  memorableName + innerMemorable;
};

/* let awesome = 10000; */

/* let awesome = 111; */

let z = 10;

let z = find;

let z = later;

let m = Other.later;

for (_index in 0 to 10) {
  print_endline("hellO");
};

module OneOneOneOne  = {
  module TwoTwoTwoTwo = {
    let xxxxxxxxxx = 10;
  };
};
let r = OneOneOneOne.TwoTwoTwoTwo.xxxxxxxxxx;

type awesome = {
  one: string,
  two: float,
};

open OneOneOneOne.TwoTwoTwoTwo;

include OneOneOneOne.TwoTwoTwoTwo;

include More;

Other.oo.person.name;

type lots =
| Parties
| Plutocrats(int, float)
| Possums
| Oppossums;

let y = Some(10 + awesome(3));

let z = {contents: 30};
let party = {one: "one", two: 2.};

let {one, two} = party;

let thing = () => {
  34 + 43;
};

type more = awesome;

let {contents} = z;

switch (y) {
| Some(u) => ()
| None => ()
};

/* let x = [%raw " hello"]; */

let awesome = "hello";
