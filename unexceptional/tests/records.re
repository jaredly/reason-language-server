
type holdsTwo = {
  one: unit => unit,
  two: unit => unit
};

let callsOne = holder => holder.one();

[@expectFailure]
[@pure]
let failer = () => {
  callsOne({
    one: () => failwith("Bad"),
    two: () => (),
  })
};

[@pure]
let passer = () => {
  callsOne({
    one: () => (),
    two: () => failwith("Not called though")
  })
};

[@pure]
let doesntCall = (v: holdsTwo) => v;

let overridesOne = v => {...v, one: () => ()};
let overridesOneWithFailer = v => {...v, one: () => failwith("Fail")};

[@pure]
let isFine = () => callsOne(overridesOne({one: () => failwith("bad"), two: () => failwith("yeah")}));

[@pure]
[@expectFailure]
let isBad = () => callsOne(overridesOneWithFailer({one: () => (), two: () => failwith("yeah")}));

/* Is that a thing I can do? */
[@pure]
[@expectFailure]
let alsoBad = () => {
  let m = overridesOneWithFailer({one: () => (), two: () => ()});
  let n = m.one;
  n();
};



/* Does this even make sense?
Like we might need to literally have a type system for this though.

 */