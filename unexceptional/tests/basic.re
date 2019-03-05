
let throwsFailure = () => failwith("Nope");

exception MyCustomException;

let throwsCustom = () => if (true) {raise(MyCustomException)} else { 10 };

[@expectFailure]
[@pure]
let doesntCatch = () => throwsCustom();

[@pure]
let catches = () => switch (throwsCustom()) {
  | exception MyCustomException => 5
  | m => m
};

[@pure]
let alsoCatches = () => try (throwsCustom()) {
  | MyCustomException => 5
};

[@expectFailure]
[@pure]
let d = (m) => if m {
  throwsFailure()
} else {
  throwsCustom()
};

[@pure]
let callsArgument = fn => fn();

let passingImpureFunction = {
  [@expectFailure]callsArgument(throwsFailure);
};

let passingPureFunction = {
  ignore(callsArgument(() => 5))
};

[@pure]
let callsCatchesFailure = fn => try (fn()) {
  | exception Failure(_) => 10
};

let passingFailureCaller = {
  callsCatchesFailure(throwsFailure)
};

let passingOtherCaller = {
  [@expectFailure]callsCatchesFailure(throwsCustom)
};


