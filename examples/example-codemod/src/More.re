/** Toplevel docs */;

open Result;

let changeMe = (a, b, c) => Error(Unspecified("hello"));

let nonCollapsible = something =>
  if (something > 2) {
    Result.Ok(something);
  } else {
    Error(Unspecified("bad news"));
  };
