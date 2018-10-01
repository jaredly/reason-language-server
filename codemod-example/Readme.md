# Example Codemod

Here's an example of a type-driven codemod!

It finds functions that have a return type of `Belt.Result.t(int, string)`, and wraps the error payloads with `Unspecified`.


So, for the file:
```reason
open Result;

let changeMe = (a, b, c) => Error("hello");

let nonCollapsible = something =>
  if (something > 2) {
    Result.Ok(something);
  } else {
    Error("bad news");
  };
```


It rewrites as:
```reason
open Result;

let changeMe = (a, b, c) => Error("hello");

let nonCollapsible = something =>
  if (something > 2) {
    Result.Ok(something);
  } else {
    Error(Unspecified("bad news"));
  };
```


Note that the first `Error()` was not wrapped, because we specified that the `'ok` type had to be `int`. We could have left the `ok` type argument blank, and it would have picked that one up too.