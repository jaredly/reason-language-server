
/**
Things I would like:

// maybe this is overkill? also probably harder to parse
switch%opt (somethingOptional) {
| theContents =>
};

// so each non-wildcard branch is wrapped in `Some`. Is this too weird?
switch%orNone (x) {
  | each => case
  | doesntNeed => toBe
  | aSome => atTheEnd
  | _ => None
}

Alsoooo I really want to be able to provide
hover-for-explanation for %ppx extension points.
How could I do that in a general way?

Done!!! As long as the ppx drops a `[@ocaml.explanation "some text"]`
somewhere, the `loc` of attribute's `loc(string)` bit will be used to
show the hover text that is the context of the attribute.

[@ocaml.explanation {|

```
let%opt name = value;
otherStuff
```
is transformed into
```
switch (value) {
  | None => None
  | Some(name) =>
    otherStuff
}
```
This means that `otherStuff` needs to end with an optional.

If you want `otherStuff` to be automatically wrapped in `Some()`,
then use `let%opt_wrap`.
Alternatively, if you are just performing a side effect, and want
the result of the whole thing to be unit, use `let%consume`.
|}]

 */

open Migrate_parsetree
open OCaml_402.Ast

/***
 * https://ocsigen.org/lwt/dev/api/Ppx_lwt
 * https://github.com/zepalmer/ocaml-monadic
 */
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

let rec process_bindings = (bindings) =>
  Parsetree.(
    switch bindings {
    | [] => assert false
    | [binding] => (binding.pvb_pat, binding.pvb_expr)
    | [binding, ...rest] =>
      let (pattern, expr) = process_bindings(rest);
      (
        Ast_helper.Pat.tuple([binding.pvb_pat, pattern]),
        [%expr Let_syntax.join2([%e binding.pvb_expr], [%e expr])]
      )
    }
  );

let process_let = (contents, loc) => {
  open Parsetree;
  let bindings =
    switch contents {
    | PStr([{pstr_desc: Pstr_value(Nonrecursive, bindings)}]) => bindings
    | _ => fail(loc, "extension must contain a nonrecursive let binding")
    };
  process_bindings(bindings)
};

let getExpr = (contents, loc) =>
  Parsetree.(
    switch contents {
    | PStr([{pstr_desc: Pstr_eval(expr, _)}]) => expr
    | _ => fail(loc, "@else must contain an expression")
    }
  );

let opt_explanation = {|
Optional declaration sugar:
```
let%opt name = value;
otherStuff
```
is transformed into
```
switch (value) {
| None => None
| Some(name) =>
  otherStuff
}
```
This means that `otherStuff` needs to have type `option`.

If you want `otherStuff` to be automatically wrapped in `Some()`,
then use `let%opt_wrap`.
Alternatively, if you are just performing a side effect, and want
the result of the whole thing to be unit, use `let%consume`.
|};

let opt_wrap_explanation = {|
Optional declaration sugar:
```
let%opt_wrap name = value;
otherStuff
```
is transformed into
```
switch (value) {
| None => None
| Some(name) => Some({
    otherStuff
  })
}
```
The `wrap` suffix means that the `otherStuff` will be automatically
wrapped in a `Some`.

If you don't want this wrapping, then use `let%opt`.
Alternatively, if you are just performing a side effect, and want
the result of the whole thing to be unit, use `let%consume`.
|};

let opt_consume_explanation = {|
Optional declaration sugar:
```
let%consume name = value;
otherStuff
```
is transformed into
```
switch (value) {
| None => ()
| Some(name) =>
  otherStuff
}
```
This is intented for performing side-effects only -- `otherStuff`
must end up as type `unit`.
|};

let mapper =
  Parsetree.{
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) =>
      switch expr.pexp_desc {
      | Pexp_extension(({txt: (
        "opt" | "opt_wrap" | "opt_consume" | "opt_force"
        | "try" | "try_wrap" | "try_consume" | "try_force"
        | "await" | "await_wrap" | "await_consume"
        ) as txt, loc}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_let(Nonrecursive, bindings, continuation)}, _)}]))) => {
        let (front, explanation) = switch (txt) {
          | "opt" => ([%expr Monads.Option.bind], opt_explanation)
          | "opt_wrap" => ([%expr Monads.Option.map], opt_wrap_explanation)
          | "opt_consume" => ([%expr Monads.Option.consume], opt_consume_explanation)
          | "opt_force" => ([%expr Monads.Option.force], "Force an optional. Throws an error if None")
          | "try" => ([%expr Monads.Result.bind], "Sugar for the Result type")
          | "try_wrap" => ([%expr Monads.Result.map], "Sugar for the Result type - auto-wraps in `Ok()`")
          | "try_consume" => ([%expr Monads.Result.consume], "Sugar for the Result type - side-effectful version")
          | "try_force" => ([%expr Monads.Result.force], "Sugar for the Result type - force a result")
          | "await" => ([%expr Monads.Promise.bind], "Sugar for Promises")
          | "await_wrap" => ([%expr Monads.Promise.map], "Sugar for Promises - auto-wraps in `Promise.resolve`")
          | "await_consume" => ([%expr Monads.Promise.consume], "Sugar for Promises - just for side effects. Throws on error")
          | _ => assert(false)
        };
        let (pat, expr) = process_bindings(bindings);
        Ast_helper.Exp.attr(
          [%expr [%e front]([%e mapper.expr(mapper, expr)], ~f=([%p pat]) => [%e mapper.expr(mapper, continuation)])],
          ({txt: "ocaml.explanation", loc}, PStr([
            Ast_helper.Str.eval(Ast_helper.Exp.constant(Const_string(explanation, None)))
          ]))
        )
      }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      }
  };

let () = Driver.register(~name="ppx_monads", ~args=[], Versions.ocaml_402, (_config, _cookies) => mapper);