
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
    | PStr([{pstr_desc: Pstr_value(Nonrecursive, bindings), pstr_loc}]) => bindings
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

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) =>
      switch expr.pexp_desc {
      | Pexp_extension(({txt: (
        "opt" | "opt_wrap" | "opt_consume"
        | "try" | "try_wrap" | "try_consume"
        | "await" | "await_wrap" | "await_consume"
        ) as txt}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_let(Nonrecursive, bindings, continuation)}, attributes)}]))) => {
        let front = switch (txt) {
          | "opt" => [%expr Monads.Option.bind]
          | "opt_wrap" => [%expr Monads.Option.map]
          | "opt_consume" => [%expr Monads.Option.consume]
          | "try" => [%expr Monads.Result.bind]
          | "try_wrap" => [%expr Monads.Result.map]
          | "try_consume" => [%expr Monads.Result.consume]
          | "await" => [%expr Monads.Promise.bind]
          | "await_wrap" => [%expr Monads.Promise.map]
          | "await_consume" => [%expr Monads.Promise.consume]
          | _ => assert(false)
        };
        let (pat, expr) = process_bindings(bindings);
        [%expr [%e front]([%e mapper.expr(mapper, expr)], ~f=([%p pat]) => [%e mapper.expr(mapper, continuation)])]
      }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      }
  };

let () = Ast_mapper.run_main(mapper);