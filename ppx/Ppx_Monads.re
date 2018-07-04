
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
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "map" | "awaitWrap"}, contents)), pexp_loc},
          next
        )
      | Pexp_sequence(
          {
            pexp_desc: Pexp_extension(({txt: "bind" | "await"}, contents)),
            pexp_loc,
            pexp_attributes: [({txt: "wrap"}, _), ..._]
          },
          next
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        [%expr Let_syntax.map([%e expr], ~f=([%p pat]) => [%e mapper.expr(mapper, next)])]
      | Pexp_extension(({txt: "map" | "awaitWrap"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        [%expr Let_syntax.map([%e expr], ~f=([%p pat]) => ())]
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "bind" | "await"}, contents)), pexp_loc},
          next
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        [%expr Let_syntax.bind([%e expr], ~f=([%p pat]) => [%e mapper.expr(mapper, next)])]
      | Pexp_extension(({txt: "bind" | "await"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        [%expr Let_syntax.bind([%e expr], ~f=([%p pat]) => ())]
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "consume"}, contents)), pexp_loc, pexp_attributes},
          next
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        let thenn =
          switch pexp_attributes {
          | [({txt: "then"}, contents)] => getExpr(contents, pexp_loc)
          | _ => [%expr ()]
          };
        [%expr
          {
            Let_syntax.consume([%e expr], ~f=([%p pat]) => [%e mapper.expr(mapper, next)]);
            [%e thenn]
          }
        ]
      | Pexp_extension(({txt: "consume"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        let thenn =
          switch expr.pexp_attributes {
          | [({txt: "then"}, contents)] => getExpr(contents, expr.pexp_loc)
          | _ => [%expr ()]
          };
        [%expr
          {
            Let_syntax.consume([%e expr], ~f=([%p pat]) => ());
            [%e thenn]
          }
        ]
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      }
  };

let () = Ast_mapper.run_main(mapper);