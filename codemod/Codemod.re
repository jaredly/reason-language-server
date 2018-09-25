
open Parsetree;

open Helpers;

/*

Ok, so I'm finding that for this codemod, I want rather more information.
Like, every expression (which I'm not currently doing).

 */

/*

"Convert all Error(x) to Error(Unspecified(x)) if the function's return type is Graphql.Schema.io_field"

 */

let replaceErrors = (ctx, expr) =>
  expr
  ->mapExpr((mapper, expr) => {
      switch (expr.pexp_desc) {
      | Pexp_construct({txt: Longident.Lident("Error")} as lid, Some({pexp_desc: Pexp_tuple([arg])})) =>
        let typ = ctx->getExprType(arg);
        if (typ->matchesType("string")) {
          Some(
            Ast_helper.Exp.construct(
              lid,
              Some(Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("Unspecified")), Some(arg))),
            ),
          );
        } else {
          None;
        }
      | _ => None
      };
    });

let modify = (ctx, structure) => {
  structure->strExpr((mapper, expr) =>
      expr->mapFnExpr((mapper, args, body) => {
          if (ctx->getExprType(body)->matchesType("Belt.Result.t")) {
            Some((args, ctx->replaceErrors(body)));
          } else {
            None;
          };
        })
      ->Some
    );
};

switch (Sys.argv) {
  | [|_, root|] =>
    print_endline("Running on project: " ++ root);
    Runner.runCodeMod(
      root,
      (path, moduleName) => Filename.extension(path) == ".re",
      modify
    );
  | _ => ()
};

