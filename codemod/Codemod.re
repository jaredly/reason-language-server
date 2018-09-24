
open Parsetree;

open Helpers;

/*

Ok, so I'm finding that for this codemod, I want rather more information.
Like, every expression (which I'm not currently doing).

 */

/*

"Convert all Error(x) to Error(Unspecified(x)) if the function's return type is Graphql.Schema.io_field"

 */

module Convert = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_404, Migrate_parsetree.OCaml_406);
module ConvertBack = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_406, Migrate_parsetree.OCaml_404);

let runCodeMod = (root, pathChecker, modify) => {
  let state = Lib.TopTypes.empty();
  let state = {...state, settings: {...state.settings,
    recordAllLocations: true,
    autoRebuild: false,
  }};
  print_endline("Setting up a package");
  let%try_force package = Lib.State.newPackageForRoot(state, root);
  print_endline("Got a package");

  let fullForCmt = (switch (package.compilerVersion) {
    | Lib.BuildSystem.V402 => Process_402.fullForCmt
    | V406 => Process_406.fullForCmt
  })(~allLocations=true);

  package.Lib.TopTypes.localModules->Belt.List.forEach(moduleName => {
    let%opt_force paths = Utils.maybeHash(package.pathsForModule, moduleName);
    let%opt_consume (cmt, src) = SharedTypes.getImpl(paths);
    print_endline(src);

    if (pathChecker(src, moduleName)) {
      let%try_force full = fullForCmt(~moduleName, cmt, src, x => x);
      let ctx = {state, package, full};

      let file_chan = open_in(src);
      seek_in(file_chan, 0);
      let lexbuf = Lexing.from_channel(file_chan);
      let (structure, comments) = Reason_toolchain.RE.implementation_with_comments(lexbuf);
      close_in(file_chan);
      let structure = Convert.copy_structure(structure);

      /* let%try_force structure = Process_406.parseTreeForCmt(cmt); */
      let structure = modify(ctx, structure);

      let structure = ConvertBack.copy_structure(structure);
      /* Pprintast.structure(Format.str_formatter, structure); */
      Reason_toolchain.RE.print_implementation_with_comments(Format.str_formatter, (structure, comments));

      Files.writeFileExn(src, Format.flush_str_formatter());
    };
  });
};


let replaceErrors = (ctx, expr) =>
  expr
  ->mapExpr((mapper, expr) =>
      switch (expr.pexp_desc) {
      | Pexp_construct({txt: Longident.Lident("Error")} as lid, Some(arg))
          when ctx->getExprType(arg)->matchesType("string", []) =>
        Some(
          Ast_helper.Exp.construct(
            lid,
            Some(Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("Unspecified")), Some(arg))),
          ),
        )
      | _ => None
      }
    );

let modify = (ctx, structure) =>
  structure
  ->strExpr((mapper, expr) =>
      expr
      ->mapFnExpr(
          (
            mapper,
            args: list((Asttypes.arg_label, option(Parsetree.expression), Parsetree.pattern)),
            body: Parsetree.expression,
          ) =>
          if (ctx->getExprType(body)->matchesType("Belt.Result.t", [])) {
            Some((args, ctx->replaceErrors(body)));
          } else {
            None;
          }
        )
      ->Some
    );

switch (Sys.argv) {
  | [|_, root|] =>
    print_endline("Running on this " ++ root);
    runCodeMod(
      root,
      (path, moduleName) => Filename.extension(path) == ".re",
      /* (ctx, str) => str */
      modify
    );
  | _ => ()
};

