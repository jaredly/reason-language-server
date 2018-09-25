open Parsetree;

type ctx = {
  full: SharedTypes.full,
  state: Lib.TopTypes.state,
  package: Lib.TopTypes.package,
};

let mapStr = (structure, strMapper) => {
  let mapper = {
    ...Ast_mapper.default_mapper,
    structure_item: (mapper, str) => switch (strMapper(mapper, str)) {
      | None => Ast_mapper.default_mapper.structure_item(mapper, str)
      | Some(str) => str
    },
  };
  mapper.structure(mapper, structure)
};

let mapExpr = (expr, exprMapper) => {
  let mapper = {
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) => switch (exprMapper(mapper, expr)) {
      | None => Ast_mapper.default_mapper.expr(mapper, expr)
      | Some(expr) => expr
    },
  };
  mapper.expr(mapper, expr)
};

let strExpr = (structure, exprMapper) => {
  let mapper = {
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) => switch (exprMapper(mapper, expr)) {
      | None => Ast_mapper.default_mapper.expr(mapper, expr)
      | Some(expr) => expr
    },
  };
  mapper.structure(mapper, structure)
};

let rec collectFnArgs = (expr) => switch (expr.pexp_desc) {
  | Pexp_fun(label, defaultValue, pattern, body) =>
    let (args, body) = collectFnArgs(body);
    ([(label, defaultValue, pattern), ...args], body)
  | _ => ([], expr)
};

let rec makeFn = (args, body) => switch args {
  | [] => body
  | [(label, defaultValue, pattern), ...rest] => Ast_helper.Exp.fun_(label, defaultValue, pattern, makeFn(rest, body))
};

let rec mapFn = (mapper, expr) => switch (expr.pexp_desc) {
  | Pexp_fun(label, defaultValue, pattern, body) =>
    {...expr, pexp_desc: Pexp_fun(
      label,
      switch defaultValue {
        | None => None
        | Some(exp) => Some(mapper.Ast_mapper.expr(mapper, exp))
      },
      mapper.pat(mapper, pattern),
      mapFn(mapper, body)
    )}
  | _ => mapper.expr(mapper, expr)
};

let mapFnExpr = (expr, ~returnType=?, fnMapper) => {
  expr->mapExpr((mapper, expr) => switch (expr.pexp_desc) {
    | Pexp_fun(_) =>
      let (args, body) = collectFnArgs(expr);
      switch (fnMapper(mapper, args, body)) {
        | Some((args, body)) => Some(makeFn(args, body))
        | None => Some(mapFn(mapper, expr))
      }
    | _ => None
  })
};

/* let mapConstructor */

let rec pathParts = path => switch path {
  | Path.Pident({name}) => [name]
  | Pdot(inner, name, _) => pathParts(inner) @ [name]
  | Papply(one, two) => pathParts(one) @ pathParts(two)
};

let matchesType = (typ, stringPath, args) => {
  switch (typ) {
    | None => false
    | Some(typ) =>
      switch (typ.SharedTypes.getConstructorPath()) {
        | Some((path, pargs)) when pathParts(path) == Utils.split_on_char('.', stringPath) => true
        | Some((path, _)) =>
          print_endline("StringPath: " ++ stringPath ++ " vs " ++ Path.name(path));
          false
        | _ => false
      }
  }
};

let getExprType = (ctx, expr) => {
  /* print_endline(expr.pexp_loc.loc_start.pos_fname); */
  let%opt loc = References.locForLocations(~extra=ctx.full.extra, expr.pexp_loc);
  switch loc {
    | Typed(typ, _) => {
      print_endline("Found an expr type yall");
      Some(typ)
    }
    | _ => None
  }
};
