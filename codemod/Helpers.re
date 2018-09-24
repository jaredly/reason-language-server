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

let mapFnExpr = (expr, fnMapper) => {
  expr->mapExpr((mapper, expr) => switch (expr.pexp_desc) {
    | Pexp_fun(_) =>
      let (args, body) = collectFnArgs(expr);
      switch (fnMapper(mapper, args, body)) {
        | Some((args, body)) => Some(makeFn(args, body))
        | None => None
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
        | _ => false
      }
  }
};

let getExprType = (ctx, expr) => {
  let%opt loc = References.locForLocations(~extra=ctx.full.extra, expr.pexp_loc);
  switch loc {
    | Typed(typ, _) => Some(typ)
    | _ => None
  }
};
