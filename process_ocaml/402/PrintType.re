
open Compiler_libs_402;
open Outcometree;

let rec dig = (typ) =>
  switch typ.Types.desc {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | Types.Tpoly(inner, _) => dig(inner)
  | _ => typ
  };

let rec collectArgs = (coll, typ) => switch typ.Types.desc {
| Types.Tarrow(label, arg, result, _) => collectArgs([(label, arg), ...coll], result)
| Tlink(inner) => collectArgs(coll, inner)
| Tsubst(inner) => collectArgs(coll, inner)
| _ => (coll, typ)
};

type pathType = PModule | PModuleType | PValue | PType;

module T = {
  type stringifier = {
    path: (stringifier, Path.t, pathType) => Pretty.doc,
    expr: (~depth:int=?, stringifier, Types.type_expr) => Pretty.doc,
    ident: (stringifier, Ident.t) => Pretty.doc,
    decl: (stringifier, string, string, Types.type_declaration) => Pretty.doc,
    value: (stringifier, string, string, Types.type_expr) => Pretty.doc,
    constructor: (stringifier, Types.constructor_declaration) => Pretty.doc,
  };
};
open T;

let break = Pretty.line("");
let space = Pretty.line(" ");
let dedent = Pretty.back(2, "");

let str = Pretty.text;
let (@!) = Pretty.append;

let sepd_list = (sep, items, loop) => {
  let rec recur = items => switch items {
    | [] => Pretty.empty
    | [one] => loop(one)
    | [one, ...more] =>
      let l = loop(one);
      l @! sep @! recur(more)
  };
  recur(items)
};

let commad_list = (loop, items) => {
  sepd_list(str(",") @! space, items, loop)
};

let indentGroup = doc => Pretty.indent(2, Pretty.group(doc));

let tuple_list = (items, loop) => {
  str("(") @! indentGroup(break @!
  commad_list(loop, items) @!
  dedent) @!
  str(")")
};

let replace = (one, two, text) => Str.global_replace(Str.regexp_string(one), two, text);
let htmlEscape = text => replace("<", "&lt;", text) |> replace(">", "&gt;");

let showArgs = (loop, args) => {
    str("(") @!
    indentGroup(
      break @!
    commad_list(((label, typ)) => {
      if (label == "") {
        loop(typ)
      } else {
        str("~" ++ label ++ ": ") @! loop(typ)
      }
    }, args)
    @! dedent
    ) @! str(")")
};

type namer = {reset: unit => unit, get: Types.type_expr => string};
let makeNamer = () => {
  let alphabet = "abcdefghijklmnopqrstuvwxyz";
  let latest = ref(0);
  let names = Hashtbl.create(10);
  let newName = () => {
    let i = latest^;
    latest := i + 1;
    let num = i > 25 ? "t" ++ string_of_int(i) : String.sub(alphabet, i, 1);
    "'" ++ num;
  };
  let get = t => 
    try (Hashtbl.find(names, t)) {
      | Not_found => {
        let name = newName();
        Hashtbl.replace(names, t, name);
        name;
      }
    };
  let reset = () => {
    latest := 0;
    Hashtbl.clear(names);
  };
  {get, reset};
};

let namer = makeNamer();

let print_expr = (~depth=0, stringifier, typ) => {
  /* Log.log("print_expr"); */
  let loop = stringifier.expr(~depth=depth + 1, stringifier);
  if (depth > 20) {
    str("Too deep")
  } else {

  open Types;
  switch (typ.desc) {
  | Tvar(None) => str(namer.get(typ))
  | Tvar(Some(s)) => str("'" ++ s)
  | Tarrow(label, arg, result, _) => {
    let (args, result) = collectArgs([(label, arg)], result);
    let args = List.rev(args);
    switch args {
    | [("", typ)] => {
      switch (dig(typ)) {
        | {desc: Ttuple(_)} => showArgs(loop, args)
        | _ => loop(typ)
      }
    }
    | _ => showArgs(loop, args)
    }
     @! str(" => ") @!
    loop(result);
  }
  | Ttuple(items) => tuple_list(items, loop)
  | Tconstr(path, args, _) => {
    stringifier.path(stringifier, path, PType) @!
    switch args {
    | [] => Pretty.empty
    | args => tuple_list(args, loop)
    }
  }
  | Tlink(inner) => loop(inner)
  | Tsubst(inner) => loop(inner)
  | Tnil => str("(no type)")
  | Tfield(_, _, _, _)
  | Tvariant(_)
  | Tunivar(_)
  | Tpoly(_, _)
  | Tpackage(_, _, _)
  | Tobject(_, _) => {
    let txt = {
      try {
        Printtyp.type_expr(Format.str_formatter, typ);
      } { 
        | _ => Format.fprintf(Format.str_formatter, "Unable to print type")
      };
      Format.flush_str_formatter()
    };
    str(txt)
  }
  }
  }
};

let print_constructor = (loop, {Types.cd_id: {name}, cd_args, cd_res}) => {
  str(name) @!
  (switch cd_args {
  | [] => Pretty.empty
  | args =>
    tuple_list(args, loop)
  }) @!
  (switch cd_res {
  | None => Pretty.empty
  | Some(typ) => {
    str(": ") @!
    loop(typ)
  }
  })
};

let print_attr = (printer, {Types.ld_id, ld_mutable, ld_type}) => {
  switch ld_mutable {
  | Asttypes.Immutable => Pretty.empty
  | Mutable => str("mut ")
  } @!
  printer.ident(printer, ld_id) @!
  str( ": ") @!
  printer.expr(printer, ld_type);
};

let print_value = (stringifier, realName, name, decl) => {
  str("let ") @!
  str(~len=String.length(realName), name) @!
  str(": ") @! stringifier.expr(stringifier, decl)
};

let print_decl = (stringifier, realName, name, decl) => {
  open Types;
  str("type ") @!
  str(~len=String.length(realName), name) @!
  switch decl.type_params {
  | [] => Pretty.empty
  | args => tuple_list(args, stringifier.expr(stringifier))
  } @!
  switch decl.type_kind {
  | Type_abstract => Pretty.empty
  | Type_open => str(" = ..")
  | Type_record(labels, representation) => {
    str(" = {") @! indentGroup(break @!
    commad_list(print_attr(stringifier), labels)
     @! dedent) @! str("}")
  }
  | Type_variant(constructors) => {
    str(" = ") @! indentGroup(break @! str("| ") @!
      sepd_list(space @! str("| "), constructors, print_constructor(stringifier.expr(stringifier)))
    ) @! break
  }
  } @!
  switch decl.type_manifest {
  | None => Pretty.empty
  | Some(manifest) => {
    str(" = ") @!
    stringifier.expr(stringifier, manifest)
  }
  };
};

let default = {
  ident: (_, {Ident.name}) => str(name),
  path: (stringifier, path, pathType) => switch path {
    | Path.Pident(ident) => stringifier.ident(stringifier, ident)
    | Pdot(path, name, _) => {stringifier.path(stringifier, path, pathType) @! str("." ++ name)}
    | Papply(_, _) => str("<apply>")
  },
  value: print_value,
  expr: print_expr,
  decl: print_decl,
  constructor: (s, decl) => print_constructor(s.expr(s), decl),
};

let prettyString = (~width=60, doc) => {
  namer.reset();
  let buffer = Buffer.create(100);
  Pretty.print(~width, ~output=(text => Buffer.add_string(buffer, text)), ~indent=(num => {
    Buffer.add_string(buffer, "\n");
    for (i in 1 to num) { Buffer.add_string(buffer, " ") }
  }), doc);
  Buffer.to_bytes(buffer) |> Bytes.to_string
};