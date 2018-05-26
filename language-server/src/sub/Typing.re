
type tag = Type | Value | Module | Constructor(string) | Attribute(string);

type ident('a) = ('a, tag);

let mapIdent = (fn, (a, tag)) => (fn(a), tag);

let contents = ((a, _)) => a;

let showPath = Path.name;

let showLident = l => String.concat(".", Longident.flatten(l));

let showLoc = loc => {Location.print(Format.str_formatter, loc); Format.flush_str_formatter()};
let showType = loc => {Printtyp.type_expr(Format.str_formatter, loc); Format.flush_str_formatter()};

module type Collector = {
  let add: (~mend: Lexing.position=?, ~depth: int=?, Types.type_expr, Location.t) => unit;
  let ident: (ident(Path.t), Location.t) => unit;
  let declaration: (ident(Ident.t), Location.t) => unit;
};

type openn = {mutable used: list(ident(Longident.t)), path: Path.t, loc: Location.t};
type open_stack = {
  mutable closed: list(openn),
  mutable opens: list(openn),
  parent: option(open_stack)
};

/*
let rec convertLidentToPath = lident => switch lident {
| Lident(text) =>
}; */

let rec pathToLident = Longident.(Path.(path => switch path {
| Pident({Ident.name}) => Lident(name)
| Pdot(a, b, _) => Ldot(pathToLident(a), b)
| _ => assert(false)
}));

let rec addLidentToPath = (path, lident) => {
  open Path;
  open Longident;

  switch lident {
  | Lident(text) => Pdot(path, text, 0)
  | Ldot(lident, text) => Pdot(addLidentToPath(path, lident), text, 0)
  | Lapply(_, _) => failwith("I dont know what these are")
  }
};

let rec usesOpen = (ident, path) => switch (ident, path) {
| (Longident.Lident(name), Path.Pdot(path, pname, _)) => true
| (Longident.Lident(_), Path.Pident(_)) => false
| (Longident.Ldot(ident, _), Path.Pdot(path, _, _)) => usesOpen(ident, path)
| _ => failwith("Cannot relative "  ++ Path.name(path) ++ " " ++ String.concat(".", Longident.flatten(ident)))
};

let rec relative = (ident, path) => switch (ident, path) {
| (Longident.Lident(name), Path.Pdot(path, pname, _)) when pname == name => path
| (Longident.Ldot(ident, _), Path.Pdot(path, _, _)) => relative(ident, path)
| _ => failwith("Cannot relative "  ++ Path.name(path) ++ " " ++ String.concat(".", Longident.flatten(ident)))
};


module F = (Collector: Collector) => {
  include TypedtreeIter.DefaultIteratorArgument;
  let depth = ref(0);

  let root_stack = {opens: [{
    used: [],
    loc: {
      let pos = {Lexing.pos_fname: "", pos_lnum: 1, pos_cnum: 0, pos_bol: 0};
      {
        Location.loc_ghost: false,
        loc_start: pos,
        loc_end: pos
      }
    },
    path: Path.Pident({
      Ident.name: "Pervasives",
      stamp: 0,
      flags: 1,
    })
  }], parent: None, closed: []};
  let closed_stacks = ref([]);
  let open_stack = ref(root_stack);
  let new_stack = () => open_stack := {parent: Some(open_stack^), opens: [], closed: []};
  let pop_stack = () => switch (open_stack^.parent) {
  | Some(parent) => {
    closed_stacks := [open_stack^, ...closed_stacks^];
    open_stack := parent
  }
  | None => ()
  };

  let add_open = (path, loc) => {
    open_stack^.opens = [{path, loc, used: []}, ...open_stack^.opens];
  };
  let pop_open = () => {
    switch (open_stack^.opens) {
    | [] => ()
    | [top, ...rest] => {
      open_stack^.opens = rest;
      open_stack^.closed = [top, ...open_stack^.closed];
    }
    }
  };

  let add_use = (~inferable=false, (path, tag), ident, loc) => {
    let openNeedle = relative(ident, path);
    let rec loop = stack => {
      let rec inner = opens => switch opens {
      | [] => switch stack.parent {
      | Some(parent) => loop(parent)
      | None => if (!inferable) {
        print_endline("Unable to find an open to meet my needs: " ++ showLident(ident) ++ ": " ++ showLoc(loc)  )
      }
      }
      | [{path} as one, ...rest] when Path.same(path, openNeedle) =>  {
        /* print_endline("Matched " ++ Path.name(path) ++ " "); */
        one.used = [(ident, tag), ...one.used];
      }
      | [{path}, ...rest] => {
        inner(rest)
      }
      };
      inner(stack.opens)
    };
    loop(open_stack^)
  };

  let rec dig = typ => switch typ.Types.desc {
  | Types.Tlink(inner) => dig(inner)
  | Types.Tsubst(inner) => dig(inner)
  | _ => typ
  };

  let enter_core_type = (typ) => {
    open Typedtree;
    Collector.add(~depth=depth^, typ.ctyp_type, typ.ctyp_loc);
    switch typ.ctyp_desc {
    | Ttyp_constr(path, {txt, loc}, args) => {
      if (usesOpen(txt, path)) {
        add_use((path, Type), txt, loc);
      };
      Collector.ident((path, Type), loc)
    }
    | _ => ()
    }
  };
  let enter_type_declaration = (typ) => {
    open Typedtree;
    switch typ.typ_type.Types.type_manifest {
    | Some((x)) => Collector.add(~depth=depth^, x, typ.typ_loc)
    | _ => ()
    };
    Collector.declaration((typ.typ_id, Type), typ.typ_name.loc)
  };

  let handleConstructor = (path, txt) => {
    let typeName = switch path {
    | Path.Pdot(path, typename, _) => typename
    | Pident({Ident.name}) => name
    | _ => assert(false)
    };

    Longident.(switch txt {
    | Longident.Lident(name) => (name, Lident(typeName))
    | Ldot(left, name) => (name, Ldot(left, typeName))
    | Lapply(_) => assert(false)
    });
  };

  let handleRecord = (path, txt) => {
    let typeName = switch path {
    | Path.Pdot(path, typename, _) => typename
    | Pident({Ident.name}) => name
    | _ => assert(false)
    };

    Longident.(switch txt {
    | Lident(name) => Lident(typeName)
    | Ldot(inner, name) => Ldot(inner, typeName)
    | Lapply(_) => assert(false)
    });
  };


  let enter_pattern = pat => {
    open Typedtree;
    Collector.add(~depth=depth^, pat.pat_type, pat.pat_loc);
    switch (pat.pat_desc) {
    | Tpat_var(ident, {txt, loc}) => Collector.declaration((ident, Value), loc)
    | Tpat_alias(_, ident, {txt, loc}) => Collector.declaration((ident, Value), loc)
    | Tpat_construct({txt, loc}, desc, args) => {
      switch (dig(pat.pat_type).Types.desc) {
      | Tconstr(path, args, _) => {
        let (constructorName, typeTxt) = handleConstructor(path, txt);
        Collector.ident((path, Constructor(constructorName)), loc);

        if (usesOpen(typeTxt, path)) {
          add_use((path, Constructor(constructorName)), typeTxt, loc)
        };
      }
      | _ => print_endline("a constructor wasn't typed as a constructor?" ++ {
        Printtyp.type_expr(Format.str_formatter, pat.pat_type);
        Format.flush_str_formatter()
      })
    }
    }
    | Tpat_record(items, isClosed) => items |> List.iter((({Asttypes.txt, loc}, label, value)) => {

        switch (dig(label.Types.lbl_res).Types.desc) {
        | Tconstr(path, args, _) => {
          Collector.ident((path, Attribute(label.Types.lbl_name)), loc);
          let typeTxt = handleRecord(path, txt);
          if (usesOpen(typeTxt, path)) {
            add_use(~inferable=true, (path, Attribute(label.Types.lbl_name)), typeTxt, loc)
          };
        }
        | _ => print_endline("Record not a constr " ++ {
          Printtyp.type_expr(Format.str_formatter, pat.pat_type);
          Format.flush_str_formatter()
        })
        };

    })
    | _ => ()
    }
  };

  let enter_structure_item = str => {
    open Typedtree;
    switch str.str_desc {
    | Tstr_value(isrec, bindings) =>
      List.iter(
        (binding) => Collector.add(binding.vb_expr.exp_type, binding.vb_loc),
        bindings
      )
    | Tstr_module({mb_id, mb_name: {txt, loc}}) => {
      Collector.declaration((mb_id, Module), loc);
      new_stack();
    }
    | Tstr_type(decls) => List.iter(
      decl => {
        /* TODO figure this out */
        switch (decl.typ_type.type_manifest) {
        | None => ()
        | Some(exp) => Collector.add(exp, str.str_loc)
        };
        switch (decl.typ_kind) {
        | Ttype_variant(constructors) => {
          constructors |> List.iter(({cd_id, cd_name: {txt, loc}}) => (
            Collector.declaration((decl.typ_id, Constructor(txt)), loc)
          ))
        }
        | Ttype_record(labels) => labels |> List.iter(({ld_id, ld_name: {txt, loc}}) => (
            Collector.declaration((decl.typ_id, Attribute(txt)), loc)
          ))
        | _ => ()
        }
      },
      decls
    )
    | Tstr_open({open_path, open_txt: {txt, loc}}) => {
      if (usesOpen(txt, open_path)) {
        add_use((open_path, Module), txt, loc);
      };
      Collector.ident((open_path, Module), loc);
      add_open(open_path, loc);
    }
    | _ => ()
    }
  };
  let leave_structure_item = str => Typedtree.(switch str.str_desc {
  | Tstr_module(_) => pop_stack()
  | _ => ()
  });

  let enter_expression = (expr) => {
    open Typedtree;
    expr.exp_extra |> List.iter(((ex, loc, _)) => switch ex {
    | Texp_open(_, path, {txt, loc}, _) => add_open(path, loc)
    | _ => ()
    });
    Collector.add(~depth=depth^, expr.exp_type, expr.Typedtree.exp_loc);
    switch expr.exp_desc {
    | Texp_for(ident, _, _, _, _, _) => {
      /* TODO I'm not super happy about this because the loc is not around the actual declaration :/ */
      /* But wait!! On the parsetree side, we do have a good loc. Soo we'll have to do extra work, but we can get it done. */
      Collector.declaration((ident, Value), expr.Typedtree.exp_loc)
    }

    | Texp_let(isrec, bindings, rest) =>
      List.iter(
        (binding) => Collector.add(~depth=depth^, binding.vb_expr.exp_type, binding.vb_loc),
        bindings
      )
    | Texp_ident(path, {txt, loc}, value_description) => {
      if (usesOpen(txt, path)) {
        add_use((path, Value), txt, loc);
      };
      Collector.ident((path, Value), loc)
    }
    | Texp_field(inner, {txt, loc}, label) => {
      Collector.add(~depth=depth^, label.Types.lbl_res, loc);
      switch (dig(label.Types.lbl_res).Types.desc) {
      | Tconstr(path, args, _) => {
          Collector.ident((path, Attribute(label.Types.lbl_name)), loc);
          let typeTxt = handleRecord(path, txt);
          if (usesOpen(typeTxt, path)) {
            add_use(~inferable=true, (path, Attribute(label.Types.lbl_name)), typeTxt, loc)
          };
      }
      | _ => print_endline("Field access wasn't constructor")
      }
    }
    | Texp_record(items, ext) =>
      List.iter((({Asttypes.txt, loc}, label, ex)) => {
        Collector.add(
          ~depth=depth^,
          expr.exp_type,
          ~mend=expr.exp_loc.Location.loc_end,
          loc
        );

        switch (dig(label.Types.lbl_res).Types.desc) {
        | Tconstr(path, args, _) => {
          Collector.ident((path, Attribute(label.Types.lbl_name)), loc);
          let typeTxt = handleRecord(path, txt);
          if (usesOpen(typeTxt, path)) {
            add_use((path, Attribute(label.Types.lbl_name)), typeTxt, loc)
          };
        }
        | _ => print_endline("Record not a constr " ++ {
          Printtyp.type_expr(Format.str_formatter, expr.exp_type);
          Format.flush_str_formatter()
        })
        };
      }, items)

    | Texp_construct({txt, loc}, {Types.cstr_name, cstr_loc}, args) => {
      switch (dig(expr.exp_type).Types.desc) {
      | Tconstr(path, args, _) => {
        let (constructorName, typeTxt) = handleConstructor(path, txt);
        Collector.ident((path, Constructor(constructorName)), loc);
        if (usesOpen(typeTxt, path)) {
          add_use((path, Constructor(constructorName)), typeTxt, loc)
        };
      }
      | _ => print_endline("a constructor wasn't typed as a constructor?" ++ {
        Printtyp.type_expr(Format.str_formatter, expr.exp_type);
        Format.flush_str_formatter()
      })
      };
    }

    | _ => ()
    };
    depth :=depth^ + 1;
  };
  let leave_expression = (expr) => {
    depth :=depth^ - 1;
    open Typedtree;
    expr.exp_extra |> List.iter(((ex, loc, _)) => switch ex {
    | Texp_open(_, path, {txt, loc}, _) => pop_open()
    | _ => ()
    });
  };
};



let ppos = (pos) =>
  Lexing.(
    Printf.sprintf(
      {|{"line": %d, "col": %d, "chars": %d}|},
      pos.pos_lnum,
      pos.pos_cnum - pos.pos_bol,
      pos.pos_cnum
    )
  );

let entry = (loc, ~depth, ~mend=?, typ) => {
  open Location;
  let mend =
    switch mend {
    | Some((x)) => x
    | None => loc.loc_end
    };
  Printf.sprintf(
    {|{"depth": %d, "start": %s, "end": %s, "type": %S}|},
    depth,
    ppos(loc.loc_start),
    ppos(mend),
    typ
  );
};


let type_to_string = (typ) => {
  Printtyp.type_expr(Format.str_formatter, typ);
  Format.flush_str_formatter();
};

type externalsUsed = list((Path.t, Location.t));

type bindings = Hashtbl.t(int, list((Ident.t, Location.t)));

let truncateLoc = (length, {Location.loc_start, loc_end} as loc) => {
  ...loc,
  loc_start,
  loc_end: {
    ...loc_start,
    pos_cnum: loc_start.pos_cnum + length
  }
};

let collectTypes = annots => {
  let types = Hashtbl.create(100);
  let add = (~mend=?, ~depth=0, typ, loc) => if (!loc.Location.loc_ghost) {
    let loc_end = switch mend {
    | Some(m) => m
    | None => loc.Location.loc_end
    };
    Hashtbl.add(types, (loc.Location.loc_start, loc_end), typ)
  };
  let externals = ref([]);
  let bindings = Hashtbl.create(100);
  let locToPath = Hashtbl.create(100);

  let ident = (path, loc) => {
    if (!loc.Location.loc_ghost) {
      Hashtbl.replace(locToPath, (loc.Location.loc_start.pos_cnum, loc.Location.loc_end.pos_cnum), path);
    }
  };

  let declaration = (decl, loc) => {
    ident(mapIdent(id => Path.Pident(id), decl), loc)
  };

  let module Config = {
    let add = add;
    let ident = ident;
    let declaration = declaration;
  };
  let module IterSource = F(Config);
  let module Iter = TypedtreeIter.MakeIterator(IterSource);

  switch annots {
  | Cmt_format.Implementation(structure) => {
    Printtyped.implementation(Format.str_formatter, structure);
    Files.writeFile("./log_types.txt", Format.flush_str_formatter()) |> ignore;
    Iter.iter_structure(structure)
  }
  | _ => failwith("Not a valid cmt file")
  };

  let all_opens = IterSource.root_stack.opens @ IterSource.root_stack.closed @ List.concat(
    List.map(op => op.opens @ op.closed, IterSource.closed_stacks^)
  );

  (types, bindings, externals^, all_opens, locToPath)
};
