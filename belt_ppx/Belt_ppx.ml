open Migrate_parsetree
open OCaml_403.Ast

open Ast_helper
type exp = Parsetree.expression

let mapper =
  {
    Ast_mapper.default_mapper with
    expr= fun self e ->
      let loc = e.pexp_loc in
      begin match e.pexp_desc with
        | Pexp_apply({pexp_desc=Pexp_ident {txt = Lident "|."; _}; _}, [Nolabel, obj_arg; Nolabel, fn]) ->
          let new_obj_arg = self.expr self obj_arg in
          begin match fn with
            | {pexp_desc = Pexp_apply (fn, args); pexp_loc; _} ->
              let fn = self.expr self fn in
              let args = List.map (fun (lab,exp) -> lab, self.expr self exp ) args in
              { Parsetree.pexp_desc = Pexp_apply(fn, (Nolabel, new_obj_arg) :: args);
                pexp_attributes = [];
                pexp_loc = pexp_loc}
            | {pexp_desc = Pexp_construct (lident, None); pexp_loc; _} ->
              { Parsetree.pexp_desc = Pexp_construct(lident, (Some new_obj_arg));
                pexp_attributes = [];
                pexp_loc = pexp_loc}
            | _ -> Exp.apply ~loc (self.expr self fn) [Nolabel, new_obj_arg]
          end
        | _ -> Ast_mapper.default_mapper.expr self e
      end
  }

let () = Driver.register ~name:"ppx_belt" ~args:[] Versions.ocaml_403 (fun _config _cookies -> mapper)