module W = Warnings
open Ppxlib
open Gospel

module Make (B : Backend.S) = struct
  open Builder
  module T = Translation

  let term_printer (spec : Tast.val_spec) (t : Tterm.term) =
    match t.t_loc with
    | None -> Fmt.str "%a" Tterm.print_term t
    | Some loc -> (
        try
          String.sub spec.sp_text
            (loc.loc_start.pos_cnum - spec.sp_loc.loc_start.pos_cnum)
            (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
        with Invalid_argument _ -> Fmt.str "%a" Tterm.print_term t)

  let of_gospel_args args =
    let to_string x =
      Fmt.str "%a" Gospel.Tast.Ident.pp x.Gospel.Tterm.vs_name
    in
    List.fold_right
      (fun arg (eargs, pargs) ->
        match arg with
        | Tast.Lunit -> ((Nolabel, eunit) :: eargs, (Nolabel, punit) :: pargs)
        | Tast.Lnone x ->
            let s = to_string x in
            ((Nolabel, evar s) :: eargs, (Nolabel, pvar s) :: pargs)
        | Tast.Loptional x ->
            let s = to_string x in
            ((Optional s, evar s) :: eargs, (Nolabel, pvar s) :: pargs)
        | Tast.Lnamed x ->
            let s = to_string x in
            ((Labelled s, evar s) :: eargs, (Labelled s, pvar s) :: pargs)
        | Tast.Lghost _ -> (eargs, pargs))
      args ([], [])

  let value ~driver (val_desc : Tast.val_description) =
    let process (spec : Tast.val_spec) =
      let term_printer = term_printer spec in
      (* Declaration location *)
      let loc = val_desc.vd_loc in
      let setup_expr, register_name = T.mk_setup loc val_desc.vd_name.id_str in
      let register_name = evar register_name in
      (* Arguments *)
      let eargs, pargs = of_gospel_args spec.sp_args in
      (* Returned pattern *)
      let ret_pat, ret_expr = T.returned_pattern spec.sp_ret in
      let pre_checks =
        T.mk_pre_checks ~driver ~register_name ~term_printer spec.sp_pre
      in
      let let_call =
        T.mk_call ~driver ~register_name ~term_printer ret_pat loc
          val_desc.vd_name.id_str spec.sp_xpost eargs
      in
      let post_checks =
        T.mk_post_checks ~driver ~register_name ~term_printer spec.sp_post
      in
      let body =
        efun pargs @@ setup_expr @@ pre_checks @@ let_call @@ post_checks
        @@ ret_expr
      in
      [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]]
    in
    Option.map process val_desc.vd_spec

  let constant ~driver (vd : Tast.val_description) =
    let process spec =
      let term_printer = term_printer spec in
      let loc = vd.vd_loc in
      let setup_expr, register_name = T.mk_setup loc vd.vd_name.id_str in
      let register_name = evar register_name in
      let post_checks =
        T.mk_post_checks ~driver ~register_name ~term_printer spec.sp_post
      in
      let body = setup_expr @@ post_checks @@ evar vd.vd_name.id_str in
      [%stri let [%p pvar vd.vd_name.id_str] = [%e body]]
    in
    Option.map process vd.vd_spec

  let function_ ~driver (func : Tast.function_) =
    let loc = func.fun_loc in
    match func.fun_def with
    | None ->
        W.(register (Unsupported "uninterpreted function or predicate", loc));
        None
    | Some def -> (
        let name = gen_symbol ~prefix:("__" ^ func.fun_ls.ls_name.id_str) () in
        let pargs =
          List.map
            (fun vs ->
              (Nolabel, pvar (Fmt.str "%a" Identifier.Ident.pp vs.Tterm.vs_name)))
            func.fun_params
        in
        (* This is needed for recursive functions; ideally the driver should be
           functional.*)
        Drv.add_translation driver func.fun_ls name;
        let recursive = if func.fun_rec then Recursive else Nonrecursive in
        match T.mk_function_def ~driver def with
        | None ->
            Drv.remove_translation driver func.fun_ls;
            None
        | Some expr ->
            let body = efun pargs expr in
            Some
              (pstr_value recursive
                 [ value_binding ~pat:(pvar name) ~expr:body ]))

  let signature module_name env s =
    let driver = Drv.v env in
    let declarations =
      List.filter_map
        (fun (sig_item : Tast.signature_item) ->
          match sig_item.sig_desc with
          | Sig_val (decl, true) ->
              W.register (W.Unsupported "ghost value", decl.vd_loc);
              None
          | Sig_val (decl, _ghost) when decl.vd_args <> [] -> value ~driver decl
          | Sig_val (decl, _ghost) -> constant ~driver decl
          | Sig_type (_, _, true) ->
              W.register (W.Unsupported "ghost type", sig_item.sig_loc);
              None
          | Sig_type (_rec_flag, ty_decls, _ghost)
            when List.exists (fun td -> td.Tast.td_spec <> None) ty_decls ->
              W.register (W.Unsupported "type specification", sig_item.sig_loc);
              None
          | Sig_function func -> function_ ~driver func
          | Sig_axiom axiom ->
              W.register (W.Unsupported "axiom", axiom.Tast.ax_loc);
              None
          | _ -> None)
        s
    in
    W.report ();
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    B.prelude @ (include_lib :: declarations)
end
