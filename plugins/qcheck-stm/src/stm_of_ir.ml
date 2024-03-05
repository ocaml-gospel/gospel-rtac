module Cfg = Config
open Ir
open Ppxlib
open Ortac_core.Builder
module Ident = Gospel.Identifier.Ident

let ty_default = Ptyp_constr (noloc (Lident "char"), [])
let pat_default = ppat_construct (lident "Char") None
let exp_default = evar "char"
let res_default = Ident.create ~loc:Location.none "res"

let may_raise_exception v =
  match (v.postcond.exceptional, v.postcond.checks) with
  | [], [] -> false
  | _, _ -> true

let qualify ms v =
  let lid =
    match ms with
    | [] -> Lident v
    | m :: ms ->
        let pref = List.fold_left (fun acc x -> Ldot (acc, x)) (Lident m) ms in
        Ldot (pref, v)
  in
  Ast_helper.Exp.ident (noloc lid)

let subst_core_type inst ty =
  let rec aux ty =
    {
      ty with
      ptyp_desc =
        (match ty.ptyp_desc with
        | Ptyp_any -> ty_default
        | Ptyp_var x ->
            Option.fold ~none:ty_default
              ~some:(fun x -> x.ptyp_desc)
              (List.assoc_opt x inst)
        | Ptyp_arrow (x, l, r) ->
            let l = aux l and r = aux r in
            Ptyp_arrow (x, l, r)
        | Ptyp_tuple elems ->
            let elems = List.map aux elems in
            Ptyp_tuple elems
        | Ptyp_constr (c, args) ->
            let args = List.map aux args in
            Ptyp_constr (c, args)
        | Ptyp_object (_, _)
        | Ptyp_class (_, _)
        | Ptyp_alias (_, _)
        | Ptyp_variant (_, _, _)
        | Ptyp_poly (_, _)
        | Ptyp_package _ | Ptyp_extension _ ->
            failwith "Case should not happen in `subst_core_type'");
    }
  in
  aux ty

let lazy_force =
  let open Gospel in
  let open Tterm_helper in
  let vs_name = Ident.create ~loc:Location.none "Lazy.force"
  and vs_ty = Ttypes.fresh_ty_var "a" in
  let lazy_force = mk_term (Tvar { vs_name; vs_ty }) None Location.none in
  fun t ->
    Tterm_helper.(
      mk_term (Tapp (Symbols.fs_apply, [ lazy_force; t ])) None Location.none)

let ocaml_of_term cfg t =
  let open Ortac_core.Ocaml_of_gospel in
  let open Reserr in
  try term_with_catch ~context:cfg.Cfg.context t |> ok
  with W.Error e -> error e

(** [subst_term state ~gos_t ?old_lz ~old_t ?new_lz ~new_t trm] will substitute
    occurrences of [gos_t] with [new_t] or [old_t] depending on whether the
    occurrence appears above or under the [old] operator, adding a [Lazy.force]
    if the corresponding [xxx_lz] is [true] (defaults to [false]). [gos_t] must
    always be in a position in which it is applied to one of its model fields.
    Calling [subst_term] with [new_t] and [old_t] as None will check that the
    term does not contain [gos_t] *)
let subst_term state ?(out_of_scope = []) ~gos_t ?(old_lz = false) ~old_t
    ?(new_lz = false) ~new_t term =
  let exception
    ImpossibleSubst of
      (Gospel.Tterm.term * [ `Never | `New | `Old | `NotModel | `OutOfScope ])
  in
  let rec aux cur_lz cur_t term =
    let open Gospel.Tterm in
    let next = aux cur_lz cur_t in
    match term.t_node with
    (* First: the only case where substitution happens, ie x.model *)
    | Tfield (({ t_node = Tvar { vs_name; vs_ty }; _ } as subt), ls)
      when Ident.equal vs_name gos_t ->
        if List.exists (fun (m, _) -> Ident.equal m ls.ls_name) state then
          match cur_t with
          | Some cur_t ->
              let t = { subt with t_node = Tvar { vs_name = cur_t; vs_ty } } in
              let t = if cur_lz then lazy_force t else t in
              { term with t_node = Tfield (t, ls) }
          | None ->
              raise
                (ImpossibleSubst
                   ( subt,
                     match (new_t, old_t) with
                     | None, None -> `Never
                     | None, _ -> `New
                     | _, _ -> `Old ))
        else
          (* case x.f where f is _not_ a model field *)
          raise (ImpossibleSubst (term, `NotModel))
    (* If the first case didn't match, it must be because [gos_t] is not used to
       access one of its model fields, so we error out *)
    | Tvar { vs_name; _ } when Ident.equal vs_name gos_t ->
        raise (ImpossibleSubst (term, `NotModel))
    (* Then, we check if the variable is not out_of_scope in the function we are building *)
    | Tvar { vs_name; _ } when List.exists (Ident.equal vs_name) out_of_scope ->
        raise (ImpossibleSubst (term, `OutOfScope))
    | Tconst _ -> term
    | Tvar _ -> term
    | Tapp (ls, terms) -> { term with t_node = Tapp (ls, List.map next terms) }
    | Tfield (t, ls) -> { term with t_node = Tfield (next t, ls) }
    | Tif (cnd, thn, els) ->
        { term with t_node = Tif (next cnd, next thn, next els) }
    | Tlet (vs, t1, t2) -> { term with t_node = Tlet (vs, next t1, next t2) }
    | Tcase (t, brchs) ->
        {
          term with
          t_node =
            Tcase
              ( next t,
                List.map
                  (fun (p, ot, t) -> (p, Option.map next ot, next t))
                  brchs );
        }
    | Tquant (q, vs, t) -> { term with t_node = Tquant (q, vs, next t) }
    | Tlambda (ps, t) -> { term with t_node = Tlambda (ps, next t) }
    | Tbinop (o, l, r) -> { term with t_node = Tbinop (o, next l, next r) }
    | Tnot t -> { term with t_node = Tnot (next t) }
    | Told t -> aux old_lz old_t t
    | Ttrue -> term
    | Tfalse -> term
  in
  let open Reserr in
  try ok (aux new_lz new_t term)
  with ImpossibleSubst (t, b) ->
    error (Impossible_term_substitution b, t.t_loc)

let translate_checks config state value state_ident t =
  let open Reserr in
  subst_term state ~gos_t:value.sut_var ~old_t:(Some state_ident)
    ~new_t:(Some state_ident) t
  >>= ocaml_of_term config

let str_of_ident = Fmt.str "%a" Ident.pp
let longident_loc_of_ident id = str_of_ident id |> lident

let mk_cmd_pattern value =
  let pat_args = function
    | _, None -> punit
    | _, Some x -> ppat_var (noloc (str_of_ident x))
  in
  let args =
    match value.args with
    | [] -> None
    | [ x ] -> Some (pat_args x)
    | xs -> List.map pat_args xs |> ppat_tuple |> Option.some
  in
  let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
  ppat_construct name args

let munge_longident cap ty lid =
  let open Reserr in
  match lid.txt with
  | Lident i | Ldot (Lident i, "t") | Ldot (Ldot (_, i), "t") | Ldot (_, i) ->
      let f =
        if cap then String.capitalize_ascii else String.uncapitalize_ascii
      in
      ok (f i)
  | Lapply (_, _) ->
      error
        (Type_not_supported (Fmt.str "%a" Pprintast.core_type ty), ty.ptyp_loc)

let pat_of_core_type inst typ =
  let rec aux ty =
    let open Reserr in
    match ty.ptyp_desc with
    | Ptyp_any -> ok pat_default
    | Ptyp_var v -> (
        match List.assoc_opt v inst with
        | None -> ok pat_default
        | Some t -> aux t)
    | Ptyp_constr (c, xs) ->
        let constr_str = lident <$> munge_longident true ty c
        and pat_arg =
          match xs with
          | [] -> ok None
          | xs -> (fun xs -> Some (ppat_tuple xs)) <$> map aux xs
        in
        ppat_construct <$> constr_str <*> pat_arg
    | _ ->
        error
          ( Type_not_supported (Fmt.str "%a" Pprintast.core_type typ),
            typ.ptyp_loc )
  in
  aux typ

let exp_of_core_type inst typ =
  let rec aux ty =
    let open Reserr in
    match ty.ptyp_desc with
    | Ptyp_any -> ok exp_default
    | Ptyp_var v -> (
        match List.assoc_opt v inst with
        | None -> ok exp_default
        | Some t -> aux t)
    | Ptyp_constr (c, xs) -> (
        let constr_str = evar <$> munge_longident false ty c in
        match xs with
        | [] -> constr_str
        | xs ->
            pexp_apply
            <$> constr_str
            <*> (List.map (fun e -> (Nolabel, e)) <$> map aux xs))
    | _ ->
        error
          ( Type_not_supported (Fmt.str "%a" Pprintast.core_type typ),
            typ.ptyp_loc )
  in
  aux typ

let exp_of_ident id = pexp_ident (lident (str_of_ident id))

let arb_cmd_case value =
  let open Reserr in
  let epure = pexp_ident (lident "pure") in
  let pure e = pexp_apply epure [ (Nolabel, e) ] in
  let fun_cstr =
    let args =
      List.map
        (function
          | _, None -> (Nolabel, punit)
          | _, Some id -> (Nolabel, ppat_var (noloc (str_of_ident id))))
        value.args
    in
    let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
    let body =
      pexp_construct name
        (pexp_tuple_opt
           (List.map
              (function
                | _, None -> eunit | _, Some id -> evar (str_of_ident id))
              value.args))
    in
    efun args body |> pure
  in
  let gen_args =
    (* XXX TODO: use `requires` clauses to build smarter generators *)
    List.map (fun (ty, _) -> exp_of_core_type value.inst ty) value.args
  in
  let app l r = pexp_apply (evar "( <*> )") [ (Nolabel, l); (Nolabel, r) ] in
  List.fold_left app fun_cstr <$> sequence gen_args

let arb_cmd ir =
  let open Reserr in
  let* cmds = elist <$> map arb_cmd_case ir.values in
  let open Ppxlib in
  let let_open str e =
    pexp_open Ast_helper.(Opn.mk (Mod.ident (lident str |> noloc))) e
  in
  let oneof = let_open "Gen" (pexp_apply (evar "oneof") [ (Nolabel, cmds) ]) in
  let body =
    let_open "QCheck"
      (pexp_apply (evar "make")
         [ (Labelled "print", evar "show_cmd"); (Nolabel, oneof) ])
  in
  let pat = pvar "arb_cmd" in
  let expr = efun [ (Nolabel, ppat_any (* for now we don't use it *)) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let run_case config sut_name value =
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* rhs =
    let res = lident "Res" in
    let* ty_show = exp_of_core_type value.inst (Ir.get_return_type value) in
    let ty_show =
      if may_raise_exception value then
        pexp_apply (evar "result") [ (Nolabel, ty_show); (Nolabel, evar "exn") ]
      else ty_show
    in
    let call =
      let efun = exp_of_ident value.id in
      let mk_arg = Option.fold ~none:eunit ~some:exp_of_ident in
      let rec aux ty args =
        match (ty.ptyp_desc, args) with
        | Ptyp_arrow (lb, l, r), xs when Cfg.is_sut config l ->
            (lb, evar sut_name) :: aux r xs
        | Ptyp_arrow (lb, _, r), x :: xs -> (lb, mk_arg x) :: aux r xs
        | _, [] -> []
        | _, _ ->
            failwith
              "shouldn't happen (list of arguments should be consistent with \
               type)"
      in
      pexp_apply efun (aux value.ty (List.map snd value.args))
    in
    let call =
      if may_raise_exception value then
        let lazy_call = efun [ (Nolabel, punit) ] call in
        pexp_apply (evar "protect") [ (Nolabel, lazy_call); (Nolabel, eunit) ]
      else call
    in
    let args = Some (pexp_tuple [ ty_show; call ]) in
    pexp_construct res args |> ok
  in
  case ~lhs ~guard:None ~rhs |> ok

let run config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let sut_name = gen_symbol ~prefix:"sut" () in
  let open Reserr in
  let* cases = map (run_case config sut_name) ir.values in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "run" in
  let expr = efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar sut_name) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let next_state_case state config state_ident nb_models value =
  let state_var = str_of_ident state_ident |> evar in
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* idx, rhs =
    (* substitute state variable when under `old` operator and translate description into ocaml *)
    let descriptions =
      List.filter_map
        (fun (i, { model; description }) ->
          subst_term ~out_of_scope:value.ret state ~gos_t:value.sut_var
            ~old_t:(Some state_ident) ~new_t:None description
          >>= ocaml_of_term config
          |> to_option
          |> Option.map (fun description -> (i, model, description)))
        value.next_state.formulae
    in
    (* choose one and only one description per modified model *)
    let pick id =
      List.find_opt (fun (_, m, _) -> Ident.equal id m) descriptions
    in
    let* descriptions =
      map
        (fun (id, loc) ->
          of_option
            ~default:
              ( Ensures_not_found_for_next_state
                  (value.id.id_str, id.Ident.id_str),
                loc )
            (pick id))
        value.next_state.modifies
    in
    (* [idx], like [descriptions], is in the order of the modifies clauses *)
    let idx = List.map (fun (i, _, _) -> i) descriptions in
    match
      List.map (fun (_, m, e) -> (lident (str_of_ident m), e)) descriptions
    with
    | [] -> ok (idx, state_var)
    | fields -> (
        let new_state =
          pexp_record fields
            (if List.length fields = nb_models then None
             else Some (evar (str_of_ident state_ident)))
        in
        let translate_checks =
          translate_checks config state value state_ident
        in
        let* checks = map translate_checks value.postcond.checks in
        match checks with
        | [] -> ok (idx, new_state)
        | _ ->
            ok
              (idx, pexp_ifthenelse (list_and checks) new_state (Some state_var))
        )
  in
  (idx, case ~lhs ~guard:None ~rhs) |> ok

let next_state config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let state_ident = Ident.create ~loc:Location.none state_name in
  let nb_models = List.length ir.state in
  let open Reserr in
  let* idx_cases =
    map
      (fun v ->
        let* i, c = next_state_case ir.state config state_ident nb_models v in
        ok ((v.id, i), c))
      ir.values
  in
  let idx, cases = List.split idx_cases in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "next_state" in
  let expr =
    efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar state_name) ] body
  in
  (idx, pstr_value Nonrecursive [ value_binding ~pat ~expr ]) |> ok

let precond_case config state state_ident value =
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* rhs =
    list_and
    <$> map
          (fun t ->
            subst_term state ~gos_t:value.sut_var ~old_t:None
              ~new_t:(Some state_ident) t
            >>= ocaml_of_term config)
          value.precond
  in
  ok (case ~lhs ~guard:None ~rhs)

let precond config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let state_ident = Ident.create ~loc:Location.none state_name in
  let open Reserr in
  let* cases = map (precond_case config ir.state state_ident) ir.values in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "precond" in
  let expr =
    efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar state_name) ] body
  in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let postcond_case config state idx state_ident new_state_ident value =
  let open Reserr in
  let translate_postcond t =
    subst_term state ~gos_t:value.sut_var ~old_t:(Some state_ident) ~new_lz:true
      ~new_t:(Some new_state_ident) t
    >>= ocaml_of_term config
  in
  let idx = List.sort Int.compare idx in
  let lhs0 = mk_cmd_pattern value in
  let* lhs1 =
    let ret_ty = Ir.get_return_type value in
    let* ret_ty =
      let open Ppxlib in
      match ret_ty.ptyp_desc with
      | Ptyp_var _ | Ptyp_constr _ -> ok ret_ty
      | _ ->
          error
            ( Type_not_supported (Fmt.str "%a" Pprintast.core_type ret_ty),
              ret_ty.ptyp_loc )
    in
    let* pat_ty = pat_of_core_type value.inst ret_ty in
    let pat_ty =
      if may_raise_exception value then
        ppat_construct (lident "Result")
          (Some (ppat_tuple [ pat_ty; ppat_construct (lident "Exn") None ]))
      else pat_ty
    in
    let pat_ret =
      match value.ret with
      | [] ->
          if may_raise_exception value then pvar (str_of_ident res_default)
          else ppat_any
      | [ id ] -> pvar (str_of_ident id)
      | _ ->
          failwith
            "shouldn't happen (functions returning tuples are filtered out \
             before)"
    in
    ok
      (ppat_construct (lident "Res")
         (Some (ppat_tuple [ ppat_tuple [ pat_ty; ppat_any ]; pat_ret ])))
  in
  let lhs = ppat_tuple [ lhs0; lhs1 ] in
  let* rhs =
    let normal =
      let rec aux idx postcond =
        match (idx, postcond) with
        | [], ps -> List.map snd ps
        | i :: idx, (j, _) :: ps when i = j -> aux idx ps
        | i :: _, (j, p) :: ps ->
            assert (j < i);
            p :: aux idx ps
        | _, _ -> assert false
      in
      aux idx value.postcond.normal
    in
    list_and <$> map translate_postcond normal
  in
  let res, pat_ret =
    match value.ret with
    | [] -> (evar (str_of_ident res_default), ppat_any)
    | [ id ] ->
        let id = str_of_ident id in
        (evar id, pvar id)
    | _ ->
        failwith
          "shouldn't happen (functions returning tuples are filtered out \
           before)"
  in
  let* rhs =
    if may_raise_exception value then
      let case_ok =
        case ~lhs:(ppat_construct (lident "Ok") (Some pat_ret)) ~guard:None ~rhs
      in
      let* cases_error =
        Fun.flip ( @ ) [ case ~lhs:ppat_any ~guard:None ~rhs:(ebool false) ]
        <$> map
              (fun (x, p, t) ->
                let lhs =
                  ppat_construct
                    (Fmt.str "%a" Ident.pp x.Gospel.Ttypes.xs_ident |> lident)
                    (Option.map Ortac_core.Ocaml_of_gospel.pattern p)
                in
                let lhs = ppat_construct (lident "Error") (Some lhs) in
                let* rhs = translate_postcond t in
                case ~lhs ~guard:None ~rhs |> ok)
              value.postcond.exceptional
      in
      pexp_match res (case_ok :: cases_error) |> ok
    else ok rhs
  in
  let* rhs =
    let translate_checks = translate_checks config state value state_ident in
    let* checks = map translate_checks value.postcond.checks in
    match checks with
    | [] -> ok rhs
    | _ ->
        let inv_arg =
          ppat_construct (lident "Invalid_argument") (Some ppat_any)
        in
        pexp_ifthenelse (list_and checks) rhs
          (Some
             (pexp_match res
                [
                  case
                    ~lhs:(ppat_construct (lident "Error") (Some inv_arg))
                    ~guard:None ~rhs:(ebool true);
                  case ~lhs:ppat_any ~guard:None ~rhs:(ebool false);
                ]))
        |> ok
  in
  ok (case ~lhs ~guard:None ~rhs)

let postcond config idx ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let res_name = gen_symbol ~prefix:"res" () in
  let new_state_name = gen_symbol ~prefix:"new_state" () in
  let new_state_let =
    pexp_let Nonrecursive
      [
        value_binding ~pat:(pvar new_state_name)
          ~expr:
            (pexp_lazy
               (pexp_apply
                  (pexp_ident (lident "next_state"))
                  [
                    (Nolabel, pexp_ident (lident cmd_name));
                    (Nolabel, pexp_ident (lident state_name));
                  ]));
      ]
  in
  let state_ident = Ident.create ~loc:Location.none state_name in
  let new_state_ident = Ident.create ~loc:Location.none new_state_name in
  let open Reserr in
  let* cases =
    (Fun.flip ( @ )) [ case ~lhs:ppat_any ~guard:None ~rhs:(ebool true) ]
    <$> map
          (fun v ->
            postcond_case config ir.state (List.assoc v.id idx) state_ident
              new_state_ident v)
          ir.values
  in
  let body =
    pexp_match (pexp_tuple [ evar cmd_name; evar res_name ]) cases
    |> new_state_let
  in
  let pat = pvar "postcond" in
  let expr =
    efun
      [
        (Nolabel, pvar cmd_name);
        (Nolabel, pvar state_name);
        (Nolabel, pvar res_name);
      ]
      body
  in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let cmd_constructor value =
  let name = String.capitalize_ascii value.id.Ident.id_str |> noloc in
  let args =
    List.map (fun (ty, _) -> subst_core_type value.inst ty) value.args
  in
  constructor_declaration ~name ~args:(Pcstr_tuple args) ~res:None

let state_type ir =
  let lds =
    List.map
      (fun (id, ty) ->
        label_declaration
          ~name:(Fmt.str "%a" Ident.pp id |> noloc)
          ~mutable_:Immutable ~type_:ty)
      ir.state
  in
  let kind = Ptype_record lds in
  let td =
    type_declaration ~name:(noloc "state") ~params:[] ~cstrs:[] ~kind
      ~private_:Public ~manifest:None
  in
  pstr_type Nonrecursive [ td ]

let cmd_type ir =
  let constructors = List.map cmd_constructor ir.values in
  let td =
    type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant constructors) ~private_:Public ~manifest:None
  in
  pstr_type Recursive [ td ]

let pp_cmd_case value =
  let lhs = mk_cmd_pattern value in
  let qualify_pp = qualify [ "Util"; "Pp" ] in
  let get_name =
    Option.fold ~none:eunit ~some:(fun id -> str_of_ident id |> evar)
  in
  let open Reserr in
  let rec pp_of_ty ty : expression reserr =
    match ty.ptyp_desc with
    | Ptyp_tuple [ ty0; ty1 ] ->
        let* pp0 = pp_of_ty ty0 and* pp1 = pp_of_ty ty1 in
        ok (pexp_apply (evar "pp_pair") [ (Nolabel, pp0); (Nolabel, pp1) ])
    | Ptyp_constr (lid, xs) ->
        let* xs = map pp_of_ty xs and* s = munge_longident false ty lid in
        let pp = qualify_pp ("pp_" ^ s) in
        ok
          (match xs with
          | [] -> pp
          | _ -> pexp_apply pp (List.map (fun x -> (Nolabel, x)) xs))
    | _ ->
        error
          (Type_not_supported (Fmt.str "%a" Pprintast.core_type ty), ty.ptyp_loc)
  in
  let* rhs =
    let name = str_of_ident value.id in
    let* pp_args =
      concat_map
        (fun (ty, id) ->
          let ty = subst_core_type value.inst ty in
          let* pp = pp_of_ty ty in
          ok [ pexp_apply pp [ (Nolabel, ebool true) ]; get_name id ])
        value.args
    in
    let fmt =
      String.concat " " ("%s" :: List.map (Fun.const "%a") value.args)
      |> estring
    in
    let args =
      List.map (fun x -> (Nolabel, x)) (fmt :: estring name :: pp_args)
    in
    pexp_apply (qualify [ "Format" ] "asprintf") args |> ok
  in
  case ~lhs ~guard:None ~rhs |> ok

let cmd_show ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let open Reserr in
  let* cases = map pp_cmd_case ir.values in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "show_cmd" in
  let expr = efun [ (Nolabel, pvar cmd_name) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let sut_type cfg =
  let td =
    type_declaration ~name:(noloc "sut") ~params:[] ~cstrs:[]
      ~kind:Ptype_abstract ~private_:Public
      ~manifest:(Some cfg.Cfg.sut_core_type)
  in
  pstr_type Recursive [ td ]

(* This function generates an expression of the form
   ```
   let a = expr0 in
   let b = expr1 in
   { field0 = (translation of a gospel term using a and/or b; ... }
   ```
*)
let init_state config ir =
  let pat_of_lb_arg = function
    (* here we don't need the labels as we'll use them in the body of the function *)
    | Gospel.Tast.Lunit -> punit
    | Gospel.Tast.Lnone vs
    | Gospel.Tast.Loptional vs
    | Gospel.Tast.Lnamed vs
    | Gospel.Tast.Lghost vs ->
        pvar (Fmt.str "%a" Ident.pp vs.vs_name)
  in
  let bindings =
    pexp_let Nonrecursive
      (List.map
         (fun (lb_arg, expr) -> value_binding ~pat:(pat_of_lb_arg lb_arg) ~expr)
         ir.Ir.init_state.arguments)
  in
  let open Reserr in
  let translate_field_desc Ir.{ model; description } =
    let* desc =
      subst_term ir.state ~gos_t:ir.init_state.returned_sut ~old_t:None
        ~new_t:None description
      >>= ocaml_of_term config
    in
    ok (model, desc)
  in
  let* fields = map translate_field_desc ir.Ir.init_state.descriptions in
  let* fields =
    map
      (fun (id, _) ->
        (fun d -> (longident_loc_of_ident id, d))
        <$> (List.assoc_opt id fields
            |> of_option
                 ~default:
                   ( Impossible_init_state_generation
                       (No_translatable_specification id.Ident.id_str),
                     Ppxlib.Location.none )))
      ir.state
  in
  let expr = pexp_record fields None |> bindings in
  let pat = pvar "init_state" in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let ghost_function config fct =
  let open Gospel in
  let open Tast in
  let open Reserr in
  match fct.fun_def with
  | None -> failwith "impossible"
  | Some t ->
      let name = str_of_ident fct.fun_ls.ls_name in
      let config' =
        Cfg.
          {
            config with
            context =
              Ortac_core.Context.add_function fct.fun_ls name config.context;
          }
      in
      let* body = ocaml_of_term (if fct.fun_rec then config' else config) t in
      let body =
        efun
          (List.map
             (fun vs -> (Nolabel, pvar (str_of_ident vs.Symbols.vs_name)))
             fct.fun_params)
          body
      in
      let bindings = [ value_binding ~pat:(pvar name) ~expr:body ] in
      ( config',
        pstr_value (if fct.fun_rec then Recursive else Nonrecursive) bindings )
      |> ok

let ghost_functions config =
  let open Reserr in
  let rec aux config (acc : structure) = function
    | [] -> ok (config, List.rev acc)
    | fct :: xs -> (
        let* f = promote_opt (ghost_function config fct) in
        match f with
        | None -> aux config acc xs
        | Some (config, f) -> aux config (f :: acc) xs)
  in
  aux config []

let stm config ir =
  let open Reserr in
  let* config, ghost_functions = ghost_functions config ir.ghost_functions in
  let warn = [%stri [@@@ocaml.warning "-26-27"]] in
  let incl =
    Option.map
      (fun m ->
        let open Ast_helper in
        String.capitalize_ascii m
        |> lident
        |> Mod.ident
        |> Incl.mk
        |> pstr_include)
      config.include_
    |> Option.to_list
  in
  let sut = sut_type config in
  let cmd = cmd_type ir in
  let* cmd_show = cmd_show ir in
  let state = state_type ir in
  let* idx, next_state = next_state config ir in
  let* postcond = postcond config idx ir in
  let* precond = precond config ir in
  let* run = run config ir in
  let* arb_cmd = arb_cmd ir in
  let* init_state = init_state config ir in
  let cleanup =
    let pat = pvar "cleanup" in
    let expr = efun [ (Nolabel, ppat_any) ] eunit in
    pstr_value Nonrecursive [ value_binding ~pat ~expr ]
  in
  let init_sut =
    let pat = pvar "init_sut" in
    let expr = efun [ (Nolabel, punit) ] config.Cfg.init_sut in
    pstr_value Nonrecursive [ value_binding ~pat ~expr ]
  in
  let open_mod m = pstr_open Ast_helper.(Opn.mk (Mod.ident (lident m))) in
  let spec_expr =
    pmod_structure
      ([ open_mod "STM"; warn ]
      @ incl
      @ [
          sut;
          cmd;
          cmd_show;
          state;
          init_state;
          init_sut;
          cleanup;
          arb_cmd;
          next_state;
          precond;
          postcond;
          run;
        ])
  in
  let stm_spec =
    pstr_module (module_binding ~name:(noloc (Some "Spec")) ~expr:spec_expr)
  in
  let tests =
    pstr_module
      (module_binding ~name:(noloc (Some "STMTests"))
         ~expr:
           (pmod_apply
              (pmod_ident (Ldot (Lident "STM_sequential", "Make") |> noloc))
              (pmod_ident (lident "Spec"))))
  in
  let module_name = Ortac_core.Context.module_name config.context in
  let call_tests =
    let loc = Location.none in
    let descr = estring (module_name ^ " STM tests") in
    let expr =
      [%expr
        QCheck_base_runner.run_tests_main
          (let count = 1000 in
           [ STMTests.agree_test ~count ~name:[%e descr] ])]
    in
    let expr =
      match config.protect_call with
      | None -> expr
      | Some f ->
          pexp_apply (qualify [ "Spec" ] f)
            [ (Nolabel, efun [ (Nolabel, punit) ] expr) ]
    in
    pstr_value Nonrecursive [ value_binding ~pat:ppat_any ~expr ]
  in
  ok
    ([ open_mod module_name ]
    @ ghost_functions
    @ [ stm_spec; tests; call_tests ])
