open Gospel
open Tast
module Cfg = Config
module Ident = Identifier.Ident

let constant_test vd =
  let open Reserr in
  match vd.vd_args with
  | [] ->
      (Constant_value (Fmt.str "%a" Ident.pp vd.vd_name), vd.vd_loc) |> error
  | _ -> ok ()

let is_a_function ty =
  let open Ppxlib in
  match ty.ptyp_desc with Ptyp_arrow (_, _, _) -> true | _ -> false

let unify value_name sut_ty ty =
  let open Ppxlib in
  let open Reserr in
  let add_if_needed a x i =
    match List.assoc_opt a i with
    | None -> ok ((a, x) :: i)
    | Some y when x.ptyp_desc = y.ptyp_desc -> ok i
    | _ -> error (Incompatible_type value_name, ty.ptyp_loc)
  in
  let rec aux i = function
    | [], [] -> ok i
    | x :: xs, y :: ys ->
        let* i =
          match (x.ptyp_desc, y.ptyp_desc) with
          | _, Ptyp_var a -> add_if_needed a x i
          | Ptyp_tuple xs, Ptyp_tuple ys -> aux i (xs, ys)
          | Ptyp_constr (c, xs), Ptyp_constr (d, ys) when c.txt = d.txt ->
              aux i (xs, ys)
          | _ -> error (Incompatible_type value_name, ty.ptyp_loc)
        in
        aux i (xs, ys)
    | _, _ -> error (Incompatible_type value_name, ty.ptyp_loc)
  in
  match (sut_ty.ptyp_desc, ty.ptyp_desc) with
  | Ptyp_constr (t, args_sut), Ptyp_constr (t', args_ty) when t.txt = t'.txt ->
      aux [] (args_sut, args_ty)
  | _ -> failwith "Case should not happen in `unify'"

let ty_var_substitution config (vd : val_description) =
  let value_name, value_type = (vd.vd_name.id_str, vd.vd_type) in
  assert (is_a_function value_type);
  let open Ppxlib in
  let ret = function
    | None -> Reserr.(error (No_sut_argument value_name, value_type.ptyp_loc))
    | Some x -> Reserr.ok x
  in
  let rec aux seen ty =
    match ty.ptyp_desc with
    | Ptyp_any | Ptyp_var _ -> ret seen
    | Ptyp_arrow (_, l, r) ->
        if Cfg.is_sut config l then
          match seen with
          | None ->
              let open Reserr in
              let* x = unify value_name config.sut_core_type l in
              aux (Some x) r
          | Some _ ->
              Reserr.(
                error (Multiple_sut_arguments value_name, value_type.ptyp_loc))
        else aux seen r
    | Ptyp_tuple elems ->
        if List.for_all (fun t -> not (Cfg.is_sut config t)) elems then ret seen
        else Reserr.(error (Returning_sut value_name, ty.ptyp_loc))
    | Ptyp_constr (_, _) ->
        if not (Cfg.is_sut config ty) then ret seen
        else Reserr.(error (Returning_sut value_name, ty.ptyp_loc))
    (* not supported *)
    | Ptyp_object (_, _)
    | Ptyp_class (_, _)
    | Ptyp_alias (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_poly (_, _)
    | Ptyp_package _ | Ptyp_extension _ ->
        failwith "not supported"
  in
  aux None value_type

let split_args config ty args =
  let open Ppxlib in
  let rec aux sut acc ty args =
    match (ty.ptyp_desc, args) with
    | _, Lghost _ :: xs -> aux sut acc ty xs
    | Ptyp_arrow (_, l, r), Lnone vs :: xs
    | Ptyp_arrow (_, l, r), Loptional vs :: xs
    | Ptyp_arrow (_, l, r), Lnamed vs :: xs ->
        if Cfg.is_sut config l then aux (Some vs.vs_name) acc r xs
        else aux sut (Some vs.vs_name :: acc) r xs
    | Ptyp_arrow (_, _, r), Lunit :: xs -> aux sut (None :: acc) r xs
    | _, [] -> (sut, List.rev acc)
    | _, _ -> failwith "shouldn't happen (too few parameters)"
  in
  match aux None [] ty args with
  | None, _ -> failwith "shouldn't happen (sut type not found)"
  | Some sut, args -> (sut, args)

let next_state sut state spec =
  let open Tterm in
  let is_t vs =
    let open Symbols in
    Ident.equal sut vs.vs_name
  in
  let open Ir in
  let pred i t =
    match t.t_node with
    | Tapp (ls, [ { t_node = Tfield ({ t_node = Tvar vs; _ }, m); _ }; right ])
      when Symbols.(ls_equal ps_equ ls) && is_t vs ->
        if List.exists (fun (id, _) -> Ident.equal id m.ls_name) state then
          Some (i, { model = m.ls_name; description = right })
        else None
    | _ -> None
  in
  let formulae = List.mapi pred spec.sp_post |> List.filter_map Fun.id in
  let open Reserr in
  let check_modify = function
    | { t_node = Tvar vs; _ } when is_t vs -> List.map fst state |> ok
    | { t_node = Tfield ({ t_node = Tvar vs; _ }, m); _ } when is_t vs ->
        ok [ m.ls_name ]
    | t ->
        error
          ( Ignored_modifies (Fmt.str "%a" Gospel.Tterm_printer.print_term t),
            t.t_loc )
  in
  let* modifies = concat_map check_modify spec.sp_wr in
  let modifies = List.sort_uniq Ident.compare modifies in
  ok { formulae; modifies }

let postcond spec =
  let normal = List.mapi (fun i x -> (i, x)) spec.sp_post
  and exceptional =
    List.concat_map
      (fun (x, l) -> List.rev_map (fun (p, t) -> (x, p, t)) l)
      spec.sp_xpost
  in
  Ir.{ normal; exceptional; checks = spec.sp_checks }

let val_desc config state vd =
  let open Reserr in
  let* () = constant_test vd
  and* inst = ty_var_substitution config vd
  and* spec =
    of_option ~default:(No_spec vd.vd_name.id_str, vd.vd_loc) vd.vd_spec
  in
  let sut, args = split_args config vd.vd_type spec.sp_args in
  let ret =
    List.filter_map
      (function Lnone vs -> Some vs.vs_name | _ -> None)
      spec.sp_ret
    |> function
    | [] -> None
    | x :: _ -> Some x
  in
  let* next_state = next_state sut state spec in
  let postcond = postcond spec in
  Ir.value vd.vd_name vd.vd_type inst sut args ret next_state postcond |> ok

let sig_item config state s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> Some (val_desc config state vd)
  | _ -> None

let state config sigs =
  let sut_name = Cfg.get_sut_type_name_str config in
  let is_sut_decl s =
    let p t = t.td_ts.ts_ident.id_str = sut_name in
    match s.sig_desc with
    | Sig_type (_, ts, _) -> (
        List.filter p ts |> function
        | [] -> None
        | [ t ] -> Some t
        (* As multiple type declarations with the same name is illegal, last
           case can't happen *)
        | _ -> assert false)
    | _ -> None
  in
  let open Reserr in
  let* ty =
    match List.filter_map is_sut_decl sigs with
    | [] -> error (No_sut_type sut_name, Ppxlib.Location.none)
    | [ ty ] -> ok ty
    (* As multiple type declarations with the same name is illegal, last
       case can't happen *)
    | _ -> assert false
  in
  let open Ortac_core in
  let* subst =
    Fun.flip List.assoc_opt
    <$> (Ocaml_of_gospel.core_type_of_tysymbol ty.td_ts
        |> unify sut_name config.sut_core_type)
  and* spec =
    match ty.td_spec with
    | None -> error (Sut_type_not_specified sut_name, ty.td_loc)
    | Some spec -> ok spec
  in
  let process_model (ls, _) =
    let open Symbols in
    ( ls.ls_name,
      Option.get ls.ls_value |> Ocaml_of_gospel.core_type_of_ty_with_subst subst
    )
  in
  match spec.ty_fields with
  | [] -> error (No_models sut_name, spec.ty_loc)
  | xs -> List.map process_model xs |> ok

let signature config state sigs =
  List.filter_map (sig_item config state) sigs |> Reserr.promote

let run sigs config =
  let open Reserr in
  let open Ir in
  let* state = state config sigs in
  let* values = signature config state sigs in
  ok { state; values }
