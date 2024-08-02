module W = Ortac_core.Warnings
open Ppxlib
open Ortac_core.Builder
open Ir
module F = Failure
module M = Map.Make (String)
module Context = Ortac_core.Context

let setup name loc register_name next =
  [%expr
    let [%p pvar register_name] =
      Ortac_runtime.Errors.create [%e elocation loc] [%e estring name]
    in
    [%e next]]

let term (t : Ir.term) next =
  match t.translation with Error _ -> next | Ok c -> pexp_sequence c next

let terms terms next = List.fold_left (fun acc t -> term t acc) next terms

let compute_check (c : Ir.check) next =
  match c.translations with
  | Error _ -> next
  | Ok ((x, e), _) ->
      pexp_let Nonrecursive [ value_binding ~pat:(pvar x) ~expr:e ] next

let compute_checks checks next =
  List.fold_left (fun acc t -> compute_check t acc) next checks

let check_positive (c : Ir.check) next =
  match c.translations with
  | Error _ -> next
  | Ok (_, p) -> pexp_sequence p next

let checks ~register_name positive checks next =
  if positive then
    List.fold_left (fun acc t -> check_positive t acc) next checks
  else
    let all_checks =
      List.fold_left
        (fun acc c ->
          match c.translations with
          | Error _ -> acc
          | Ok ((x, _), _) -> eapply (evar "(&&)") [ acc; evar x ])
        (ebool true) checks
    in
    [%expr
      if [%e all_checks] then [%e F.unexpected_checks ~register_name];
      [%e next]]

let invariants ~register_name pos instance (t : type_) next =
  List.fold_left
    (fun next (t : invariant) ->
      match t.translation with
      | Error _ -> next
      | Ok (name, _) ->
          pexp_sequence
            (eapply (evar name) [ register_name; evar pos; evar instance ])
            next)
    next t.invariants

let var_invariants ~register_name pos ignore_consumes (v : ocaml_var) next =
  if ignore_consumes && v.consumed then next
  else invariants ~register_name pos v.name v.type_ next

let vars_invariants ~register_name pos ignore_consumes vl next =
  List.fold_left
    (fun acc t -> var_invariants ~register_name pos ignore_consumes t acc)
    next vl

let group_xpost (v : Ir.value) =
  let register_name = evar v.register_name in
  let invariants = vars_invariants ~register_name "XPost" true v.arguments in
  let default_cases =
    [
      case ~guard:None
        ~lhs:[%pat? (Stack_overflow | Out_of_memory) as e]
        ~rhs:
          [%expr
            [%e
              checks ~register_name true v.checks
              @@ invariants
              @@ F.report ~register_name];
            raise e];
      case ~guard:None
        ~lhs:[%pat? e]
        ~rhs:
          [%expr
            [%e F.unexpected_exn ~allowed_exn:[] ~exn:(evar "e") ~register_name];
            [%e
              checks ~register_name true v.checks
              @@ invariants
              @@ F.report ~register_name];
            raise e];
    ]
  in
  let default_cases =
    let invalid_arg_case =
      case ~guard:None
        ~lhs:[%pat? Invalid_argument _ as e]
        ~rhs:
          [%expr
            [%e
              checks ~register_name false v.checks
              @@ invariants
              @@ F.report ~register_name];
            raise e]
    in
    if v.checks = [] then default_cases else invalid_arg_case :: default_cases
  in
  let tbl = Hashtbl.create 0 in
  let rec aux keys = function
    | [] -> keys
    | { exn; args; translation = Ok translation } :: t ->
        Hashtbl.add tbl exn translation;
        aux (M.add exn args keys) t
    | _ :: t -> aux keys t
  in
  aux M.empty v.xpostconditions |> fun s ->
  M.fold
    (fun exn args acc ->
      let e = gen_symbol ~prefix:"__e_" () in
      let lhs =
        ppat_alias
          (ppat_construct (lident exn)
             (if args = 0 then None else Some ppat_any))
          (noloc e)
      in
      let matches =
        Hashtbl.find_all tbl exn |> List.map (pexp_match (evar e)) |> esequence
      in
      let rhs =
        esequence
          [
            matches;
            checks ~register_name true v.checks
            @@ invariants
            @@ F.report ~register_name;
            eapply (evar "raise") [ evar e ];
          ]
      in
      case ~guard:None ~lhs ~rhs :: acc)
    s default_cases

let args f = List.map (fun a -> (a.label, f a.name))

let rets (returns : ocaml_var list) =
  match returns with
  | [] -> (eunit, punit)
  | [ x ] -> (evar x.name, pvar x.name)
  | ret ->
      List.fold_right
        (fun (x : ocaml_var) (e, p) -> (evar x.name :: e, pvar x.name :: p))
        ret ([], [])
      |> fun (e, p) -> (pexp_tuple e, ppat_tuple p)

let value (v : Ir.value) =
  let register_name = evar v.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let eargs = args evar v.arguments in
  let pargs = args pvar v.arguments in
  let eret, pret = rets v.returns in
  let call = pexp_apply (evar v.name) eargs in
  let try_call = pexp_try call (group_xpost v) in
  let body =
    setup v.name v.loc v.register_name
    @@ terms v.preconditions
    @@ vars_invariants ~register_name "Pre" false v.arguments
    @@ compute_checks v.checks
    @@ report
    @@ pexp_let Nonrecursive [ value_binding ~pat:pret ~expr:try_call ]
    @@ terms v.postconditions
    @@ checks ~register_name true v.checks
    @@ vars_invariants ~register_name "Post" true v.arguments
    @@ vars_invariants ~register_name "Post" false v.returns
    @@ report
    @@ eret
  in
  [ [%stri let [%p pvar v.name] = [%e efun pargs body]] ]

let function_ (f : Ir.function_) =
  match f.definition with
  | Some { translation = Ok def; _ } ->
      let pat = pvar f.name in
      let pargs = args pvar f.arguments in
      let expr = efun pargs def in
      let rec_flag = if f.rec_ then Recursive else Nonrecursive in
      [ pstr_value rec_flag [ value_binding ~pat ~expr ] ]
  | _ -> []

let constant (c : Ir.constant) =
  let register_name = evar c.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let body =
    setup c.name c.loc c.register_name
    @@ terms c.checks
    @@ invariants ~register_name "Pre" c.name c.type_
    @@ report
    @@ evar c.name
  in
  [ [%stri let [%p pvar c.name] = [%e body]] ]

let type_ (t : Ir.type_) =
  List.filter_map
    (fun (i : invariant) -> Result.to_option i.translation |> Option.map snd)
    t.invariants

let axiom (a : Ir.axiom) =
  let register_name = evar a.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let body =
    setup a.name a.loc a.register_name @@ term a.definition @@ report @@ eunit
  in
  [ [%stri let () = [%e body]] ]

let structure runtime module_name ir : Ppxlib.structure =
  (pmod_ident (lident module_name) |> include_infos |> pstr_include)
  :: pstr_module
       (module_binding
          ~name:{ txt = Some "Ortac_runtime"; loc }
          ~expr:(pmod_ident (lident runtime)))
  :: (Ir.map_translation ir ~f:(function
        | Ir.Value v -> value v
        | Ir.Function f -> function_ f
        | Ir.Predicate f -> function_ f
        | Ir.Constant c -> constant c
        | Ir.Type t -> type_ t
        | Ir.Axiom a -> axiom a)
     |> List.flatten)

let signature ~runtime ~module_name namespace s =
  let open Ortac_core in
  let context = Context.init module_name namespace in
  let ir = Ir_of_gospel.signature ~context s in
  Report.emit_warnings Fmt.stderr ir;
  structure runtime (Context.module_name context) ir

let generate path fmt =
  let open Ortac_core.Utils in
  let { module_name; namespace; ast } = check path in
  signature ~runtime:"Ortac_runtime" ~module_name namespace ast
  |> Fmt.pf fmt "%a@." Ppxlib_ast.Pprintast.structure
