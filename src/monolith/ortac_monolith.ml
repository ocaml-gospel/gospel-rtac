open Ppxlib

module M : Ortac_core.Backend.S = struct
  let prelude =
    let loc = Location.none in
    [
      [%stri open Ortac_runtime];
      [%stri
        module Errors = struct
          type t = error_report

          let create loc fun_name = { loc; fun_name; errors = [] }

          let register t e = t.errors <- e :: t.errors

          let is_pre = function
            | Violated_condition e -> e.term_kind = Pre
            | Specification_failure e -> e.term_kind = Pre
            | _ -> false

          let report t =
            match t.errors with
            | [] -> ()
            | errs when List.exists is_pre errs -> raise Monolith.PleaseBackOff
            | _ ->
                Fmt.flush Fmt.stderr (pp_error_report Fmt.stderr t);
                (* pp_error_report Fmt.stderr t; *)
                raise (Error t)
        end];
    ]
end

module G = Ortac_core.Ortac.Make (M)
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let mk_reference module_name env s =
  let rtac = G.signature module_name env s in
  let module_r = A.pmod_structure ~loc rtac in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "R")) ~expr:module_r
  in
  A.pstr_module ~loc module_bind

let mk_candidate module_name =
  let module_c = A.pmod_ident ~loc (B.lident module_name) in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "C")) ~expr:module_c
  in
  A.pstr_module ~loc module_bind

let is_arrow = function Ptyp_arrow _ -> true | _ -> false

let rec translate_ret s =
  match s.ptyp_desc with
  | Ptyp_var _s -> [%expr sequential ()]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, _) -> [%expr unit]
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr int]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, _) -> [%expr bool]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ param ]) ->
      [%expr list [%e translate_ret param]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ param ]) ->
      [%expr M.array [%e translate_ret param]]
  | Ptyp_constr ({ txt = Lident ty; _ }, _) -> B.evar (Printf.sprintf "S.%s" ty)
  | _ -> failwith "monolith deconstructible spec not implemented yet"

let rec translate s =
  match s.ptyp_desc with
  | Ptyp_var _s -> [%expr sequential ()]
  | Ptyp_constr ({ txt = Lident "unit"; _ }, _params) -> [%expr unit]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, _params) -> [%expr bool]
  | Ptyp_constr ({ txt = Lident "char"; _ }, _params) -> [%expr char]
  | Ptyp_constr ({ txt = Lident "int"; _ }, _params) -> [%expr M.int]
  | Ptyp_constr ({ txt = Lident "string"; _ }, _params) -> [%expr string]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ param ]) ->
      [%expr list [%e translate param]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ param ]) ->
      [%expr M.array [%e translate param]]
  | Ptyp_arrow (_, x, y) when is_arrow y.ptyp_desc ->
      [%expr [%e translate x] ^> [%e translate y]]
  | Ptyp_arrow (_, x, y) -> [%expr [%e translate x] ^!> [%e translate_ret y]]
  | Ptyp_constr ({ txt = Lident ty; _ }, _) -> B.evar (Printf.sprintf "S.%s" ty)
  | _ ->
      failwith
        "monolith constructible spec not implemented yet (from translate)"

let mk_declaration (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_val (decl, _ghost) ->
      let fun_name = decl.vd_name.id_str in
      let fun_type = decl.vd_type in
      let msg = B.estring (Printf.sprintf "%s is Ok" fun_name) in
      let reference = Printf.sprintf "R.%s" fun_name in
      let candidate = Printf.sprintf "C.%s" fun_name in
      Some
        [%expr
          let spec = [%e translate fun_type] in
          declare [%e msg] spec [%e B.evar reference] [%e B.evar candidate]]
  | _ -> None

let mk_declarations s =
  match List.filter_map mk_declaration s with
  | [] -> raise (failwith "module is empty")
  | [ e ] -> [%stri let () = [%e e]]
  | e1 :: es -> [%stri let () = [%e List.fold_left B.pexp_sequence e1 es]]

let mk_specs s =
  let main =
    [%stri
      let () =
        let fuel = 100 in
        main fuel]
  in
  [ mk_declarations s; main ]

let standalone module_name env s =
  let driver = Ortac_core.Drv.v env in
  let module_r = mk_reference module_name env s in
  let module_c = mk_candidate module_name in
  let module_g = Generators.generators driver s in
  let module_p = Printers.printers driver s in
  let module_s = Spec.specs s in
  let specs = mk_specs s in
  [%stri open Monolith]
  :: [%stri module M = Ortac_runtime_monolith]
  :: module_r :: module_c :: module_g :: module_p :: module_s :: specs

let generate path =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  standalone module_name env sigs |> Ppxlib_ast.Pprintast.structure Fmt.stdout
