module Ir = Ortac_wrapper__Ir
module Utils = Ortac_core__Utils

let translate path =
  let open Utils in
  let { module_name; namespace; ast } = check (Registration.MLI path) in
  let context = Ortac_core.Context.init module_name namespace in
  Ortac_wrapper__Ir_of_gospel.signature ~context ast

let mutability =
  let open Ir in
  let pp_mut ppf (m : mutability) =
    match m with
    | Unknown -> Fmt.pf ppf "Unknown"
    | Immutable -> Fmt.pf ppf "Immutable"
    | Mutable -> Fmt.pf ppf "Mutable"
    | Dependant _ -> Fmt.pf ppf "Dependant"
  in
  let eq m n =
    match (m, n) with
    | Unknown, Unknown
    | Immutable, Immutable
    | Mutable, Mutable
    | Dependant _, Dependant _ ->
        true
    | _ -> false
  in
  Alcotest.testable pp_mut eq

let get_mutability = function Ir.Type t -> t.mutable_ | _ -> assert false
let is_pure = function Ir.Value v -> v.pure | _ -> assert false
let type_name = function Ir.Type t -> t.name | _ -> assert false
let val_name = function Ir.Value v -> v.name | _ -> assert false

let mutability_to_string = function
  | Ir.Mutable -> "mutable"
  | Ir.Unknown -> "unknown"
  | Ir.Dependant _ -> "dependant"
  | Ir.Immutable -> "immutable"

let is_val (t : Ir.structure_item) =
  match t with Ir.Value _ -> true | _ -> false

let test_mutability path mut flag () =
  let translations = translate path in
  Ir.iter_translation
    ~f:(fun t ->
      if !flag then (
        print_endline (type_name t);
        Alcotest.(check mutability)
          (Fmt.str "%s is %s" (type_name t) (mutability_to_string mut))
          mut (get_mutability t))
      else if is_val t then flag := true)
    translations

let type_unknown () =
  let flag = ref false in
  test_mutability "./translation/unknown.mli" Ir.Unknown flag

let type_mutable () =
  let flag = ref false in
  test_mutability "./translation/mutable.mli" Ir.Mutable flag

let type_immutable () =
  let flag = ref false in
  test_mutability "./translation/immutable.mli" Ir.Immutable flag

let type_dependant () =
  let flag = ref false in
  test_mutability "./translation/dependant.mli"
    (Ir.Dependant (fun _ -> Ir.Unknown))
    flag

let val_pure () =
  let translations = translate "./translation/pure.mli" in
  Ir.iter_translation
    ~f:(fun v ->
      Alcotest.(check bool) (Fmt.str "%s is pure" (val_name v)) true (is_pure v))
    translations

let suite =
  ( "Translation",
    [
      ("type mutable", `Quick, type_mutable ());
      ("type immutable", `Quick, type_immutable ());
      ("type unknown", `Quick, type_unknown ());
      ("type dependant", `Quick, type_dependant ());
      ("value purity", `Quick, val_pure);
    ] )
