open Ppxlib
open Gospel

let loc = Location.none

module A = Ast_builder.Default
module B = Ortac_core.Builder

let rec ty2gen (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar tvs -> tvs2gen tvs
  | Tyapp (tys, tyl) -> tys2gen tys tyl

and tvs2gen (tvs : Ttypes.tvsymbol) =
  match tvs.tv_name.id_str with
  | "unit" -> [%expr Gen.unit]
  | "bool" -> [%expr Gen.bool]
  | "char" -> [%expr Gen.char]
  | "int" -> [%expr Gen.int 1024]
  | "string" -> [%expr Gen.string (Gen.int 1024) Gen.char]
  | s -> failwith (Printf.sprintf "%s is not yet implemented" s)

and tys2gen (tys : Ttypes.tysymbol) (tyl : Ttypes.ty list) =
  match tys.ts_ident.id_str with
  | "unit" -> [%expr Gen.unit]
  | "bool" -> [%expr Gen.bool]
  | "char" -> [%expr Gen.char]
  | "int" -> [%expr Gen.int 1024]
  | "string" -> [%expr Gen.string (Gen.int 1024) Gen.char]
  | "array" -> [%expr Gen.array (Gen.int Int.max_int) [%e ty2gen (List.hd tyl)]]
  | "list" -> [%expr Gen.list (Gen.int Int.max_int) [%e ty2gen (List.hd tyl)]]
  | s -> failwith (Printf.sprintf "%s is not yet implemented" s)

let lsymbol2gen (ls : Tterm.lsymbol) =
  match ls.ls_value with
  | Some ty -> ty2gen ty
  | None -> failwith "can't find type to build generator"

let variant_generator (constructors : Tast.constructor_decl list) =
  let name (c : Tast.constructor_decl) =
    Printf.sprintf "R.%s" c.cd_cs.ls_name.id_str
  in
  let arg (c : Tast.constructor_decl) = c.cd_cs.ls_args in
  let gen (c : Tast.constructor_decl) =
    let gen = arg c |> List.map ty2gen in
    List.map (fun e -> [%expr [%e e] ()]) gen |> A.pexp_tuple_opt ~loc
  in
  let variant (c : Tast.constructor_decl) =
    let name = name c in
    let gen = gen c in
    A.pexp_variant ~loc name gen
  in
  let variants = List.map variant constructors in
  let v = A.pexp_array ~loc variants in
  [%expr
    fun () ->
      let v = [%e v] in
      v.(Gen.int (Array.length v) ())]

let record_generator (rec_decl : Tast.rec_declaration) =
  let field (ld : Tterm.lsymbol Tast.label_declaration) =
    Printf.sprintf "R.%s" ld.ld_field.ls_name.id_str
  in
  let gen (ld : Tterm.lsymbol Tast.label_declaration) =
    let gen = lsymbol2gen ld.ld_field in
    [%expr [%e gen] ()]
  in
  let r =
    A.pexp_record ~loc
      (List.map (fun ld -> (B.lident (field ld), gen ld)) rec_decl.rd_ldl)
      None
  in
  [%expr fun () -> [%e r]]

let generator_expr (ty_kind : Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> None
  | Pty_variant constructors -> Some (variant_generator constructors)
  | Pty_record rec_decl -> Some (record_generator rec_decl)
  | Pty_open -> None

let generator_definition (type_decl : Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  match generator_expr type_decl.td_kind with
  | None -> None
  | Some generator -> Some [%stri let [%p id] = [%e generator]]

let generator_option (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_type (_, [ type_decl ], _) -> generator_definition type_decl
  | _ -> None

let generators s =
  let gen = List.filter_map generator_option s in
  let module_g = A.pmod_structure ~loc gen in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "G")) ~expr:module_g
  in
  A.pstr_module ~loc module_bind
