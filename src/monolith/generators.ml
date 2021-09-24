open Ppxlib
open Gospel

let loc = Location.none

module A = Ast_builder.Default
module B = Ortac_core.Builder

let rec ty2gen drv (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar _tvs -> [%expr Gen.sequential]
  | Tyapp (tys, tyl) -> tyapp2gen drv tys tyl

and tyapp2gen drv (tys : Ttypes.tysymbol) (tyl : Ttypes.ty list) =
  let get_ts = Ortac_core.Drv.get_ts drv in
  if Ttypes.ts_equal tys Ttypes.ts_unit then [%expr Gen.unit]
  else if Ttypes.ts_equal tys Ttypes.ts_bool then [%expr Gen.bool]
  else if Ttypes.ts_equal tys Ttypes.ts_char then [%expr Gen.char]
  else if Ttypes.ts_equal tys Ttypes.ts_integer then [%expr Gen.int]
  else if Ttypes.ts_equal tys Ttypes.ts_string then
    [%expr Gen.string (Gen.int 1024) Gen.char]
  else if Ttypes.ts_equal tys Ttypes.ts_list && List.length tyl = 1 then
    [%expr Gen.list [%e ty2gen drv (List.hd tyl)]]
  else if Ttypes.is_ts_tuple tys then tuple drv tyl
  else if Ttypes.ts_equal tys (get_ts [ "Gospelstdlib"; "int" ]) then
    [%expr Gen.int Int.max_int]
  else if
    Ttypes.ts_equal tys (get_ts [ "Gospelstdlib"; "array" ])
    && List.length tyl = 1
  then [%expr Gen.array [%e ty2gen drv (List.hd tyl)]]
  else
    failwith
      (Printf.sprintf "%s generator is not yet implemented from tys2printer"
         tys.ts_ident.id_str)

and tuple drv tyl =
  let tuple =
    A.pexp_tuple ~loc
      (List.map (fun ts -> A.eapply ~loc (ty2gen drv ts) [ [%expr ()] ]) tyl)
  in
  [%expr fun () -> [%e tuple]]

let lsymbol2gen drv (ls : Tterm.lsymbol) =
  match ls.ls_value with
  | Some ty -> ty2gen drv ty
  | None -> failwith "can't find type to build generator"

let variant_generator drv (constructors : Tast.constructor_decl list) =
  let name (c : Tast.constructor_decl) =
    Printf.sprintf "R.%s" c.cd_cs.ls_name.id_str |> B.evar
  in
  let arg (c : Tast.constructor_decl) = c.cd_cs.ls_args in
  let gen (c : Tast.constructor_decl) =
    let ty2gen = ty2gen drv in
    let gen = arg c |> List.map ty2gen in
    List.map (fun e -> [%expr [%e e] ()]) gen |> A.pexp_tuple_opt ~loc
  in
  let variant (c : Tast.constructor_decl) =
    let name = name c in
    let gen = gen c in
    match gen with
    | None -> [%expr fun () -> [%e name]]
    | Some gen -> [%expr fun () -> [%e name] [%e gen]]
  in
  let variants = List.map variant constructors |> A.pexp_array ~loc in
  [%expr
    fun () ->
      let v = [%e variants] in
      v.(Gen.int (Array.length v) ()) ()]

let record_generator drv (rec_decl : Tast.rec_declaration) =
  let field (ld : Tterm.lsymbol Tast.label_declaration) =
    Printf.sprintf "R.%s" ld.ld_field.ls_name.id_str
  in
  let gen (ld : Tterm.lsymbol Tast.label_declaration) =
    let gen = lsymbol2gen drv ld.ld_field in
    [%expr [%e gen] ()]
  in
  let r =
    A.pexp_record ~loc
      (List.map (fun ld -> (B.lident (field ld), gen ld)) rec_decl.rd_ldl)
      None
  in
  [%expr fun () -> [%e r]]

let generator_expr drv (ty_kind : Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> None
  | Pty_variant constructors -> Some (variant_generator drv constructors)
  | Pty_record rec_decl -> Some (record_generator drv rec_decl)
  | Pty_open -> None

let generator_definition drv (type_decl : Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  match generator_expr drv type_decl.td_kind with
  | None -> None
  | Some generator -> Some [%stri let [%p id] = [%e generator]]

let generator_option drv (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_type (_, [ type_decl ], _) -> generator_definition drv type_decl
  | _ -> None

let generators drv s =
  let gen = List.filter_map (generator_option drv) s in
  let module_g = A.pmod_structure ~loc gen in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "G")) ~expr:module_g
  in
  A.pstr_module ~loc module_bind
