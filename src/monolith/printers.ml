open Ppxlib
open Gospel
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let rec ty2printer (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar tvs -> tvs2printer tvs
  | Tyapp (tys, tyl) -> tys2printer tys tyl

and tvs2printer (tvs : Ttypes.tvsymbol) =
  match tvs.tv_name.id_str with
  | "unit" -> [%expr PPrintOCaml.unit]
  | "bool" -> [%expr PPrintOCaml.bool]
  | "char" -> [%expr PPrintOCaml.char]
  | "int" -> [%expr PPrintOCaml.int]
  | "string" -> [%expr PPrintOCaml.string]
  | s -> failwith (Printf.sprintf "%s printer is not yet implementer" s)

and tys2printer (tys : Ttypes.tysymbol) (tyl : Ttypes.ty list) =
  let aux i = ty2printer (List.nth tyl i) in
  match tys.ts_ident.id_str with
  | "unit" -> [%expr PPrintOCaml.unit]
  | "bool" -> [%expr PPrintOCaml.bool]
  | "char" -> [%expr PPrintOCaml.char]
  | "int" -> [%expr PPrintOCaml.int]
  | "string" -> [%expr PPrintOCaml.string]
  | "tuple2" -> [%expr M.Printer.tuple2 [%e aux 0] [%e aux 1]]
  | "tuple3" -> [%expr M.Printer.tuple3 [%e aux 0] [%e aux 1] [%e aux 2]]
  | "tuple4" ->
      [%expr M.printer.tuple4 [%e aux 0] [%e aux 1] [%e aux 2] [%e aux 3]]
  | "tuple5" ->
      [%expr
        M.printer.tuple5 [%e aux 0] [%e aux 1] [%e aux 2] [%e aux 3] [%e aux 4]]
  | s ->
      failwith
        (Printf.sprintf "%s printer is not yet implemented from tys2printer" s)

let lsymbol2printer (ls : Tterm.lsymbol) =
  match ls.ls_value with
  | Some ty -> ty2printer ty
  | None -> failwith "can't find type to build printer"

let mk_field (ld : Tterm.lsymbol Tast.label_declaration) =
  let field = ld.ld_field.ls_name.id_str in
  let printer = lsymbol2printer ld.ld_field in
  [%expr [%e B.estring field], [%e printer] [%e B.evar field]]

let record_printer (rec_decl : Tast.rec_declaration) =
  let fields =
    List.map
      (fun (ld : Tterm.lsymbol Tast.label_declaration) ->
        ld.ld_field.ls_name.id_str)
      rec_decl.rd_ldl
  in
  let fields_pat = String.concat ";" fields in
  let fields_printer = List.map mk_field rec_decl.rd_ldl in
  let pat = B.pvar (Printf.sprintf "R.{ %s }" fields_pat) in
  let l = B.elist fields_printer in
  [%expr fun [%p pat] -> PPrintOCaml.record "" [%e l]]

let ty2repr x (ty : Ttypes.ty) =
  let printer = ty2printer ty in
  [%expr [%e printer] [%e B.evar x]]

let variant_printer (constructors : Tast.constructor_decl list) =
  let variant (cd : Tast.constructor_decl) =
    let x = gen_symbol ~prefix:"__x" () in
    let lhs = Printf.sprintf "R.%s %s" cd.cd_cs.ls_name.id_str x |> B.pvar in
    let cname = B.estring cd.cd_cs.ls_name.id_str in
    let args = List.map (ty2repr x) cd.cd_cs.ls_args |> B.elist in
    let rhs = [%expr PPrintOCaml.variant "" [%e cname] 0 [%e args]] in
    A.case ~guard:None ~lhs ~rhs
  in
  let cases = List.map variant constructors in
  A.pexp_function ~loc cases

let printer_expr (ty_kind : Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> None
  | Pty_variant constructors -> Some (variant_printer constructors)
  | Pty_record rec_decl -> Some (record_printer rec_decl)
  | Pty_open -> None

let printer_definition (type_decl : Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  match printer_expr type_decl.td_kind with
  | None -> None
  | Some printer -> Some [%stri let [%p id] = [%e printer]]

let printer_option (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_type (_, [ type_decl ], _) -> printer_definition type_decl
  | _ -> None

let printers s =
  let name = B.noloc (Some "P") in
  let printers = List.filter_map printer_option s in
  let expr = A.pmod_structure ~loc printers in
  A.pstr_module ~loc (A.module_binding ~loc ~name ~expr)
