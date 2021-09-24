open Ppxlib
open Gospel
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let rec ty2printer drv (ty : Ttypes.ty) =
  match ty.ty_node with
  (* 'a is generated as int but the type checker doesn't know it *)
  | Tyvar _tvs -> [%expr fun _ -> Print.string "alpha value"]
  | Tyapp (tys, tyl) -> tyapp2printer drv tys tyl

and tyapp2printer drv (tys : Ttypes.tysymbol) (tyl : Ttypes.ty list) =
  let get_ts = Ortac_core.Drv.get_ts drv in
  if Ttypes.ts_equal tys Ttypes.ts_unit then [%expr Print.unit]
  else if Ttypes.ts_equal tys Ttypes.ts_bool then [%expr Print.bool]
  else if Ttypes.ts_equal tys Ttypes.ts_char then [%expr Print.char]
  else if Ttypes.ts_equal tys Ttypes.ts_integer then [%expr Print.int]
  else if Ttypes.ts_equal tys Ttypes.ts_string then [%expr Print.string]
  else if Ttypes.ts_equal tys Ttypes.ts_list && List.length tyl = 1 then
    [%expr Print.list [%e ty2printer drv (List.hd tyl)]]
  else if Ttypes.is_ts_tuple tys then tuple drv tyl
  else if Ttypes.ts_equal tys (get_ts [ "Gospelstdlib"; "int" ]) then
    [%expr Print.int]
  else if
    Ttypes.ts_equal tys (get_ts [ "Gospelstdlib"; "array" ])
    && List.length tyl = 1
  then [%expr Print.array [%e ty2printer drv (List.hd tyl)]]
  else
    failwith
      (Printf.sprintf "%s printer is not yet implemented from tys2printer"
         tys.ts_ident.id_str)

and tuple drv tyl =
  let ty2printer = ty2printer drv in
  let elts = List.map (fun _ -> gen_symbol ~prefix:"__t" ()) tyl in
  let vars = List.map B.evar elts in
  let pat = A.ppat_tuple ~loc (List.map B.pvar elts) in
  let printers = List.map ty2printer tyl in
  let rec tuple_helper acc vars printers =
    match (vars, printers) with
    | [], [] -> acc
    | v :: vs, p :: ps ->
        tuple_helper [%expr [%e acc] ^^ comma ^^ [%e p] [%e v]] vs ps
    | _, _ ->
        failwith
          "from Printers.tuple, lists should be both empty or both not empty"
  in
  let start vars printers =
    match (vars, printers) with
    | [], [] -> [%expr ()]
    | v :: vs, p :: ps -> tuple_helper [%expr [%e p] [%e v]] vs ps
    | _, _ ->
        failwith
          "from Printers.start, lists should be both empty or both not empty"
  in
  let x = gen_symbol ~prefix:"__x" () in
  [%expr
    fun [%p B.pvar x] ->
      let [%p pat] = [%e B.evar x] in
      PPrint.(lparen ^^ [%e start vars printers] ^^ rparen)]

let lsymbol2printer drv (ls : Tterm.lsymbol) =
  match ls.ls_value with
  | Some ty -> ty2printer drv ty
  | None -> failwith "can't find type to build printer"

let mk_field drv (ld : Tterm.lsymbol Tast.label_declaration) =
  let field = ld.ld_field.ls_name.id_str in
  let printer = lsymbol2printer drv ld.ld_field in
  [%expr [%e B.estring field], [%e printer] [%e B.evar field]]

let record_printer drv (rec_decl : Tast.rec_declaration) =
  let fields =
    List.map
      (fun (ld : Tterm.lsymbol Tast.label_declaration) ->
        String.concat "." [ "R"; ld.ld_field.ls_name.id_str ])
      rec_decl.rd_ldl
  in
  let prec =
    A.ppat_record ~loc
      (List.map (fun x -> (B.lident x, B.pvar x)) fields)
      Ppxlib__.Import.Closed
  in
  let fields_printer = List.map (mk_field drv) rec_decl.rd_ldl |> B.elist in
  [%expr fun [%p prec] -> PPrintOCaml.record "" [%e fields_printer]]

let ty2repr drv x (ty : Ttypes.ty) =
  let printer = ty2printer drv ty in
  B.eapply printer [ B.evar x ]

let variant_printer drv (constructors : Tast.constructor_decl list) =
  let variant (cd : Tast.constructor_decl) =
    let ty2repr = ty2repr drv in
    let cname = cd.cd_cs.ls_name.id_str in
    let cargs = cd.cd_cs.ls_args in
    let xs =
      List.init (List.length cargs) (fun _ -> gen_symbol ~prefix:"__x" ())
    in
    let parg = A.ppat_tuple_opt ~loc (List.map B.pvar xs) in
    let cident = String.concat "." [ "R"; cname ] |> B.lident in
    let lhs = A.ppat_construct ~loc cident parg in
    let args = List.map2 ty2repr xs cargs |> B.elist in
    let rhs = [%expr PPrintOCaml.variant "" [%e B.estring cname] 0 [%e args]] in
    A.case ~guard:None ~lhs ~rhs
  in
  let cases = List.map variant constructors in
  A.pexp_function ~loc cases

let printer_expr drv (ty_kind : Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> None
  | Pty_variant constructors -> Some (variant_printer drv constructors)
  | Pty_record rec_decl -> Some (record_printer drv rec_decl)
  | Pty_open -> None

let printer_definition drv (type_decl : Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  match printer_expr drv type_decl.td_kind with
  | None -> None
  | Some printer -> Some [%stri let [%p id] = [%e printer]]

let printer_option drv (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_type (_, [ type_decl ], _) -> printer_definition drv type_decl
  | _ -> None

let printers drv s =
  let name = B.noloc (Some "P") in
  let printers = List.filter_map (printer_option drv) s in
  let expr = A.pmod_structure ~loc printers in
  A.pstr_module ~loc (A.module_binding ~loc ~name ~expr)
