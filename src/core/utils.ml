open Gospel

let term_printer text global_loc (t : Tterm.term) =
  let open Ppxlib.Location in
  try
    String.sub text
      (t.t_loc.loc_start.pos_cnum - global_loc.loc_start.pos_cnum)
      (t.t_loc.loc_end.pos_cnum - t.t_loc.loc_start.pos_cnum)
  with Invalid_argument _ -> Fmt.str "%a" Tterm_printer.print_term t

let module_name_of_path p =
  let filename = Filename.basename p in
  String.index filename '.' |> String.sub filename 0 |> String.capitalize_ascii

open Gospel

type checked = {
  module_name : string;
  namespace : Tmodule.namespace;
  ast : Tast.signature;
}

let type_check load_path name =
  let sigs = Parser_frontend.parse_ocaml_gospel name
  and md = Tmodule.init_muc name
  and penv =
    module_name_of_path name |> Utils.Sstr.singleton |> Typing.penv load_path
  in
  let gfile = List.fold_left (Typing.type_sig_item penv) md sigs in
  let sigs = Tmodule.wrap_up_muc gfile |> fun file -> file.fl_sigs in
  (gfile.muc_import, sigs)

let read_gospel_file filename =
  let open Tmodule in
  let gfile : module_uc = read_gospel_file filename in
  let sigs = Tmodule.wrap_up_muc gfile |> fun file -> file.fl_sigs in
  (gfile.muc_import, sigs)

let check filename =
  let open Registration in
  let module_name = module_name_of_path (unwrap filename)
  and env, ast =
    match filename with
    | MLI filename -> type_check [] filename
    | GOSPEL filename -> read_gospel_file filename
  in
  { module_name; namespace = List.hd env; ast }
