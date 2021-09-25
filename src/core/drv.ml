open Gospel
module H = Hashtbl.Make (Tterm.LS)

type t = { translations : string H.t; env : Tmodule.namespace list }

let get_env get env path =
  List.find_map (fun ns -> try Some (get ns path) with Not_found -> None) env
  |> function
  | Some s -> s
  | None ->
      Fmt.(
        failwith "Internal error: path `%a' was not found"
          (list ~sep:(any ".") string)
          path)

let get_ls_env = get_env Tmodule.ns_find_ls

let v env =
  let table =
    [
      ([ "None" ], "None");
      ([ "Some" ], "Some");
      ([ "[]" ], "[]");
      ([ "infix ::" ], "(::)");
      ([ "infix =" ], "(=)");
      ([ "Gospelstdlib"; "infix +" ], "Z.add");
      ([ "Gospelstdlib"; "infix -" ], "Z.sub");
      ([ "Gospelstdlib"; "infix *" ], "Z.mul");
      ([ "Gospelstdlib"; "infix /" ], "Z.div");
      ([ "Gospelstdlib"; "mod" ], "Z.rem");
      ([ "Gospelstdlib"; "pow" ], "Z.pow");
      ([ "Gospelstdlib"; "logand" ], "Z.logand");
      ([ "Gospelstdlib"; "prefix -" ], "Z.neg");
      ([ "Gospelstdlib"; "infix >" ], "Z.gt");
      ([ "Gospelstdlib"; "infix >=" ], "Z.geq");
      ([ "Gospelstdlib"; "infix <" ], "Z.lt");
      ([ "Gospelstdlib"; "infix <=" ], "Z.leq");
      ([ "Gospelstdlib"; "integer_of_int" ], "Z.of_int");
      ([ "Gospelstdlib"; "abs" ], "Z.abs");
      ([ "Gospelstdlib"; "min" ], "Z.min");
      ([ "Gospelstdlib"; "max" ], "Z.max");
      ([ "Gospelstdlib"; "succ" ], "Z.succ");
      ([ "Gospelstdlib"; "pred" ], "Z.pred");
      ([ "Gospelstdlib"; "Array"; "length" ], "Array.length");
      ([ "Gospelstdlib"; "Array"; "get" ], "Array.get");
      ([ "Gospelstdlib"; "Array"; "for_all" ], "Array.for_all");
    ]
  in
  let translations = H.create 0 in
  List.iter
    (fun (path, ocaml) ->
      let ls = get_ls_env env path in
      H.add translations ls ocaml)
    table;
  { translations; env }

let translate t ls = H.find_opt t.translations ls

let add_translation t ls s = H.replace t.translations ls s

let remove_translation t ls = H.remove t.translations ls

let get_ls t = get_ls_env t.env

let get_ts t = get_env Tmodule.ns_find_ts t.env
