type t = {
  translations : (Gospel.Tterm.lsymbol, string) Hashtbl.t;
  env : Gospel.Tmodule.namespace list;
}

let get_ls_env env path =
  List.find_map
    (fun ns ->
      try Some (Gospel.Tmodule.ns_find_ls ns path) with Not_found -> None)
    env
  |> function
  | Some ls -> ls
  | None ->
      Fmt.(
        failwith
          "Internal error: path `%a' was not found when initialising ortac \
           driver"
          (list ~sep:(any ".") string)
          path)

let v env =
  let table =
    [
      (* Built-in Gospel functions *)
      ([ "None" ], "None");
      ([ "Some" ], "Some");
      ([ "[]" ], "[]");
      ([ "infix ::" ], "(::)");
      ([ "infix =" ], "(=)");
      (* Arithmetic *)
      ([ "Gospelstdlib"; "succ" ], "Z.succ");
      ([ "Gospelstdlib"; "pred" ], "Z.pred");
      ([ "Gospelstdlib"; "prefix -" ], "Z.neg");
      ([ "Gospelstdlib"; "infix +" ], "Z.add");
      ([ "Gospelstdlib"; "infix -" ], "Z.sub");
      ([ "Gospelstdlib"; "infix *" ], "Z.mul");
      ([ "Gospelstdlib"; "infix /" ], "Z.div");
      ([ "Gospelstdlib"; "mod" ], "Z.rem");
      ([ "Gospelstdlib"; "pow" ], "Z.pow");
      ([ "Gospelstdlib"; "abs" ], "Z.abs");
      ([ "Gospelstdlib"; "min" ], "Z.min");
      ([ "Gospelstdlib"; "max" ], "Z.max");
      ([ "Gospelstdlib"; "infix >" ], "Z.gt");
      ([ "Gospelstdlib"; "infix >=" ], "Z.geq");
      ([ "Gospelstdlib"; "infix <" ], "Z.lt");
      ([ "Gospelstdlib"; "infix <=" ], "Z.leq");
      ([ "Gospelstdlib"; "logand" ], "Z.logand");
      ([ "Gospelstdlib"; "logor" ], "Z.logor");
      ([ "Gospelstdlib"; "logxor" ], "Z.logxor");
      ([ "Gospelstdlib"; "lognot" ], "Z.lognot");
      ([ "Gospelstdlib"; "shift_left" ], "Z.shift_left");
      ([ "Gospelstdlib"; "shift_right" ], "Z.shift_right");
      ([ "Gospelstdlib"; "shift_right_trunc" ], "Z.shift_right_trunc");
      (* Machine integers *)
      ([ "Gospelstdlib"; "integer_of_int" ], "Z.of_int");
      ([ "Gospelstdlib"; "max_int" ], "Z.max_int");
      ([ "Gospelstdlib"; "min_int" ], "Z.max_int");
      (* Couples *)
      ([ "Gospelstdlib"; "fst" ], "fst");
      ([ "Gospelstdlib"; "snd" ], "snd");
      (* References *)
      ([ "Gospelstdlib"; "prefix !" ], "(!)");
      ([ "Gospelstdlib"; "ref" ], "ref");
      (* Lists *)
      ([ "Gospelstdlib"; "List"; "length" ], "List.length");
      ([ "Gospelstdlib"; "List"; "hd" ], "List.hd");
      ([ "Gospelstdlib"; "List"; "tl" ], "List.tl");
      ([ "Gospelstdlib"; "List"; "nth" ], "List.nth");
      ([ "Gospelstdlib"; "List"; "nth_opt" ], "List.nth_opt");
      ([ "Gospelstdlib"; "List"; "rev" ], "List.rev");
      ([ "Gospelstdlib"; "List"; "init" ], "List.init");
      ([ "Gospelstdlib"; "List"; "map" ], "List.map");
      ([ "Gospelstdlib"; "List"; "mapi" ], "List.mapi");
      ([ "Gospelstdlib"; "List"; "fold_left" ], "List.fold_left");
      ([ "Gospelstdlib"; "List"; "fold_right" ], "List.fold_right");
      ([ "Gospelstdlib"; "List"; "map2" ], "List.map2");
      ([ "Gospelstdlib"; "List"; "for_all" ], "List.for_all");
      ([ "Gospelstdlib"; "List"; "_exists" ], "List.exists");
      ([ "Gospelstdlib"; "List"; "for_all2" ], "List.for_all2");
      ([ "Gospelstdlib"; "List"; "_exists2" ], "List.exists2");
      (* Arrays *)
      ([ "Gospelstdlib"; "Array"; "length" ], "Array.length");
      ([ "Gospelstdlib"; "Array"; "get" ], "Array.get");
      ([ "Gospelstdlib"; "Array"; "make" ], "Array.make");
      ([ "Gospelstdlib"; "Array"; "init" ], "Array.init");
      ([ "Gospelstdlib"; "Array"; "append" ], "Array.append");
      ([ "Gospelstdlib"; "Array"; "concat" ], "Array.concat");
      ([ "Gospelstdlib"; "Array"; "sub" ], "Array.sub");
      ([ "Gospelstdlib"; "Array"; "map" ], "Array.map");
      ([ "Gospelstdlib"; "Array"; "mapi" ], "Array.mapi");
      ([ "Gospelstdlib"; "Array"; "fold_left" ], "Array.fold_left");
      ([ "Gospelstdlib"; "Array"; "fold_right" ], "Array.fold_right");
      ([ "Gospelstdlib"; "Array"; "map2" ], "Array.map2");
      ([ "Gospelstdlib"; "Array"; "for_all" ], "Array.for_all");
      ([ "Gospelstdlib"; "Array"; "_exists" ], "Array.exists");
      ([ "Gospelstdlib"; "Array"; "for_all2" ], "Array.for_all2");
      ([ "Gospelstdlib"; "Array"; "_exists2" ], "Array.exists2");
      ([ "Gospelstdlib"; "Array"; "to_list" ], "Array.to_list");
      ([ "Gospelstdlib"; "Array"; "of_list" ], "Array.of_list");
      (* System *)
      ([ "Gospelstdlib"; "Sys"; "word_size" ], "Sys.word_size");
      ([ "Gospelstdlib"; "Sys"; "int_size" ], "Sys.int_size");
      ([ "Gospelstdlib"; "Sys"; "big_endian" ], "Sys.big_endian");
      ([ "Gospelstdlib"; "Sys"; "max_string_length" ], "Sys.max_string_length");
      ([ "Gospelstdlib"; "Sys"; "max_array_length" ], "Sys.max_array_length");
    ]
  in
  let translations = Hashtbl.create 0 in
  List.iter
    (fun (path, ocaml) ->
      let ls = get_ls_env env path in
      Hashtbl.add translations ls ocaml)
    table;
  { translations; env }

let translate t ls = Hashtbl.find_opt t.translations ls

let get_ls t = get_ls_env t.env
