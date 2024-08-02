type plugins = unit Cmdliner.Cmd.t Queue.t
type input_file = MLI of string | GOSPEL of string

let unwrap = function MLI s | GOSPEL s -> s
let pp_input_file ppf x = Fmt.(pf ppf "%s" (unwrap x))
let plugins = Queue.create ()
let register cmd = Queue.add cmd plugins
let fold = Queue.fold
let get_channel = function None -> stdout | Some path -> open_out path
let get_out_formatter s = get_channel s |> Format.formatter_of_out_channel

open Cmdliner

let setup_log =
  let init style_renderer = Fmt_tty.setup_std_outputs ?style_renderer () in
  Term.(const init $ Fmt_cli.style_renderer ())

let include_ =
  Arg.(
    value
    & opt (some string) None
    & info [ "i"; "include" ] ~docv:"MODULE"
        ~doc:"Include MODULE in the generated code.")

let output_file =
  let parse s =
    match Sys.is_directory s with
    | true -> Error (`Msg (Fmt.str "Error: `%s' is a directory" s))
    | false | (exception Sys_error _) -> Ok (Some s)
  in
  Arg.(
    value
    & opt (conv ~docv:"OUTPUT" (parse, Fmt.(option string))) None
    & info [ "o"; "output" ] ~absent:"stdout" ~docv:"OUTPUT"
        ~doc:
          "Print the generated code in OUTPUT. Overwrite the file if it exists.")

let quiet =
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc:"Don't print any warnings.")

let input_file =
  let parse s =
    if not (Sys.file_exists s) then `Error (Fmt.str "Error: `%s' not found" s)
    else
      match Filename.extension s with
      | ".mli" -> `Ok (MLI s)
      | ".gospel" -> `Ok (GOSPEL s)
      | _ -> `Error "Error, expecting .mli or .gospel file."
  in
  Arg.(
    required
    & pos 0 (some (parse, pp_input_file)) None
    & info [] ~docv:"FILE"
        ~doc:
          "Read Gospel specifications in FILE. FILE can be an OCaml interface \
           or a .gospel file")
