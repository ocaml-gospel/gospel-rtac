open Cmdliner
open Registration

(* reexpose Generate module as it is used by the monolith plugin for example *)
module Generate = Generate

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let main input output () =
    let fmt = get_out_formatter output in
    try Generate.generate input fmt
    with Gospel.Warnings.Error e ->
      Fmt.epr "%a@." Gospel.Warnings.pp e;
      exit 1

  let info =
    Cmd.info "wrapper"
      ~doc:
        "Wrap module functions with assertions to check their specifications."

  let term = Term.(const main $ input_file $ output_file $ setup_log)
  let cmd = Cmd.v info term
end

let () = register Plugin.cmd
