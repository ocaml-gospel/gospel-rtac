module Config = Config
module Ir = Ir
module Ir_of_gospel = Ir_of_gospel
module Reserr = Reserr
module Stm_of_ir = Stm_of_ir

let main path config output quiet () =
  let open Reserr in
  let fmt = Registration.get_out_formatter output in
  let pp = pp quiet Ppxlib_ast.Pprintast.structure fmt in
  pp
    (let* sigs, config = Config.init path config in
     let* ir = Ir_of_gospel.run sigs config in
     Stm_of_ir.stm config ir)

open Cmdliner

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let info =
    Cmd.info "qcheck-stm"
      ~doc:"Generate QCheck-stm test file according to Gospel specifications."

  let config =
    Arg.(
      required
      & pos 1 (some string) None
      & info []
          ~doc:
            "Build the qcheck-stm tests using CONFIG module as configuration \
             file."
          ~docv:"CONFIG")

  let term =
    let open Registration in
    Term.(const main $ input_file $ config $ output_file $ quiet $ setup_log)

  let cmd = Cmd.v info term
end

let () = Registration.register Plugin.cmd
