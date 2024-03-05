module Config = Config
module Ir = Ir
module Ir_of_gospel = Ir_of_gospel
module Reserr = Reserr
module Stm_of_ir = Stm_of_ir

let main path init sut include_ protect_call output quiet () =
  let open Reserr in
  let fmt = Registration.get_out_formatter output in
  let pp = pp quiet Ppxlib_ast.Pprintast.structure fmt in
  pp
    (let* sigs, config = Config.init path init sut in
     let* ir = Ir_of_gospel.run sigs config in
     let config = { config with include_; protect_call } in
     Stm_of_ir.stm config ir)

open Cmdliner

module Plugin : sig
  val cmd : unit Cmd.t
end = struct
  let info =
    Cmd.info "qcheck-stm"
      ~doc:"Generate QCheck-stm test file according to Gospel specifications."

  let sut =
    Arg.(
      required
      & pos 2 (some string) None
      & info [] ~doc:"Build the qcheck-stm tests with SUT." ~docv:"SUT")

  let init =
    Arg.(
      required
      & pos 1 (some string) None
      & info []
          ~doc:
            "Build the qcheck-stm tests using INIT function to initialize the \
             system under test."
          ~docv:"INIT")

  let protect_call =
    Arg.(
      value
      & opt (some string) None
      & info [ "p"; "protect-call" ] ~docv:"PROTECT_CALL"
          ~doc:
            "Protect the call of the QCheck tests with PROTECT_CALL. \
             PROTECT_CALL should be the name of a function.")

  let term =
    let open Registration in
    Term.(
      const main
      $ ocaml_file
      $ init
      $ sut
      $ include_
      $ protect_call
      $ output_file
      $ quiet
      $ setup_log)

  let cmd = Cmd.v info term
end

let () = Registration.register Plugin.cmd
