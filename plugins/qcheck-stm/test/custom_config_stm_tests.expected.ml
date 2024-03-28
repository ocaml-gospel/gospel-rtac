(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27"]
open Custom_config
module Ortac_runtime = Ortac_runtime_qcheck_stm
module Spec =
  struct
    open STM
    module QCheck =
      struct
        include QCheck
        module Gen =
          struct
            include Gen
            let int = small_signed_int
            let elt gen = elt <$> gen
          end
      end
    module Util =
      struct
        module Pp =
          struct
            include Util.Pp
            let pp_elt pp par fmt e =
              let open Format in fprintf fmt "(Elt %a)" (pp par) (proj e)
          end
      end
    type _ ty +=  
      | Elt: 'a ty -> 'a elt ty 
    let elt spec =
      let (ty, show) = spec in
      ((Elt ty), (fun x -> Printf.sprintf "Elt %s" (show (proj x))))
    type sut = int t
    type cmd =
      | Push of int elt 
      | Top 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Push e ->
          Format.asprintf "%s sut %a" "push"
            (Util.Pp.pp_elt Util.Pp.pp_int true) e
      | Top -> Format.asprintf "protect (fun () -> %s sut)" "top"
    type nonrec state = {
      contents: int Ortac_runtime.Gospelstdlib.sequence }
    let init_state =
      let () = () in
      {
        contents =
          (try Ortac_runtime.Gospelstdlib.Sequence.empty
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "custom_config.mli";
                            pos_lnum = 12;
                            pos_bol = 421;
                            pos_cnum = 446
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "custom_config.mli";
                            pos_lnum = 12;
                            pos_bol = 421;
                            pos_cnum = 460
                          }
                      })))
      }
    let init_sut () = empty ()
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof [(pure (fun e -> Push e)) <*> (elt int); pure Top])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Push e ->
          {
            contents =
              ((try
                  Ortac_runtime.Gospelstdlib.Sequence.cons (proj e)
                    state__003_.contents
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "custom_config.mli";
                                 pos_lnum = 17;
                                 pos_bol = 639;
                                 pos_cnum = 664
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "custom_config.mli";
                                 pos_lnum = 17;
                                 pos_bol = 639;
                                 pos_cnum = 703
                               }
                           }))))
          }
      | Top -> state__003_
    let precond cmd__008_ state__009_ =
      match cmd__008_ with | Push e -> true | Top -> true
    let postcond _ _ _ = true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with
      | Push e -> Res (unit, (push sut__011_ e))
      | Top ->
          Res
            ((result (elt int) exn), (protect (fun () -> top sut__011_) ()))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_postcond cmd__004_ state__005_ res__006_ =
  let open Spec in
    let open STM in
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Push e, Res ((Unit, _), _)) -> None
      | (Top, Res ((Result (Elt (Int), Exn), _), a_1)) ->
          (match if
                   try
                     not
                       (state__005_.contents =
                          Ortac_runtime.Gospelstdlib.Sequence.empty)
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "custom_config.mli";
                                    pos_lnum = 21;
                                    pos_bol = 875;
                                    pos_cnum = 886
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "custom_config.mli";
                                    pos_lnum = 21;
                                    pos_bol = 875;
                                    pos_cnum = 914
                                  }
                              }))
                 then None
                 else
                   Some
                     (Ortac_runtime.report "Custom_config" "empty ()"
                        (Either.left "Invalid_argument") "top"
                        [("t.contents <> Sequence.empty",
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "custom_config.mli";
                                 pos_lnum = 21;
                                 pos_bol = 875;
                                 pos_cnum = 886
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "custom_config.mli";
                                 pos_lnum = 21;
                                 pos_bol = 875;
                                 pos_cnum = 914
                               }
                           })])
           with
           | None ->
               (match a_1 with
                | Ok a_1 ->
                    if
                      (try
                         (proj a_1) =
                           (Ortac_runtime.Gospelstdlib.Sequence.hd
                              (Lazy.force new_state__007_).contents)
                       with
                       | e ->
                           raise
                             (Ortac_runtime.Partial_function
                                (e,
                                  {
                                    Ortac_runtime.start =
                                      {
                                        pos_fname = "custom_config.mli";
                                        pos_lnum = 22;
                                        pos_bol = 915;
                                        pos_cnum = 927
                                      };
                                    Ortac_runtime.stop =
                                      {
                                        pos_fname = "custom_config.mli";
                                        pos_lnum = 22;
                                        pos_bol = 915;
                                        pos_cnum = 958
                                      }
                                  })))
                    then None
                    else
                      Some
                        (Ortac_runtime.report "Custom_config" "empty ()"
                           (Either.right (Res (Ortac_runtime.dummy, ())))
                           "top"
                           [("proj a = Sequence.hd t.contents",
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "custom_config.mli";
                                    pos_lnum = 22;
                                    pos_bol = 915;
                                    pos_cnum = 927
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "custom_config.mli";
                                    pos_lnum = 22;
                                    pos_bol = 915;
                                    pos_cnum = 958
                                  }
                              })])
                | _ -> None)
           | _ ->
               (match a_1 with
                | Error (Invalid_argument _) -> None
                | _ ->
                    if
                      (try
                         not
                           (state__005_.contents =
                              Ortac_runtime.Gospelstdlib.Sequence.empty)
                       with
                       | e ->
                           raise
                             (Ortac_runtime.Partial_function
                                (e,
                                  {
                                    Ortac_runtime.start =
                                      {
                                        pos_fname = "custom_config.mli";
                                        pos_lnum = 21;
                                        pos_bol = 875;
                                        pos_cnum = 886
                                      };
                                    Ortac_runtime.stop =
                                      {
                                        pos_fname = "custom_config.mli";
                                        pos_lnum = 21;
                                        pos_bol = 875;
                                        pos_cnum = 914
                                      }
                                  })))
                    then None
                    else
                      Some
                        (Ortac_runtime.report "Custom_config" "empty ()"
                           (Either.left "Invalid_argument") "top"
                           [("t.contents <> Sequence.empty",
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "custom_config.mli";
                                    pos_lnum = 21;
                                    pos_bol = 875;
                                    pos_cnum = 886
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "custom_config.mli";
                                    pos_lnum = 21;
                                    pos_bol = 875;
                                    pos_cnum = 914
                                  }
                              })])))
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Custom_config STM tests"
        check_init_state ortac_postcond])
