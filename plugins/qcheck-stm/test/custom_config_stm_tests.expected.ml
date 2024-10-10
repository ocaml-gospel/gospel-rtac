(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32-38"]
open Custom_config
module Ortac_runtime = Ortac_runtime_qcheck_stm
module SUT =
  (Ortac_runtime.SUT.Make)(struct type sut = int t
                                  let init () = empty () end)
module ModelElt =
  struct
    type nonrec elt = {
      contents: int Ortac_runtime.Gospelstdlib.sequence }
    let init =
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
  end
module Model = (Ortac_runtime.Model.Make)(ModelElt)
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
    type _ ty +=  
      | Integer: Ortac_runtime.integer ty 
    let integer = (Integer, Ortac_runtime.string_of_integer)
    type _ ty +=  
      | SUT: SUT.elt ty 
    let sut = (SUT, (fun _ -> "<sut>"))
    type sut = SUT.t
    let init_sut = SUT.create 1
    type state = Model.t
    let init_state = Model.create 1 ()
    type cmd =
      | Proj of char elt 
      | Empty of unit 
      | Push of int elt 
      | Top 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Proj __arg0 ->
          Format.asprintf "%s %a" "proj"
            (Util.Pp.pp_elt Util.Pp.pp_char true) __arg0
      | Empty () -> Format.asprintf "%s %a" "empty" (Util.Pp.pp_unit true) ()
      | Push e ->
          Format.asprintf "%s <sut> %a" "push"
            (Util.Pp.pp_elt Util.Pp.pp_int true) e
      | Top -> Format.asprintf "protect (fun () -> %s <sut>)" "top"
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [(pure (fun __arg0 -> Proj __arg0)) <*> (elt char);
               (pure (fun () -> Empty ())) <*> unit;
               (pure (fun e -> Push e)) <*> (elt int);
               pure Top])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Proj __arg0 -> state__003_
      | Empty () ->
          let t_1__005_ =
            let open ModelElt in
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
              } in
          Model.push (Model.drop_n state__003_ 0) t_1__005_
      | Push e ->
          let t_2__006_ = Model.get state__003_ 0 in
          let t_2__007_ =
            let open ModelElt in
              {
                contents =
                  (try
                     Ortac_runtime.Gospelstdlib.Sequence.cons (proj e)
                       t_2__006_.contents
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
                              })))
              } in
          Model.push (Model.drop_n state__003_ 1) t_2__007_
      | Top ->
          let t_3__008_ = Model.get state__003_ 0 in
          if
            (try
               not
                 (t_3__008_.contents =
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
          then
            let t_3__009_ = t_3__008_ in
            Model.push (Model.drop_n state__003_ 1) t_3__009_
          else state__003_
    let precond cmd__018_ state__019_ =
      match cmd__018_ with
      | Proj __arg0 -> true
      | Empty () -> true
      | Push e -> true
      | Top -> true
    let postcond _ _ _ = true
    let run cmd__020_ sut__021_ =
      match cmd__020_ with
      | Proj __arg0 -> Res (char, (let res__022_ = proj __arg0 in res__022_))
      | Empty () ->
          Res
            (sut,
              (let res__023_ = empty () in
               (SUT.push sut__021_ res__023_; res__023_)))
      | Push e ->
          Res
            (unit,
              (let t_2__024_ = SUT.pop sut__021_ in
               let res__025_ = push t_2__024_ e in
               (SUT.push sut__021_ t_2__024_; res__025_)))
      | Top ->
          Res
            ((result (elt int) exn),
              (let t_3__026_ = SUT.pop sut__021_ in
               let res__027_ = protect (fun () -> top t_3__026_) () in
               (SUT.push sut__021_ t_3__026_; res__027_)))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_show_cmd cmd__029_ state__030_ last__032_ res__031_ =
  let open Spec in
    let open STM in
      match (cmd__029_, res__031_) with
      | (Proj __arg0, Res ((Char, _), _)) ->
          let lhs = if last__032_ then "r" else "_"
          and shift = 0 in
          Format.asprintf "let %s = %s %a" lhs "proj"
            (Util.Pp.pp_elt Util.Pp.pp_char true) __arg0
      | (Empty (), Res ((SUT, _), t_1)) ->
          let lhs = if last__032_ then "r" else SUT.get_name state__030_ 0
          and shift = 1 in
          Format.asprintf "let %s = %s %a" lhs "empty" (Util.Pp.pp_unit true)
            ()
      | (Push e, Res ((Unit, _), _)) ->
          let lhs = if last__032_ then "r" else "_"
          and shift = 0 in
          Format.asprintf "let %s = %s %s %a" lhs "push"
            (SUT.get_name state__030_ (0 + shift))
            (Util.Pp.pp_elt Util.Pp.pp_int true) e
      | (Top, Res ((Result (Elt (Int), Exn), _), _)) ->
          let lhs = if last__032_ then "r" else "_"
          and shift = 0 in
          Format.asprintf "let %s = protect (fun () -> %s %s)" lhs "top"
            (SUT.get_name state__030_ (0 + shift))
      | _ -> assert false
let ortac_postcond cmd__010_ state__011_ res__012_ =
  let open Spec in
    let open STM in
      let new_state__013_ = lazy (next_state cmd__010_ state__011_) in
      match (cmd__010_, res__012_) with
      | (Proj __arg0, Res ((Char, _), result)) -> None
      | (Empty (), Res ((SUT, _), t_1)) -> None
      | (Push e, Res ((Unit, _), _)) -> None
      | (Top, Res ((Result (Elt (Int), Exn), _), a_1)) ->
          (match if
                   let tmp__017_ = Model.get state__011_ 0 in
                   try
                     not
                       (tmp__017_.contents =
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
                        (Ortac_runtime.Exception "Invalid_argument") "top"
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
                      let t_old__015_ = Model.get state__011_ 0
                      and t_new__016_ =
                        lazy (Model.get (Lazy.force new_state__013_) 0) in
                      (try
                         (proj a_1) =
                           (Ortac_runtime.Gospelstdlib.Sequence.hd
                              (Lazy.force t_new__016_).contents)
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
                           (Ortac_runtime.Protected_value
                              (Res (Ortac_runtime.dummy, ()))) "top"
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
                      let tmp__017_ = Model.get state__011_ 0 in
                      (try
                         not
                           (tmp__017_.contents =
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
                           (Ortac_runtime.Exception "Invalid_argument") "top"
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
     [STMTests.agree_test ~count ~name:"Custom_config STM tests" 1
        check_init_state ortac_show_cmd ortac_postcond])
