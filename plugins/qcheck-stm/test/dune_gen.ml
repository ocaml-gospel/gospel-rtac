(* Generates the boiler-plater dune configuration for tests

     dune_gen.exe array array_config ...
   will generate the dune rules for the array module with the array_config
   configuration. *)

let usage () =
  Printf.fprintf stderr "Usage: %s [MODULE CONFIGURATION] ..." Sys.argv.(0);
  exit 1

let rec print_rules pos =
  if pos < Array.length Sys.argv then (
    let m = Sys.argv.(pos) and config = Sys.argv.(pos + 1) in
    Printf.printf
      {|(library
 (name %s)
 (modules %s))

(rule
 (target %s.gospel)
 (action
  (run
   gospel
   check
   %%{dep:%s.mli})))

(rule
 (target %s_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    %s_errors
    (run
     ortac
     qcheck-stm
     %%{dep:%s.gospel}
     %%{dep:%s.ml}
     -o
     %%{target})))))

(test
 (name %s_stm_tests)
 (package ortac-qcheck-stm)
 (modules %s_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  %s)
 (action
  (echo
   "\n%%{dep:%s_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff %s_errors.expected %s_errors)
   (diff %s_stm_tests.expected.ml %s_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %%{dep:%s_stm_tests.exe} -v)))

|}
      m m m m m m m config m m m m m m m m m;
    print_rules (pos + 2))

let () =
  let nb_args = Array.length Sys.argv in
  if nb_args < 2 || nb_args mod 2 = 0 then usage () else print_rules 1
