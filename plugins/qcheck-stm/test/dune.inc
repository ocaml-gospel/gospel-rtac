(library
 (name array)
 (modules array))

(rule
 (target array.gospel)
 (action
  (run
   gospel
   check
   %{dep:array.mli})))

(rule
 (target array_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    array_errors
    (run
     ortac
     qcheck-stm
     %{dep:array.gospel}
     %{dep:array_config.ml}
     -o
     %{target})))))

(test
 (name array_stm_tests)
 (package ortac-qcheck-stm)
 (modules array_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  array)
 (action
  (echo
   "\n%{dep:array_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff array_errors.expected array_errors)
   (diff array_stm_tests.expected.ml array_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:array_stm_tests.exe} -v)))

(library
 (name hashtbl)
 (modules hashtbl))

(rule
 (target hashtbl.gospel)
 (action
  (run
   gospel
   check
   %{dep:hashtbl.mli})))

(rule
 (target hashtbl_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    hashtbl_errors
    (run
     ortac
     qcheck-stm
     %{dep:hashtbl.gospel}
     %{dep:hashtbl_config.ml}
     -o
     %{target})))))

(test
 (name hashtbl_stm_tests)
 (package ortac-qcheck-stm)
 (modules hashtbl_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  hashtbl)
 (action
  (echo
   "\n%{dep:hashtbl_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff hashtbl_errors.expected hashtbl_errors)
   (diff hashtbl_stm_tests.expected.ml hashtbl_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:hashtbl_stm_tests.exe} -v)))

(library
 (name record)
 (modules record))

(rule
 (target record.gospel)
 (action
  (run
   gospel
   check
   %{dep:record.mli})))

(rule
 (target record_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    record_errors
    (run
     ortac
     qcheck-stm
     %{dep:record.gospel}
     %{dep:record_config.ml}
     -o
     %{target})))))

(test
 (name record_stm_tests)
 (package ortac-qcheck-stm)
 (modules record_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  record)
 (action
  (echo
   "\n%{dep:record_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff record_errors.expected record_errors)
   (diff record_stm_tests.expected.ml record_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:record_stm_tests.exe} -v)))

(library
 (name ref)
 (modules ref))

(rule
 (target ref.gospel)
 (action
  (run
   gospel
   check
   %{dep:ref.mli})))

(rule
 (target ref_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    ref_errors
    (run
     ortac
     qcheck-stm
     %{dep:ref.gospel}
     %{dep:ref_config.ml}
     -o
     %{target})))))

(test
 (name ref_stm_tests)
 (package ortac-qcheck-stm)
 (modules ref_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  ref)
 (action
  (echo
   "\n%{dep:ref_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff ref_errors.expected ref_errors)
   (diff ref_stm_tests.expected.ml ref_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:ref_stm_tests.exe} -v)))

(library
 (name conjunctive_clauses)
 (modules conjunctive_clauses))

(rule
 (target conjunctive_clauses.gospel)
 (action
  (run
   gospel
   check
   %{dep:conjunctive_clauses.mli})))

(rule
 (target conjunctive_clauses_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    conjunctive_clauses_errors
    (run
     ortac
     qcheck-stm
     %{dep:conjunctive_clauses.gospel}
     %{dep:conjunctive_clauses_config.ml}
     -o
     %{target})))))

(test
 (name conjunctive_clauses_stm_tests)
 (package ortac-qcheck-stm)
 (modules conjunctive_clauses_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  conjunctive_clauses)
 (action
  (echo
   "\n%{dep:conjunctive_clauses_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff conjunctive_clauses_errors.expected conjunctive_clauses_errors)
   (diff conjunctive_clauses_stm_tests.expected.ml conjunctive_clauses_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:conjunctive_clauses_stm_tests.exe} -v)))

(library
 (name sequence_model)
 (modules sequence_model))

(rule
 (target sequence_model.gospel)
 (action
  (run
   gospel
   check
   %{dep:sequence_model.mli})))

(rule
 (target sequence_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    sequence_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:sequence_model.gospel}
     %{dep:sequence_model_config.ml}
     -o
     %{target})))))

(test
 (name sequence_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules sequence_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  sequence_model)
 (action
  (echo
   "\n%{dep:sequence_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff sequence_model_errors.expected sequence_model_errors)
   (diff sequence_model_stm_tests.expected.ml sequence_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:sequence_model_stm_tests.exe} -v)))

(library
 (name invariants)
 (modules invariants))

(rule
 (target invariants.gospel)
 (action
  (run
   gospel
   check
   %{dep:invariants.mli})))

(rule
 (target invariants_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    invariants_errors
    (run
     ortac
     qcheck-stm
     %{dep:invariants.gospel}
     %{dep:invariants_config.ml}
     -o
     %{target})))))

(test
 (name invariants_stm_tests)
 (package ortac-qcheck-stm)
 (modules invariants_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  invariants)
 (action
  (echo
   "\n%{dep:invariants_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff invariants_errors.expected invariants_errors)
   (diff invariants_stm_tests.expected.ml invariants_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:invariants_stm_tests.exe} -v)))

(library
 (name integer_in_model)
 (modules integer_in_model))

(rule
 (target integer_in_model.gospel)
 (action
  (run
   gospel
   check
   %{dep:integer_in_model.mli})))

(rule
 (target integer_in_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    integer_in_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:integer_in_model.gospel}
     %{dep:integer_in_model_config.ml}
     -o
     %{target})))))

(test
 (name integer_in_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules integer_in_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  integer_in_model)
 (action
  (echo
   "\n%{dep:integer_in_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff integer_in_model_errors.expected integer_in_model_errors)
   (diff integer_in_model_stm_tests.expected.ml integer_in_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:integer_in_model_stm_tests.exe} -v)))

(library
 (name ghost_as_model)
 (modules ghost_as_model))

(rule
 (target ghost_as_model.gospel)
 (action
  (run
   gospel
   check
   %{dep:ghost_as_model.mli})))

(rule
 (target ghost_as_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    ghost_as_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:ghost_as_model.gospel}
     %{dep:ghost_as_model_config.ml}
     -o
     %{target})))))

(test
 (name ghost_as_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules ghost_as_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  ghost_as_model)
 (action
  (echo
   "\n%{dep:ghost_as_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff ghost_as_model_errors.expected ghost_as_model_errors)
   (diff ghost_as_model_stm_tests.expected.ml ghost_as_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:ghost_as_model_stm_tests.exe} -v)))

(library
 (name custom_config)
 (modules custom_config))

(rule
 (target custom_config.gospel)
 (action
  (run
   gospel
   check
   %{dep:custom_config.mli})))

(rule
 (target custom_config_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    custom_config_errors
    (run
     ortac
     qcheck-stm
     %{dep:custom_config.gospel}
     %{dep:custom_config_config.ml}
     -o
     %{target})))))

(test
 (name custom_config_stm_tests)
 (package ortac-qcheck-stm)
 (modules custom_config_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  custom_config)
 (action
  (echo
   "\n%{dep:custom_config_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff custom_config_errors.expected custom_config_errors)
   (diff custom_config_stm_tests.expected.ml custom_config_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:custom_config_stm_tests.exe} -v)))

(library
 (name test_without_sut)
 (modules test_without_sut))

(rule
 (target test_without_sut.gospel)
 (action
  (run
   gospel
   check
   %{dep:test_without_sut.mli})))

(rule
 (target test_without_sut_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    test_without_sut_errors
    (run
     ortac
     qcheck-stm
     %{dep:test_without_sut.gospel}
     %{dep:test_without_sut_config.ml}
     -o
     %{target})))))

(test
 (name test_without_sut_stm_tests)
 (package ortac-qcheck-stm)
 (modules test_without_sut_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  test_without_sut)
 (action
  (echo
   "\n%{dep:test_without_sut_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff test_without_sut_errors.expected test_without_sut_errors)
   (diff test_without_sut_stm_tests.expected.ml test_without_sut_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:test_without_sut_stm_tests.exe} -v)))

(library
 (name tuples)
 (modules tuples))

(rule
 (target tuples.gospel)
 (action
  (run
   gospel
   check
   %{dep:tuples.mli})))

(rule
 (target tuples_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    tuples_errors
    (run
     ortac
     qcheck-stm
     %{dep:tuples.gospel}
     %{dep:tuples_config.ml}
     -o
     %{target})))))

(test
 (name tuples_stm_tests)
 (package ortac-qcheck-stm)
 (modules tuples_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  tuples)
 (action
  (echo
   "\n%{dep:tuples_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff tuples_errors.expected tuples_errors)
   (diff tuples_stm_tests.expected.ml tuples_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:tuples_stm_tests.exe} -v)))

(library
 (name functional_model)
 (modules functional_model))

(rule
 (target functional_model.gospel)
 (action
  (run
   gospel
   check
   %{dep:functional_model.mli})))

(rule
 (target functional_model_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    functional_model_errors
    (run
     ortac
     qcheck-stm
     %{dep:functional_model.gospel}
     %{dep:functional_model_config.ml}
     -o
     %{target})))))

(test
 (name functional_model_stm_tests)
 (package ortac-qcheck-stm)
 (modules functional_model_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  functional_model)
 (action
  (echo
   "\n%{dep:functional_model_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff functional_model_errors.expected functional_model_errors)
   (diff functional_model_stm_tests.expected.ml functional_model_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:functional_model_stm_tests.exe} -v)))

(library
 (name test_cleanup)
 (modules test_cleanup))

(rule
 (target test_cleanup.gospel)
 (action
  (run
   gospel
   check
   %{dep:test_cleanup.mli})))

(rule
 (target test_cleanup_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    test_cleanup_errors
    (run
     ortac
     qcheck-stm
     %{dep:test_cleanup.gospel}
     %{dep:test_cleanup_config.ml}
     -o
     %{target})))))

(test
 (name test_cleanup_stm_tests)
 (package ortac-qcheck-stm)
 (modules test_cleanup_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  test_cleanup)
 (action
  (echo
   "\n%{dep:test_cleanup_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff test_cleanup_errors.expected test_cleanup_errors)
   (diff test_cleanup_stm_tests.expected.ml test_cleanup_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:test_cleanup_stm_tests.exe} -v)))

(library
 (name sut_in_type)
 (modules sut_in_type))

(rule
 (target sut_in_type_stm_tests.ml)
 (package ortac-qcheck-stm)
 (deps
  (package ortac-core)
  (package ortac-qcheck-stm))
 (action
  (setenv
   ORTAC_ONLY_PLUGIN
   qcheck-stm
   (with-stderr-to
    sut_in_type_errors
    (run
     ortac
     qcheck-stm
     %{dep:sut_in_type.mli}
     %{dep:sut_in_type_config.ml}
     -o
     %{target})))))

(test
 (name sut_in_type_stm_tests)
 (package ortac-qcheck-stm)
 (modules sut_in_type_stm_tests)
 (libraries
  qcheck-core
  qcheck-core.runner
  qcheck-stm.stm
  qcheck-stm.sequential
  qcheck-multicoretests-util
  ortac-runtime-qcheck-stm
  sut_in_type)
 (action
  (echo
   "\n%{dep:sut_in_type_stm_tests.exe} has been generated with the ortac-qcheck-stm plugin.\n")))

(rule
 (alias runtest)
 (package ortac-qcheck-stm)
 (action
  (progn
   (diff sut_in_type_errors.expected sut_in_type_errors)
   (diff sut_in_type_stm_tests.expected.ml sut_in_type_stm_tests.ml))))

(rule
 (alias launchtests)
 (action
  (run %{dep:sut_in_type_stm_tests.exe} -v)))

