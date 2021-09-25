open Terms__Lib_rtac
open Common

let bool_ops () =
  check_success "lazy bool no exception" (fun () -> lazy_bool 42 |> ignore);
  check_raises_ortac "not lazy bool or" (fun () -> not_lazy_or 42 |> ignore);
  check_raises_ortac "not lazy bool and" (fun () -> not_lazy_and 42 |> ignore)

let scopes () =
  check_success "override variable in let binding" (fun () ->
      scope1 42 |> ignore)

let logic () =
  check_success "forall in if" (fun () -> if_forall 3 |> ignore);
  check_success "equivalence" equiv;
  check_success "exists" exists_

let patterns () =
  check_success "valid match" (fun () -> a A);
  check_success "valid match with argument" (fun () -> b (B "hello"));
  check_raises_ortac "invalid match" (fun () -> a (B "hello"));
  check_raises_ortac "invalid match with arguments" (fun () -> b A)

let peano () =
  check_success "succ" (fun () -> succ O |> ignore);
  check_success "add" (fun () -> add O (S O) |> ignore);
  check_raises_ortac "bad_add" (fun () -> bad_add O (S O) |> ignore)

let trees () =
  let t = check_success "make_tree" (fun () -> make_tree E 42 E) in
  check_success "size" (fun () -> size t |> ignore);
  check_raises_ortac "size_wrong_spec" (fun () -> size_wrong_spec t |> ignore);
  check_raises_ortac "test_tree" (fun () -> test_tree t |> ignore);
  check_success "make_alt_tree" (fun () -> make_alt_tree Ealt 42 Ealt |> ignore);
  check_success "fill 1" (fun () -> fill E [||] 0 |> ignore);
  check_raises_ortac "fill 2" (fun () -> fill E [||] 1 |> ignore);
  check_success "fill 3" (fun () -> fill (N (E, 42, E)) [| 0; 1 |] 1 |> ignore)

let suite =
  ( "Terms",
    [
      ("boolean operators", `Quick, bool_ops);
      ("scopes", `Quick, scopes);
      ("logic", `Quick, logic);
      ("patterns", `Quick, patterns);
      ("peano", `Quick, peano);
      ("trees", `Quick, trees);
    ] )
