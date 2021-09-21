open Monolith

let constructible_int = int_within (Gen.semi_open_interval (-420000) 420000)

let int = ifpol constructible_int int

let positive_int = int_within (Gen.int Int.max_int)

let array spec =
  map_outof Array.of_list
    (Array.of_list, constant "array from list")
    (list spec)
