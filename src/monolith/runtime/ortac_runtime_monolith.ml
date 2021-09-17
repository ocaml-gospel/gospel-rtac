open Monolith

let constructible_int = int_within (Gen.semi_open_interval (-420000) 420000)

let int = ifpol constructible_int int

let positive_int = int_within (Gen.int Int.max_int)

let array spec =
  map_outof Array.of_list
    (Array.of_list, constant "array foom list")
    (list spec)

module Gen = struct
  let tuple2 gen0 gen1 () = (gen0 (), gen1 ())

  let tuple3 gen0 gen1 gen2 () = (gen0 (), gen1 (), gen2 ())

  let tuple4 gen0 gen1 gen2 gen3 () = (gen0 (), gen1 (), gen2 (), gen3 ())

  let tuple5 gen0 gen1 gen2 gen3 gen4 () =
    (gen0 (), gen1 (), gen2 (), gen3 (), gen4 ())
end
