open Monolith

let constructible_int = int_within (Gen.semi_open_interval (-420000) 420000)

let int = ifpol constructible_int int

let positive_int = int_within (Gen.int Int.max_int)

let array spec =
  map_outof Array.of_list
    (Array.of_list, constant "array foom list")
    (list spec)

module Printer = struct
  let tuple2 = Print.pair

  let tuple3 printer0 printer1 printer2 (a, b, c) =
    PPrint.(
      lparen ^^ printer0 a ^^ comma ^^ printer1 b ^^ comma ^^ printer2 c
      ^^ rparen)

  let tuple4 printer0 printer1 printer2 printer3 (a, b, c, d) =
    PPrint.(
      lparen ^^ printer0 a ^^ comma ^^ printer1 b ^^ comma ^^ printer2 c
      ^^ comma ^^ printer3 d ^^ rparen)

  let tuple5 printer0 printer1 printer2 printer3 printer4 (a, b, c, d, e) =
    PPrint.(
      lparen ^^ printer0 a ^^ comma ^^ printer1 b ^^ comma ^^ printer2 c
      ^^ comma ^^ printer3 d ^^ comma ^^ printer4 e ^^ rparen)
end
