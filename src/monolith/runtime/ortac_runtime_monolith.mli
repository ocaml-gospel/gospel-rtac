val constructible_int : (int, int) Monolith.spec

val int : (int, int) Monolith.spec

val positive_int : (int, int) Monolith.spec

val array : ('r, 'c) Monolith.spec -> ('r array, 'c array) Monolith.spec

module Printer : sig
  val tuple2 :
    'a Monolith.printer -> 'b Monolith.printer -> ('a * 'b) Monolith.printer

  val tuple3 :
    'a Monolith.printer ->
    'b Monolith.printer ->
    'c Monolith.printer ->
    ('a * 'b * 'c) Monolith.printer

  val tuple4 :
    'a Monolith.printer ->
    'b Monolith.printer ->
    'c Monolith.printer ->
    'd Monolith.printer ->
    ('a * 'b * 'c * 'd) Monolith.printer

  val tuple5 :
    'a Monolith.printer ->
    'b Monolith.printer ->
    'c Monolith.printer ->
    'd Monolith.printer ->
    'e Monolith.printer ->
    ('a * 'b * 'c * 'd * 'e) Monolith.printer
end
