val constructible_int : (int, int) Monolith.spec

val int : (int, int) Monolith.spec

val positive_int : (int, int) Monolith.spec

val array : ('r, 'c) Monolith.spec -> ('r array, 'c array) Monolith.spec

module Gen : sig
  val tuple2 : 'a Monolith.gen -> 'b Monolith.gen -> ('a * 'b) Monolith.gen

  val tuple3 :
    'a Monolith.gen ->
    'b Monolith.gen ->
    'c Monolith.gen ->
    ('a * 'b * 'c) Monolith.gen

  val tuple4 :
    'a Monolith.gen ->
    'b Monolith.gen ->
    'c Monolith.gen ->
    'd Monolith.gen ->
    ('a * 'b * 'c * 'd) Monolith.gen

  val tuple5 :
    'a Monolith.gen ->
    'b Monolith.gen ->
    'c Monolith.gen ->
    'd Monolith.gen ->
    'e Monolith.gen ->
    ('a * 'b * 'c * 'd * 'e) Monolith.gen
end

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
