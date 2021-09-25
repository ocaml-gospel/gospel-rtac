open Ppxlib
open Gospel

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_setup : location -> string -> (expression -> expression) * string

val mk_pre_checks :
  driver:Drv.t ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_call :
  driver:Drv.t ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  pattern ->
  location ->
  label ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  (arg_label * expression) list ->
  expression ->
  expression

val mk_post_checks :
  driver:Drv.t ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_function_def : driver:Drv.t -> Tterm.term -> expression option
