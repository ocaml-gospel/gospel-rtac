type plugins
type input_file = MLI of string | GOSPEL of string

val unwrap : input_file -> string
val plugins : plugins
val register : unit Cmdliner.Cmd.t -> unit
val fold : ('a -> unit Cmdliner.Cmd.t -> 'a) -> 'a -> plugins -> 'a
val get_out_formatter : string option -> Format.formatter
val setup_log : unit Cmdliner.Term.t
val include_ : string option Cmdliner.Term.t
val output_file : string option Cmdliner.Term.t
val input_file : input_file Cmdliner.Term.t
val quiet : bool Cmdliner.Term.t
