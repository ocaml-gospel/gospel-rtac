val term_printer : string -> Ppxlib.Location.t -> Gospel.Tterm.term -> string
(** [term_printer text global_loc term] fetch the initial text representation of
    [term] provided that [text] is the specification's text and [global_loc] the
    specification's location. Fall back on the Gospel term pretty printer if
    something goes wrong when extracting the substring. *)

type checked = {
  module_name : string;
  namespace : Gospel.Tmodule.namespace;
  ast : Gospel.Tast.signature;
}

val check : Registration.input_file -> checked
(** [check filename] calls the Gospel type checker on [filename] with an empty
    load path if it is an interface and read its content if it is a [.gospel]
    file *)
