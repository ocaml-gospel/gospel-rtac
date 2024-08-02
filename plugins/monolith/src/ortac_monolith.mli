val generate : Registration.input_file -> Format.formatter -> unit
(** [generate path output] generate the code of the tests corresponding to the
    specifications present in [path] in the monolith configuration and print it
    on the [output] channel *)
