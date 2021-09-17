type location = { start : Lexing.position; stop : Lexing.position }

type term_kind = Pre | Post | XPost

type error =
  | Violated_condition of { term : string; term_kind : term_kind }
  | Specification_failure of { term : string; term_kind : term_kind; exn : exn }
  | Unexpected_exception of { allowed_exn : string list; exn : exn }
  | Uncaught_checks of { term : string }
  | Unexpected_checks of { terms : string list }

type error_report = {
  loc : location;
  fun_name : string;
  mutable errors : error list;
}

val pp_error_report : Format.formatter -> error_report -> unit

exception Error of error_report

module Errors : sig
  type t

  val create : location -> string -> t
  (** [empty] create a new empty error container *)

  val register : t -> error -> unit
  (** [register t a] add the element [a] to [t] *)

  val report : t -> unit
  (** [report l] prints the content of [l] *)
end

module Z : sig
  include module type of Z

  val pow : t -> t -> t

  val exists : t -> t -> (t -> bool) -> bool
  (** [exists i j p] is [true] iff the predicate there exists [k] within [i] and
      [j], included, for which [p] holds. *)

  val forall : t -> t -> (t -> bool) -> bool
  (** [forall i j p] is [true] iff the predicate `p` holds forall [k] within [i]
      and [j], included. *)

  val max_int : t

  val min_int : t
end

module Array : sig
  include module type of Array

  val length : 'a array -> Z.t

  val get : 'a array -> Z.t -> 'a

  val make : Z.t -> 'a -> 'a array

  val sub : 'a array -> Z.t -> Z.t -> 'a array

  val mapi : (Z.t -> 'a -> 'b) -> 'a array -> 'b array
end

module List : sig
  include module type of List

  val length : 'a list -> Z.t

  val nth : 'a list -> Z.t -> 'a

  val nth_opt : 'a list -> Z.t -> 'a option

  val init : Z.t -> (Z.t -> 'a) -> 'a list

  val mapi : (Z.t -> 'a -> 'b) -> 'a list -> 'b list
end
