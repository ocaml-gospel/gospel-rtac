type 'a t
(*@ mutable model contents : 'a sequence *)

exception Empty

val make : unit -> 'a t
(*@ t = make ()
    ensures t.contents = Sequence.empty *)

val push : 'a -> 'a t -> unit
(*@ push a t
    modifies t.contents
    ensures t.contents = Sequence.cons a (old t.contents) *)

val pop : 'a t -> 'a
(*@ a = pop t
    modifies t.contents
    ensures t.contents = if old t.contents = Sequence.empty
                         then Sequence.empty
                         else Sequence.tl (old t.contents)
    ensures (old t.contents) = Sequence.cons a t.contents
    raises Empty -> old t.contents = Sequence.empty = t.contents *)
