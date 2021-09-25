let lazy_bool x = x

let not_lazy_or x = x

let not_lazy_and x = x

let scope1 x = x

let if_forall x = x

let equiv () = ()

let exists_ () = ()

type t = A | B of string

let a = ignore

let b = ignore

type peano = O | S of peano

let succ x = S x

let rec add x y = match x with O -> y | S x -> S (add x y)

let rec bad_add x y = match x with O -> O | S x -> S (bad_add x y)

type tree = E | N of tree * int * tree

let rec size = function E -> 0 | N (l, _, r) -> size l + 1 + size r

let size_wrong_spec = size

let test_tree = function E -> true | N (l, _, r) -> l = r

let make_tree l x r = N (l, x, r)

let rec fill t a start =
  match t with
  | E -> start
  | N (l, x, r) ->
      let start = fill l a start in
      if start = Array.length a then start
      else (
        a.(start) <- x;
        fill r a (start + 1))

type alt_tree = Ealt | Nalt of (alt_tree * int * alt_tree)

let make_alt_tree l x r = Nalt (l, x, r)
