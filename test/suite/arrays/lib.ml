let create = Array.make

let bad_create n _ = Array.make n 42

let get = Array.get

let set = Array.set

let bad_get a i = a.(i + 1)

let fill = Array.fill

let sort = Array.sort compare

let copy_sort a =
  let b = Array.copy a in
  sort b;
  b

let bad_sort _ = [| 314; 42; 73; 57; 421 |]

let constant_sort a = create (Array.length a) 42
