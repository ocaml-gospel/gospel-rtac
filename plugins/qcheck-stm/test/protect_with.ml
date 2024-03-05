type 'a t = 'a list ref

exception Empty

let make () = ref []
let push a t = t := a :: !t

let pop t =
  match !t with
  | [] -> raise Empty
  | x :: xs ->
      t := xs;
      x
