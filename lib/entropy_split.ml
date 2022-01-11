open! Core

let entropy sizes =
  let total = List.sum (module Int) sizes ~f:Fn.id |> Float.of_int in
  let ps = List.map sizes ~f:(fun s -> Float.of_int s /. total) in
  List.sum (module Float) ps ~f:(fun p -> -.p *. log p)

let counts xs =
  let a = Hashtbl.create (module Int) in
  List.iter xs ~f:(Hashtbl.incr a);
  Hashtbl.data a |> List.filter ~f:(fun x -> x <> 0)

type t = float [@@deriving compare]

let evaluate results =
  List.map results ~f:Guess_result.to_int |> counts |> entropy
