open! Core

let counts xs =
  let a = Hashtbl.create (module Int) in
  List.iter xs ~f:(Hashtbl.incr a);
  Hashtbl.data a |> List.filter ~f:(fun x -> x <> 0)

type t = int list

let compare t1 t2 = List.compare Int.descending t1 t2

let evaluate results =
  List.map results ~f:Guess_result.to_int
  |> counts
  |> List.sort ~compare:Int.descending
