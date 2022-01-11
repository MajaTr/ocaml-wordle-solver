open! Core

let counts xs =
  let a = Hashtbl.create (module Int) in
  List.iter xs ~f:(Hashtbl.incr a);
  Hashtbl.data a |> List.filter ~f:(fun x -> x <> 0)

type t = int [@@deriving compare]

let evaluate results =
  let max =
    List.map results ~f:Guess_result.to_int
    |> counts
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  -max
