open! Core

type state = unit

let init _ = ()

let guess () = In_channel.(input_line stdin) |> Option.value_exn

let update () ~guess ~result =
  print_endline (Guess_result.display_string result ~guess)
