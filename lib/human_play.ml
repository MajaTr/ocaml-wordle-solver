open! Core

type state = unit

let init _ = ()

let guess () = In_channel.(input_line stdin) |> Option.value_exn

let update () ~guess:_ ~result = print_endline (Guess_result.to_string result)
