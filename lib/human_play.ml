open! Core

type state = unit

let init ~env:_ = ()

let guess ~env () =
  In_channel.(input_line stdin)
  |> Option.value_exn |> Environment.get_handle env
  |> Option.value_exn ~message:"Word not allowed"

let update ~env:_ () ~guess:_ ~result =
  print_endline (Guess_result.to_string result)
