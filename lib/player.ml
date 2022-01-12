open! Core
include Player_intf

module With_printing (M : S) = struct
  type state = M.state

  let init = M.init

  let guess ~env state =
    let res = M.guess ~env state in
    print_endline (Environment.Word_handle.to_string res ~env);
    res

  let update ~env state ~guess ~result =
    print_endline (Guess_result.to_string result);
    M.update ~env state ~guess ~result
end

let play (module M : S) env ~hidden =
  let rec loop state =
    let guess = M.guess ~env state in
    let result = Environment.Word_handle.guess_result ~env ~guess ~hidden in
    let new_state = M.update ~env state ~guess ~result in
    if Guess_result.guessed result then print_endline "Guessed correctly!"
    else loop new_state
  in
  loop (M.init ~env)

let cheat (module M : S) env =
  let rec loop state =
    let guess = M.guess ~env state in
    print_endline (Environment.Word_handle.to_string guess ~env);
    let result =
      In_channel.(input_line stdin)
      |> Option.value_exn |> Guess_result.of_string |> Option.value_exn
    in
    loop (M.update ~env state ~guess ~result)
  in
  loop (M.init ~env)
