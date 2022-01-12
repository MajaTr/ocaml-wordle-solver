open! Core
include Player_intf

module With_printing (M : S) = struct
  type state = M.state

  type adv_state = M.adv_state

  let init = M.init

  let guess ~env state =
    let res, adv_state = M.guess ~env state in
    print_endline (Environment.Word_handle.to_string res ~env);
    (res, adv_state)

  let update ~env adv_state ~result =
    print_endline (Guess_result.to_string result);
    M.update ~env adv_state ~result
end

let play (module M : S) env ~hidden =
  let rec loop state =
    let guess, adv_state = M.guess ~env state in
    let result = Environment.Word_handle.guess_result ~env ~guess ~hidden in
    let new_state = M.update ~env adv_state ~result in
    if Guess_result.guessed result then print_endline "Guessed correctly!"
    else loop new_state
  in
  loop (M.init ~env)

let cheat (module M : S) env =
  let rec loop state =
    let guess, adv_state = M.guess ~env state in
    print_endline (Environment.Word_handle.to_string guess ~env);
    let result =
      In_channel.(input_line stdin)
      |> Option.value_exn |> Guess_result.of_string |> Option.value_exn
    in
    loop (M.update ~env adv_state ~result)
  in
  loop (M.init ~env)
