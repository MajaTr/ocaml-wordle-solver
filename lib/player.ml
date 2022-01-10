open! Core
include Player_intf

module With_printing (M : S) = struct
  type state = M.state

  let init = M.init

  let guess s =
    let res = M.guess s in
    print_endline res;
    res

  let update s ~guess ~result =
    print_endline (Guess_result.display_string result ~guess);
    M.update s ~guess ~result
end

exception Word_not_allowed of string

let play (module M : S) env ~hidden =
  (* print_s [%message (hidden : string)];*)
  let rec loop state =
    let guess = M.guess state in
    if not (Environment.is_allowed env guess) then
      raise (Word_not_allowed "Not a word")
    else
      let result = Guess_result.obtain guess ~hidden in
      let new_state = M.update state ~guess ~result in
      if Guess_result.guessed result then print_endline "Guessed correctly!"
      else loop new_state
  in

  loop (M.init env)
