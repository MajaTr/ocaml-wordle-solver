open! Core
open Splitter_intf

module type S = S

module Make (M : S) = struct
  type state = { sampled : string list; allowed : string list }

  let init env =
    {
      sampled = Environment.sampled_words env;
      allowed = Environment.allowed_words env;
    }

  let guess { sampled; allowed } =
    match sampled with
    | [ answer ] -> answer
    | _ ->
        let _, guess =
          List.map allowed ~f:(fun guess ->
              ( List.map sampled ~f:(fun x ->
                    Guess_result.obtain guess ~hidden:x)
                |> M.evaluate,
                guess ))
          |> List.max_elt ~compare:(fun (x, _) (y, _) -> M.compare x y)
          |> Option.value_exn
        in
        guess

  let update { sampled; allowed } ~guess ~result =
    (* print_s [%message (result : Guess_result.t) (sampled : string list)];*)
    {
      sampled =
        List.filter sampled ~f:(fun w ->
            Guess_result.compare result (Guess_result.obtain guess ~hidden:w)
            = 0);
      allowed;
    }
end
