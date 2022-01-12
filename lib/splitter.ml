open! Core
open Splitter_intf

module type S = S

module Make (M : S) = struct
  type state = {
    sampled : Environment.Word_handle.t list;
    allowed : Environment.Word_handle.t list;
  }

  type adv_state = { state : state; guess : Environment.Word_handle.t }

  let init ~env =
    {
      sampled = Environment.sampled_words env;
      allowed = Environment.allowed_words env;
    }

  type is_all_equal = Empty | Yes of Guess_result.t | No

  let guess ~env ({ sampled; allowed } as state) =
    let update_all_equal x y =
      match x with
      | Empty -> Yes y
      | Yes y' -> if Guess_result.(y = y') then Yes y' else No
      | No -> No
    in
    let process guess =
      match
        List.fold_map sampled
          ~f:(fun s hidden ->
            let result =
              Environment.Word_handle.guess_result ~env ~guess ~hidden
            in
            (update_all_equal s result, result))
          ~init:Empty
      with
      | No, results -> Some results
      | (Empty | Yes _), _ -> None
    in
    match sampled with
    | [ answer ] -> (answer, { state; guess = answer })
    | _ ->
        let guess_values =
          List.filter_map allowed ~f:(fun guess ->
              let%map.Option results = process guess in
              (M.evaluate results, guess))
        in
        let _, best_guess =
          List.max_elt guess_values ~compare:(fun (x, _) (y, _) ->
              M.compare x y)
          |> Option.value_exn
        in

        let allowed = List.map ~f:snd guess_values in
        (best_guess, { state = { sampled; allowed }; guess = best_guess })

  let update ~env { state = { sampled; allowed }; guess } ~result =
    {
      sampled =
        List.filter sampled ~f:(fun hidden ->
            Guess_result.(
              result = Environment.Word_handle.guess_result ~env ~guess ~hidden));
      allowed;
    }
end
