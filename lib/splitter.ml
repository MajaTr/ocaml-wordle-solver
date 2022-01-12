open! Core
open Splitter_intf

module type S = S

module Make (M : S) = struct
  type state = { sampled : Environment.Word_handle.t list }

  let init ~env = { sampled = Environment.sampled_words env }

  let guess ~env { sampled } =
    match sampled with
    | [ answer ] -> answer
    | _ ->
        let _, guess =
          List.map (Environment.allowed_words env) ~f:(fun guess ->
              ( List.map sampled ~f:(fun hidden ->
                    Environment.Word_handle.guess_result ~env ~guess ~hidden)
                |> M.evaluate,
                guess ))
          |> List.max_elt ~compare:(fun (x, _) (y, _) -> M.compare x y)
          |> Option.value_exn
        in
        guess

  let update ~env { sampled } ~guess ~result =
    {
      sampled =
        List.filter sampled ~f:(fun hidden ->
            Guess_result.(
              result = Environment.Word_handle.guess_result ~env ~guess ~hidden));
    }
end
