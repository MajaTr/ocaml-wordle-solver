open! Core

type t =
  | Done
  | Final of string
  | Branch of { guess : string; next : (Guess_result.t * t) list }
[@@deriving sexp]

exception Empty_samples of unit

let make (module M : Player.S) env =
  let rec make_rec ~sampled state =
    match sampled with
    | [] -> raise (Empty_samples ())
    | [ hidden ] -> Final (Environment.Word_handle.to_string hidden ~env)
    | _ ->
        let guess, adv_state = M.guess ~env state in
        let next =
          List.map sampled ~f:(fun hidden ->
              (Environment.Word_handle.guess_result ~env ~guess ~hidden, hidden))
          |> Guess_result.Map.of_alist_multi |> Map.to_alist
          |> List.map ~f:(fun (result, sampled) ->
                 ( result,
                   if Guess_result.guessed result then Done
                   else make_rec ~sampled (M.update adv_state ~env ~result) ))
        in
        Branch { guess = Environment.Word_handle.to_string guess ~env; next }
  in
  make_rec ~sampled:(Environment.sampled_words env) (M.init ~env)

let to_player init =
  (module struct
    type state = t

    type adv_state = t

    let init ~env:_ = init

    exception Tree_end

    let guess ~env t =
      let guess =
        match t with
        | Done -> raise Tree_end
        | Final s -> s
        | Branch { guess; _ } -> guess
      in
      ( Environment.get_handle env guess
        |> Option.value_exn ~message:"Word not allowed",
        t )

    let update ~env:_ t ~result =
      match t with
      | Done | Final _ -> t
      | Branch { next; _ } ->
          List.find_map_exn next ~f:(fun (label, tree) ->
              if Guess_result.compare result label = 0 then Some tree else None)
  end : Player.S)

let depths t =
  let upd mp s d = Map.add_exn mp ~key:s ~data:d in
  let rec depths_rec d mp t =
    match t with
    | Done -> mp
    | Final s -> upd mp s d
    | Branch { guess; next } ->
        List.map next ~f:snd
        |> List.fold ~init:mp ~f:(fun mp -> function
             | Done -> upd mp guess d
             | (Final _ | Branch _) as t -> depths_rec (d + 1) mp t)
  in

  depths_rec 0 String.Map.empty t

let rec truncate t ~depth =
  match (t, depth) with
  | Branch { guess; _ }, 0 -> Final guess
  | Branch { guess; next }, depth ->
      Branch
        {
          guess;
          next =
            List.map next ~f:(fun (result, t') ->
                (result, truncate t' ~depth:(depth - 1)));
        }
  | ((Final _ | Done) as t), _ -> t
