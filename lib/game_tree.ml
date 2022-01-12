open! Core

type t =
  | Leaf of string
  | Branch of { guess : string; next : (Guess_result.t * t) list }
[@@deriving sexp]

exception Empty_samples of unit

let make (module M : Player.S) env =
  let rec make_rec ~sampled state =
    match sampled with
    | [] -> raise (Empty_samples ())
    | [ hidden ] -> Leaf (Environment.Word_handle.to_string hidden ~env)
    | _ ->
        let guess = M.guess ~env state in
        let next =
          List.map sampled ~f:(fun hidden ->
              (Environment.Word_handle.guess_result ~env ~guess ~hidden, hidden))
          |> Guess_result.Map.of_alist_multi |> Map.to_alist
          |> List.map ~f:(fun (result, sampled) ->
                 (result, make_rec ~sampled (M.update state ~env ~guess ~result)))
        in
        Branch { guess = Environment.Word_handle.to_string guess ~env; next }
  in
  make_rec ~sampled:(Environment.sampled_words env) (M.init ~env)

let to_player init =
  (module struct
    type state = t

    let init ~env:_ = init

    let guess ~env t =
      Environment.get_handle env
        (match t with Leaf s -> s | Branch { guess; _ } -> guess)
      |> Option.value_exn ~message:"Word not allowed"

    let update ~env:_ t ~guess:_ ~result =
      match t with
      | Leaf _ -> t
      | Branch { next; _ } ->
          List.find_map_exn next ~f:(fun (label, tree) ->
              if Guess_result.compare result label = 0 then Some tree else None)
  end : Player.S)

let depths t =
  let upd mp s d =
    Map.update mp s ~f:(Option.value_map ~f:(Int.min d) ~default:d)
  in
  let rec depths_rec d mp t =
    match t with
    | Leaf s -> upd mp s d
    | Branch { guess; next } ->
        List.map next ~f:snd
        |> List.fold ~init:(upd mp guess d) ~f:(depths_rec (d + 1))
  in

  depths_rec 0 String.Map.empty t
