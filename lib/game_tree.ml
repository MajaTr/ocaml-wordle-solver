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
    | [ hidden ] -> Leaf hidden
    | _ ->
        let guess = M.guess state in
        let next =
          List.map sampled ~f:(fun hidden ->
              (Guess_result.obtain guess ~hidden, hidden))
          |> Guess_result.Map.of_alist_multi |> Map.to_alist
          |> List.map ~f:(fun (result, sampled) ->
                 (result, make_rec ~sampled (M.update state ~guess ~result)))
        in
        Branch { guess; next }
  in
  make_rec ~sampled:(Environment.sampled_words env) (M.init env)

let to_player t =
  (module struct
    type state = t

    let init _ = t

    let guess = function Leaf s -> s | Branch { guess; _ } -> guess

    let update t ~guess:_ ~result =
      match t with
      | Leaf _ -> t
      | Branch { next; _ } ->
          List.find_map_exn next ~f:(fun (label, tree) ->
              if Guess_result.compare result label = 0 then Some tree else None)
  end : Player.S)
