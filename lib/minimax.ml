open! Core
open Minimax_intf

module type Game = Game

module Make (M : Game) (P : Params) = struct
  type state = (M.state, M.final_state) Either.t

  type adv_state = M.adv_state

  let init ~env = First (M.init ~env)

  type player = Player | Adversary

  let other = function Player -> Adversary | Adversary -> Player

  let at_least_as_good x ~as_ ~for_ =
    match (x, as_) with
    | _, None -> true
    | None, Some _ -> false
    | Some x, Some as_ -> (
        let c = M.Value.compare x as_ in
        match for_ with Player -> c >= 0 | Adversary -> c <= 0)

  let update_option_pair (s, extra) ~with_:(x, new_extra) ~player =
    if at_least_as_good x ~as_:s ~for_:player then (x, new_extra) else (s, extra)

  let update_option s ~with_ ~player =
    let res, () = update_option_pair (s, ()) ~with_:(with_, ()) ~player in
    res

  module Pruning = struct
    type t = { player's : M.Value.t option; adversary's : M.Value.t option }

    let init = { player's = None; adversary's = None }

    let get t ~player =
      match player with Player -> t.player's | Adversary -> t.adversary's

    let update t x ~player =
      let new_s = update_option (get t ~player) ~with_:(Some x) ~player in
      match player with
      | Player -> { t with player's = new_s }
      | Adversary -> { t with adversary's = new_s }

    let should_stop t x ~player =
      at_least_as_good
        (get t ~player:(other player))
        ~as_:(Some x) ~for_:(other player)
  end

  let sequence_fold ~player ~apply ~other_solve ~state ~pruning seq =
    let value, maybe_move =
      Sequence.fold_until seq
        ~init:(pruning, (None, None))
        ~f:(fun (pruning, best) move ->
          let x, _ = apply state move |> other_solve ~pruning in
          let pruning = Pruning.update pruning x ~player in
          let best =
            update_option_pair best ~with_:(Some x, Some move) ~player
          in
          if Pruning.should_stop pruning x ~player then
            Continue_or_stop.Stop best
          else Continue (pruning, best))
        ~finish:(fun (_, best) -> best)
    in
    (Option.value_exn value, maybe_move)

  let rec solve ~(pruning : Pruning.t) ~ply_left state =
    match (ply_left, state) with
    | _, Second final_state -> (M.final_eval final_state, None)
    | 0, First state -> (M.eval state, None)
    | _, First state ->
        Sequence.take (M.moves state) P.move_breadth
        |> sequence_fold ~player:Player ~apply:M.apply
             ~other_solve:(adv_solve ~ply_left:(ply_left - 1))
             ~state ~pruning

  and adv_solve ~(pruning : Pruning.t) ~ply_left adv_state =
    match ply_left with
    | 0 -> (M.adv_eval adv_state, None)
    | _ ->
        Sequence.take (M.adv_moves adv_state) P.move_breadth
        |> sequence_fold ~player:Adversary ~apply:M.adv_apply
             ~other_solve:(solve ~ply_left:(ply_left - 1))
             ~state:adv_state ~pruning

  exception Game_end

  exception No_moves

  let guess ~env:_ state =
    let _, move = solve ~pruning:Pruning.init ~ply_left:P.ply_depth state in
    match (state, move) with
    | First state, Some move -> (M.guess_of_move move, M.apply state move)
    | Second _, _ -> raise Game_end
    | _, None -> raise No_moves

  let update ~env:_ adv_state ~result =
    M.adv_move_of_result result |> M.adv_apply adv_state
end
