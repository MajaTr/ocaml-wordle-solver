open! Core

module type Game = sig
  type state

  type final_state

  type adv_state

  type move

  type adv_move

  module Value : sig
    type t [@@deriving compare]
  end

  val init : env:Environment.t -> state

  val eval : state -> Value.t

  val final_eval : final_state -> Value.t

  val adv_eval : adv_state -> Value.t

  val moves : state -> move Sequence.t

  val adv_moves : adv_state -> adv_move Sequence.t

  val apply : state -> move -> adv_state

  val adv_apply : adv_state -> adv_move -> (state, final_state) Either.t

  val guess_of_move : move -> Environment.Word_handle.t

  val adv_move_of_result : Guess_result.t -> adv_move
end

module type Params = sig
  val depth : int

  val breadth : int
end

module type Intf = sig
  module type Game = Game

  module Make (_ : Game) (_ : Params) : Player.S
end
