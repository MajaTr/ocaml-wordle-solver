open! Core

module Bit = struct
  type t = int

  let to_int t = t

  let of_int_exn t = t

  let bit_width = 1
end

module Sampled_repr = struct
  include Packed_vector.Make (Bit)
end

module Id = struct
  type t = int

  let to_int t = t

  let of_int_exn t = t

  let bit_width = 4
end

module Move_repr = struct
  module Vector = Packed_vector.Make (Id)

  type t = { splits : Vector.t; guess : Environment.Word_handle.t }
end

let solve _ = failwith ""

and adv_solve _ = failwith ""

type state = { sampled : Sampled_repr.t; moves : Move_repr.t list }

type adv_state = { state : state; move : Move_repr.t }

let init ~env:_ = failwith ""

let guess ~env:_ state = solve state

let update ~env:_ adv_state = adv_solve adv_state
