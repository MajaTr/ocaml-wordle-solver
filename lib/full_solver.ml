open! Core

module Sampled_repr = struct
  type t = int
end

module Move_repr = struct
  type t = int
end

type state = { sampled : Sampled_repr.t; moves : Move_repr.t list }

type adv_state = { state : state; guess : Environment.Word_handle.t }

let init ~env:_ = failwith ""

let guess ~env:_ = failwith ""

let update ~env:_ = failwith ""
