open! Core

module type S = sig
  type t [@@deriving compare]

  val evaluate : Guess_result.t list -> t
end

module type Intf = sig
  module type S = S

  module Make (_ : S) : Player.S
end
