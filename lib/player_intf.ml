open! Core

module type S = sig
  type state

  type adv_state

  val init : env:Environment.t -> state

  val guess :
    env:Environment.t -> state -> Environment.Word_handle.t * adv_state

  val update : env:Environment.t -> adv_state -> result:Guess_result.t -> state
end

module type Intf = sig
  module type S = S

  module With_printing (M : S) : S

  val play :
    (module S) -> Environment.t -> hidden:Environment.Word_handle.t -> unit

  val cheat : (module S) -> Environment.t -> unit
end
