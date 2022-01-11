open! Core

module type S = sig
  type state

  val init : Environment.t -> state

  val guess : state -> string

  val update : state -> guess:string -> result:Guess_result.t -> state
end

module type Intf = sig
  module type S = S

  module With_printing (M : S) : S

  val play : (module S) -> Environment.t -> hidden:string -> unit

  val cheat : (module S) -> Environment.t -> unit
end
