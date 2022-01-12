open! Core

module type Packable = sig
  type t

  val of_int_exn : int -> t

  val to_int : t -> int

  val bit_width : int
end

module type Intf = sig
  module type Packable = Packable

  module Make (M : Packable) : sig
    type t [@@deriving compare, sexp]

    val get : t -> int -> M.t

    val set : t -> int -> M.t -> t

    val init : int -> f:(int -> M.t) -> t

    val fold : t -> init:'a -> f:('a -> M.t -> 'a) -> 'a

    val for_all : t -> f:(M.t -> bool) -> bool

    val to_int : t -> int
  end
end
