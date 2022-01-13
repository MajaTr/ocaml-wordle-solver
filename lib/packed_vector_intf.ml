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
    type t [@@deriving compare, hash, sexp, equal]

    include Comparable.S with type t := t

    val get : t -> int -> M.t

    val set : t -> int -> M.t -> t

    val init : int -> f:(int -> M.t) -> t

    val length : t -> int

    val of_list : M.t list -> t

    val to_sequence : t -> M.t Sequence.t

    val fold : t -> init:'a -> f:('a -> M.t -> 'a) -> 'a

    val for_all : t -> f:(M.t -> bool) -> bool

    val to_int : t -> int

    val popcount : t -> int
  end
end
