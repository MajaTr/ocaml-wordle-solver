open! Core

type t [@@deriving compare, sexp]

val obtain : guess:string -> hidden:string -> t

val guessed : t -> bool

val to_string : t -> string

val of_string : string -> t option

val to_int : t -> int

include Comparable.S with type t := t
