open! Core

type t [@@deriving compare, sexp]

val obtain : string -> hidden:string -> t

val guessed : t -> bool

val display_string : t -> guess:string -> string

val to_int : t -> int
