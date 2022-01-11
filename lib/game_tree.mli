open! Core

type t [@@deriving sexp]

val make : (module Player.S) -> Environment.t -> t

val to_player : t -> (module Player.S)

val depths : t -> int String.Map.t
