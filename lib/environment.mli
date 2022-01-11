open! Core

type t

val import : filename:Filename.t -> word_length:int -> sampled_num:int -> t

val allowed_words : t -> string list

val is_allowed : t -> string -> bool

val sampled_words : t -> string list

val sample : t -> string
