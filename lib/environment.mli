open! Core

type t

type env = t

module Word_handle : sig
  type t [@@deriving compare, hash, equal, sexp]

  val to_string : t -> env:env -> string

  val guess_result : guess:t -> hidden:t -> env:env -> Guess_result.t
end

val import :
  filename:Filename.t -> word_length:int -> sampled_num:int option -> t

val allowed_words : t -> Word_handle.t list

val get_handle : t -> string -> Word_handle.t option

val sampled_words : t -> Word_handle.t list

val sample : t -> Word_handle.t
