open Core

val read_words : Filename.t -> string list

val read_frequent_words : Filename.t -> string list

val adhoc_compose :
  allowed:Filename.t -> sampled:Filename.t -> out:Filename.t -> unit
