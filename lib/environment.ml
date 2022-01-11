open! Core

type t = { allowed : String.Set.t; sampled : string list }

let import ~filename ~word_length ~sampled_num =
  let words_by_freq =
    Sexp.load_sexps filename
    |> List.map ~f:[%of_sexp: string * int]
    |> List.filter ~f:(fun (word, _) -> String.length word = word_length)
  in
  print_s [%message (List.length words_by_freq : int)];
  let allowed = List.map words_by_freq ~f:fst |> String.Set.of_list in
  let sampled = List.take words_by_freq sampled_num |> List.map ~f:fst in
  { allowed; sampled }

let allowed_words { allowed; _ } = Set.to_list allowed

let is_allowed { allowed; _ } s = Set.mem allowed s

let sampled_words { sampled; _ } = sampled

let sample { sampled; _ } = List.random_element_exn sampled
