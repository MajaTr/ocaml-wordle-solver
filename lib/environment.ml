open! Core

type t = { allowed : String.Set.t; sampled : string list }

let import ~allowed_file ~sampled_file ~word_length ~sampled_num =
  let allowed_words =
    Wordlist_input.read_words allowed_file
    |> List.filter ~f:(fun x -> String.length x = word_length)
  in
  let sampled_words_dups =
    Wordlist_input.read_frequent_words sampled_file
    |> List.filter ~f:(fun x -> String.length x = word_length)
  in
  let sampled_words =
    List.take sampled_words_dups sampled_num
    |> List.dedup_and_sort ~compare:String.compare
    |> List.filter ~f:(List.mem allowed_words ~equal:String.equal)
  in
  { allowed = String.Set.of_list allowed_words; sampled = sampled_words }

let allowed_words { allowed; _ } = Set.to_list allowed

let is_allowed { allowed; _ } s = Set.mem allowed s

let sampled_words { sampled; _ } = sampled

let sample { sampled; _ } = List.random_element_exn sampled
