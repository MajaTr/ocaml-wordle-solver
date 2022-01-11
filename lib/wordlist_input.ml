open Core

let read_words filename =
  In_channel.with_file filename ~f:In_channel.input_lines
  |> List.map ~f:String.lowercase

let read_frequent_words filename =
  In_channel.with_file filename ~f:In_channel.input_lines
  |> List.map ~f:(fun line -> List.nth_exn (String.split line ~on:' ') 1)
  |> List.map ~f:String.lowercase

let adhoc_compose ~allowed ~sampled ~out =
  let allowed_words =
    In_channel.with_file allowed ~f:In_channel.input_lines
    |> List.map ~f:String.lowercase
    |> String.Set.of_list
  in
  let word_to_freq =
    In_channel.with_file sampled ~f:In_channel.input_lines
    |> List.map ~f:(fun line ->
           match String.split line ~on:' ' with
           | freq :: word :: _ ->
               ( String.filter word ~f:Char.is_alpha |> String.lowercase,
                 Int.of_string freq )
           | _ -> failwith "file ill-formatted")
    |> String.Map.of_alist_reduce ~f:( + )
  in
  Map.of_key_set allowed_words ~f:(fun w ->
      Map.find word_to_freq w |> Option.value_map ~f:(( + ) 1) ~default:1)
  |> Map.to_alist
  |> List.sort ~compare:(fun (_, x) (_, y) -> Int.compare y x)
  |> List.map ~f:[%sexp_of: string * int]
  |> Sexp.save_sexps out
