open Core

let read_words filename =
  In_channel.with_file filename ~f:In_channel.input_lines
  |> List.map ~f:String.lowercase

let read_frequent_words filename =
  In_channel.with_file filename ~f:In_channel.input_lines
  |> List.map ~f:(fun line -> List.nth_exn (String.split line ~on:' ') 1)
  |> List.map ~f:String.lowercase
