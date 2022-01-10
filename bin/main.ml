open Core
open Wordle_solver

let test words =
  List.filter ~f:(fun x -> String.length x = 5) words |> List.length

let test_cmd =
  Command.basic ~summary:"Test"
    Command.Let_syntax.(
      let%map_open file = anon ("filename" %: Filename.arg_type) in
      fun () -> printf "%d\n" (Wordlist_input.read_words file |> test))

let play_cmd =
  Command.basic ~summary:"Let the computer play"
    Command.Let_syntax.(
      let%map_open data_folder =
        flag "--data-folder"
          (optional_with_default "data/" Filename.arg_type)
          ~doc:"word list folder"
      and word_length =
        flag "--word-length"
          (optional_with_default 5 int)
          ~doc:"word length for the game"
      and sampled_num =
        flag "--sampled-num"
          (optional_with_default 2500 int)
          ~doc:
            "the number of words considered as the hidden word (starting as \
             most frequent)"
      and player_name =
        flag "--player" (required string) ~doc:"the type of player"
      and hidden = flag "--hidden" (optional string) ~doc:"the hidden word" in
      let allowed_file = Filename.concat data_folder "allowed.txt" in
      let sampled_file = Filename.concat data_folder "sampled.txt" in
      let env =
        Environment.import ~allowed_file ~sampled_file ~word_length ~sampled_num
      in
      let hidden = Option.value hidden ~default:(Environment.sample env) in
      let player =
        match player_name with
        | "human" -> (module Human_play : Player.S)
        | "entropy" ->
            (module Player.With_printing (Entropy_splitter) : Player.S)
        | _ -> failwith "no such player"
      in
      fun () -> Player.play player env ~hidden)

let cmd =
  Command.group ~summary:"Wordle_solver"
    [ ("test", test_cmd); ("play", play_cmd) ]

let () = Command.run cmd
