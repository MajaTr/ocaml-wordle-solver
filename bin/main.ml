open Core
open Wordle_solver

let test words =
  List.filter ~f:(fun x -> String.length x = 5) words |> List.length

let test_cmd =
  Command.basic ~summary:"Test"
    Command.Let_syntax.(
      let%map_open file = anon ("filename" %: Filename.arg_type) in
      fun () -> printf "%d\n" (Wordlist_input.read_words file |> test))

let env_parser =
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
          "the number of words considered as the hidden word (starting as most \
           frequent)"
    in
    let allowed_file = Filename.concat data_folder "allowed.txt" in
    let sampled_file = Filename.concat data_folder "sampled.txt" in
    Environment.import ~allowed_file ~sampled_file ~word_length ~sampled_num)

let load_sexp_player filename =
  if String.is_suffix filename ~suffix:".sexp" then
    Sexp.load_sexp filename |> Game_tree.t_of_sexp |> Game_tree.to_player
    |> Option.some
  else None

let play_cmd =
  Command.basic ~summary:"Let the computer play"
    Command.Let_syntax.(
      let%map_open env = env_parser
      and player_name =
        flag "--player" (required string) ~doc:"the type of player"
      and hidden = flag "--hidden" (optional string) ~doc:"the hidden word" in
      fun () ->
        let hidden = Option.value hidden ~default:(Environment.sample env) in
        let player =
          match player_name with
          | "human" -> (module Human_play : Player.S)
          | "entropy" ->
              (module Player.With_printing (Entropy_splitter) : Player.S)
          | _ ->
              let player = Option.value_exn (load_sexp_player player_name) in
              (module Player.With_printing ((val player : Player.S)) : Player.S)
        in

        Player.play player env ~hidden)

let make_tree_cmd =
  Command.basic ~summary:"Save a game tree"
    Command.Let_syntax.(
      let%map_open env = env_parser
      and player_name =
        flag "--player" (required string) ~doc:"the type of player"
      and out_file =
        flag "-o" (required Filename.arg_type) ~doc:"output file"
      in
      fun () ->
        let player =
          match player_name with
          | "entropy" -> (module Entropy_splitter : Player.S)
          | _ -> failwith "no such player"
        in
        let tree = Game_tree.make player env in
        Game_tree.sexp_of_t tree |> Sexp.save out_file)

let cmd =
  Command.group ~summary:"Wordle_solver"
    [ ("test", test_cmd); ("play", play_cmd); ("make-tree", make_tree_cmd) ]

let () = Command.run cmd
