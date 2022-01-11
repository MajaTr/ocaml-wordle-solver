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
      flag "--sampled-num" (optional int)
        ~doc:
          "the number of words considered as the hidden word (starting as most \
           frequent)"
    in
    let env_words = Filename.concat data_folder "env_words.txt" in
    Environment.import ~filename:env_words ~word_length ~sampled_num)

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
        print_endline "ok";
        let tree = Game_tree.make player env in
        Game_tree.sexp_of_t tree |> Sexp.save out_file)

let cheat_cmd =
  Command.basic ~summary:"Cheat in wordle with a computer player"
    Command.Let_syntax.(
      let%map_open env = env_parser
      and player_name =
        flag "--player" (required string) ~doc:"the type of player"
      in
      fun () ->
        let player =
          match player_name with
          | "entropy" -> (module Entropy_splitter : Player.S)
          | _ -> Option.value_exn (load_sexp_player player_name)
        in

        Player.cheat player env)

let adhoc_cmd =
  Command.basic ~summary:"Ad-hoc command to save the word list in a good format"
    Command.Let_syntax.(
      let%map_open allowed =
        flag "--allowed" (required Filename.arg_type) ~doc:"allowed words file"
      and sampled =
        flag "--sampled" (required Filename.arg_type) ~doc:"sampled words file"
      and out = flag "-o" (required Filename.arg_type) ~doc:"output" in
      fun () -> Wordlist_input.adhoc_compose ~allowed ~sampled ~out)

module Depths_data = struct
  type t = { by_depth : string list Int.Map.t }

  let print { by_depth } =
    Map.to_alist by_depth ~key_order:`Increasing
    |> List.iter ~f:(fun (d, ws) ->
           printf "%5d %5d " d (List.length ws);
           print_s [%sexp (List.take ws 5 : string list)];
           printf "\n")

  let of_depths depths =
    let by_depth =
      Map.fold depths ~init:Int.Map.empty ~f:(fun ~key ~data ->
          Int.Map.add_multi ~key:data ~data:key)
    in
    { by_depth }
end

let tree_depths_cmd =
  Command.basic ~summary:"On tree depths"
    Command.Let_syntax.(
      let%map_open filename = anon ("tree sexp file" %: Filename.arg_type) in
      fun () ->
        Sexp.load_sexp filename |> Game_tree.t_of_sexp |> Game_tree.depths
        |> Depths_data.of_depths |> Depths_data.print)

let tree_stats_cmd =
  Command.group ~summary:"Output stats about the game tree"
    [ ("depths", tree_depths_cmd) ]

let cmd =
  Command.group ~summary:"Wordle_solver"
    [
      ("test", test_cmd);
      ("play", play_cmd);
      ("make-tree", make_tree_cmd);
      ("cheat", cheat_cmd);
      ("adhoc", adhoc_cmd);
      ("tree-stats", tree_stats_cmd);
    ]

let () = Command.run cmd
