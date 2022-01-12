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
    let%map_open env_words =
      flag "--env-words"
        (optional_with_default "data/env_words.txt" Filename.arg_type)
        ~doc:"FILENAME word list file"
    and word_length =
      flag "--word-length"
        (optional_with_default 5 int)
        ~doc:"INT word length for the game"
    and sampled_num =
      flag "--sampled-num" (optional int)
        ~doc:
          "INT the number of words considered as the hidden word (starting as \
           most frequent)"
    in
    Environment.import ~filename:env_words ~word_length ~sampled_num)

module Player_type = struct
  open Option.Let_syntax

  module Split = struct
    type t = Entropy | Max | Maxlex

    let of_string = function
      | "entropy" -> Some Entropy
      | "max" -> Some Max
      | "maxlex" -> Some Maxlex
      | _ -> None

    let eval : t -> (module Splitter.S) = function
      | Entropy -> (module Entropy_split)
      | Max -> (module Max_split)
      | Maxlex -> (module Max_lex_split)
  end

  module Auto = struct
    type t = Split of Split.t

    let of_string s =
      match%bind String.lsplit2 s ~on:'-' with
      | "split", tail ->
          let%map split = Split.of_string tail in
          Split split
      | _, _ -> None

    let eval = function
      | Split split ->
          (module Splitter.Make ((val Split.eval split)) : Player.S)
  end

  type t = Human | Auto of Auto.t | Sexp of Filename.t

  let of_string s =
    List.fold ~init:None ~f:Option.first_some
      [
        (match s with "human" -> Some Human | _ -> None);
        (match%bind String.lsplit2 s ~on:'-' with
        | "auto", tail ->
            let%map auto = Auto.of_string tail in
            Auto auto
        | _ -> None);
        Option.some_if (String.is_suffix s ~suffix:".sexp") (Sexp s);
      ]

  let of_string_exn s = of_string s |> Option.value_exn

  let arg_type = Command.Arg_type.create of_string_exn

  let eval ?(with_printing = false) s =
    let apply_printing (module M : Player.S) =
      if with_printing then (module Player.With_printing (M) : Player.S)
      else (module M)
    in
    match s with
    | Human -> (module Human_play : Player.S)
    | Auto auto -> Auto.eval auto |> apply_printing
    | Sexp filename ->
        Sexp.load_sexp filename |> Game_tree.t_of_sexp |> Game_tree.to_player
        |> apply_printing
end

let play_cmd =
  Command.basic ~summary:"Let the computer play"
    Command.Let_syntax.(
      let%map_open env = env_parser
      and player_type =
        flag "--player"
          (required Player_type.arg_type)
          ~doc:"PLAYER the type of player"
      and hidden =
        flag "--hidden" (optional string) ~doc:"WORD the hidden word"
      in
      fun () ->
        let hidden =
          Option.(
            hidden >>= Environment.get_handle env
            |> value ~default:(Environment.sample env))
        in
        Player.play
          (Player_type.eval ~with_printing:true player_type)
          env ~hidden)

let make_tree_cmd =
  Command.basic ~summary:"Save a game tree"
    Command.Let_syntax.(
      let%map_open env = env_parser
      and player_type =
        flag "--player"
          (required Player_type.arg_type)
          ~doc:"PLAYER the type of player"
      and out_file =
        flag "-o" (required Filename.arg_type) ~doc:"FILENAME output file"
      in
      fun () ->
        let tree = Game_tree.make (Player_type.eval player_type) env in
        Game_tree.sexp_of_t tree |> Sexp.save out_file)

let cheat_cmd =
  Command.basic ~summary:"Cheat in wordle with a computer player"
    Command.Let_syntax.(
      let%map_open env = env_parser
      and player_type =
        flag "--player"
          (required Player_type.arg_type)
          ~doc:"PLAYER the type of player"
      in
      fun () -> Player.cheat (Player_type.eval player_type) env)

let adhoc_cmd =
  Command.basic ~summary:"Ad-hoc command to save the word list in a good format"
    Command.Let_syntax.(
      let%map_open allowed =
        flag "--allowed"
          (required Filename.arg_type)
          ~doc:"FILENAME allowed words file"
      and sampled =
        flag "--sampled"
          (required Filename.arg_type)
          ~doc:"FILENAME sampled words file"
      and out = flag "-o" (required Filename.arg_type) ~doc:"FILENAME output" in
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

let tree_pp_cmd =
  Command.basic ~summary:"Pretty print the tree"
    Command.Let_syntax.(
      let%map_open filename = anon ("tree sexp file" %: Filename.arg_type) in
      fun () ->
        Sexp.load_sexp filename |> Sexp.pp_hum Format.std_formatter;
        Format.pp_print_flush Format.std_formatter ())

let tree_view_cmd =
  Command.group ~summary:"View data about the game tree"
    [ ("depths", tree_depths_cmd); ("pp", tree_pp_cmd) ]

let cmd =
  Command.group ~summary:"Wordle_solver"
    [
      ("test", test_cmd);
      ("play", play_cmd);
      ("make-tree", make_tree_cmd);
      ("cheat", cheat_cmd);
      ("adhoc", adhoc_cmd);
      ("tree-view", tree_view_cmd);
    ]

let () = Command.run cmd
