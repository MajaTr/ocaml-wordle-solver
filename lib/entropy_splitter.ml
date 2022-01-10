open! Core

type state = { sampled : string list; allowed : string list }

let entropy sizes =
  let total = List.sum (module Int) sizes ~f:Fn.id |> Float.of_int in
  let ps = List.map sizes ~f:(fun s -> Float.of_int s /. total) in
  List.sum (module Float) ps ~f:(fun p -> -.p *. log p)

let counts xs =
  let a = Hashtbl.create (module Int) in
  List.iter xs ~f:(Hashtbl.incr a);
  Hashtbl.data a |> List.filter ~f:(fun x -> x <> 0)

let init env =
  {
    sampled = Environment.sampled_words env;
    allowed = Environment.allowed_words env;
  }

let guess { sampled; allowed } =
  match sampled with
  | [ answer ] -> answer
  | _ ->
      let _, guess =
        List.map allowed ~f:(fun guess ->
            ( List.map sampled ~f:(fun x ->
                  Guess_result.obtain guess ~hidden:x |> Guess_result.to_int)
              |> counts |> entropy,
              guess ))
        |> List.max_elt ~compare:(fun (x, _) (y, _) -> Float.compare x y)
        |> Option.value_exn
      in
      guess

let update { sampled; allowed } ~guess ~result =
  (* print_s [%message (result : Guess_result.t) (sampled : string list)];*)
  {
    sampled =
      List.filter sampled ~f:(fun w ->
          Guess_result.compare result (Guess_result.obtain guess ~hidden:w) = 0);
    allowed;
  }
