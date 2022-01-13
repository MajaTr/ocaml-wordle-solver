open! Core

(* Assumption: the sampled words are in the front *)
type t = {
  allowed : (string, read) Array.Permissioned.t;
  to_handle : int String.Map.t;
  sampled_num : int;
  results : Guess_result.t Option_array.t array;
}

type env = t

module Word_handle = struct
  type t = int [@@deriving compare, hash, equal, sexp]

  let to_string t ~env:{ allowed; _ } = Array.Permissioned.get allowed t

  let guess_result ~guess ~hidden ~env:({ results; _ } as env) =
    match Option_array.get results.(guess) hidden with
    | Some result -> result
    | None ->
        let result =
          Guess_result.obtain ~guess:(to_string guess ~env)
            ~hidden:(to_string hidden ~env)
        in
        Option_array.set results.(guess) hidden (Some result);
        result
end

let import ~filename ~word_length ~sampled_num =
  let words_by_freq =
    Sexp.load_sexps filename
    |> List.map ~f:[%of_sexp: string * int]
    |> List.filter ~f:(fun (word, _) -> String.length word = word_length)
  in
  let allowed = List.map words_by_freq ~f:fst in
  let to_handle =
    List.mapi allowed ~f:(fun i s -> (s, i)) |> String.Map.of_alist_exn
  in
  let sampled_num = Option.value sampled_num ~default:(List.length allowed) in
  {
    allowed = Array.Permissioned.of_list allowed;
    to_handle;
    sampled_num;
    results =
      Array.init (List.length allowed) ~f:(fun _ ->
          Option_array.init sampled_num ~f:(fun _ -> None));
  }

let allowed_words { allowed; _ } =
  List.init (Array.Permissioned.length allowed) ~f:Fn.id

let get_handle { to_handle; _ } s = Map.find to_handle s

let sampled_words { sampled_num; _ } = List.init sampled_num ~f:Fn.id

let sample t = sampled_words t |> List.random_element_exn
