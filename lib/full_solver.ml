open! Core

module Bit = struct
  type t = int

  let to_int t = t

  let of_int_exn t = t

  let bit_width = 1
end

module Sampled_repr = struct
  include Packed_vector.Make (Bit)

  let solution t =
    match popcount t with
    | 1 ->
        let seq = to_sequence t in
        Sequence.findi seq ~f:(fun _ a -> Int.(a = 1)) |> Option.map ~f:fst
    | _ -> None
end

module Id : sig
  type t [@@deriving hash, sexp, compare, equal]

  include Packed_vector.Packable with type t := t

  val zero : t

  val new_id :
    (module Hashtbl_intf.Key with type t = 'a) -> 'a option -> unit -> 'a -> t
end = struct
  type t = int [@@deriving hash, sexp, compare, equal]

  let to_int t = t

  let of_int_exn t = t

  let bit_width = 4

  let zero = 0

  let new_id (type a) (module M : Hashtbl_intf.Key with type t = a) zero_val ()
      =
    let tbl = Hashtbl.create (module M) in
    fun x ->
      if Option.exists zero_val ~f:(fun zero_val -> M.(compare x zero_val = 0))
      then 0
      else
        Hashtbl.find_or_add tbl x ~default:(fun () ->
            1 + Hashtbl.length tbl |> of_int_exn)
end

type is_all_equal = Empty | Yes of Id.t | No

let update_all_equal x y =
  match x with
  | Empty -> Yes y
  | Yes y' -> if Id.(equal y y') then Yes y' else No
  | No -> No

module Move_repr = struct
  module Vector = Packed_vector.Make (Id)

  type t = { splits : Vector.t; guess : Environment.Word_handle.t list }
  [@@deriving sexp]

  let has_info { splits; _ } =
    let s, c =
      Vector.to_sequence splits
      |> Sequence.fold ~init:(Empty, 0) ~f:(fun (s, c) x ->
             if Id.(equal x zero) then (s, c) else (update_all_equal s x, c + 1))
    in
    match s with
    | Empty -> failwith "not supposed to happen"
    | Yes _ -> c = 1
    | No -> true
end

module Local_env = struct
  type t = {
    words : (Environment.Word_handle.t, read) Array.Permissioned.t;
    rev_map : (Environment.Word_handle.t, int) Hashtbl.t;
    memo : (Sampled_repr.t, int * Move_repr.t) Hashtbl.t;
  }

  let get_word { words; _ } i = Array.Permissioned.get words i
end

type state = {
  sampled : Sampled_repr.t;
  moves : Move_repr.t list;
  local_env : Local_env.t;
}

type adv_state = { state : state; move : Move_repr.t }

let empty sampled =
  Sampled_repr.init (Sampled_repr.length sampled) ~f:(fun _ -> 0)

let clip sampled splits =
  let new_id = Id.new_id (module Id) (Some Id.zero) () in
  let splits =
    Sampled_repr.to_sequence sampled
    |> Sequence.foldi ~init:splits ~f:(fun i move b ->
           if b = 0 then Move_repr.Vector.set move i (Id.of_int_exn 0) else move)
    |> Move_repr.Vector.to_sequence
    |> Sequence.foldi
         ~init:
           (Move_repr.Vector.length splits
           |> Move_repr.Vector.init ~f:(fun _ -> Id.of_int_exn 0))
         ~f:(fun i move id -> Move_repr.Vector.set move i (new_id id))
  in
  splits

let dedup sampled { Local_env.rev_map; _ } moves =
  let is_in word =
    Hashtbl.find rev_map word
    |> Option.value_map
         ~f:(fun i -> Sampled_repr.get sampled i = 1)
         ~default:false
  in
  let answer =
    List.filter_map moves ~f:(fun ({ Move_repr.splits; guess } as move) ->
        Option.some_if (Move_repr.has_info move) (splits, guess))
    |> Move_repr.Vector.Map.of_alist_reduce ~f:(fun x y ->
           match List.filter ~f:is_in x @ List.filter ~f:is_in y with
           | [] -> List.take x 1
           | z -> z)
    |> Map.to_alist
    |> List.map ~f:(fun (splits, guess) -> { Move_repr.splits; guess })
  in
  answer

let split { sampled; moves; local_env } ~move:{ Move_repr.splits; _ } =
  let tbl = Hashtbl.create (module Id) in
  Move_repr.Vector.to_sequence splits
  |> Sequence.iteri ~f:(fun i id ->
         if Id.(equal id zero) then ()
         else
           Hashtbl.update tbl id
             ~f:(Option.value_map ~f:(fun l -> i :: l) ~default:[ i ]));
  let news =
    Hashtbl.to_alist tbl
    |> List.map ~f:(fun (id, l) ->
           ( id,
             List.fold l ~init:(empty sampled) ~f:(fun s i ->
                 Sampled_repr.set s i 1) ))
  in
  List.map news ~f:(fun (id, new_sampled) ->
      let new_moves =
        List.map moves ~f:(fun ({ splits; _ } as move) ->
            { move with splits = clip new_sampled splits })
        |> dedup new_sampled local_env
      in
      ({ sampled = new_sampled; moves = new_moves; local_env }, id))

let rec solve ({ sampled; moves; local_env } as state) =
  Hashtbl.find_or_add local_env.memo sampled ~default:(fun () ->
      print_s [%message (sampled : Sampled_repr.t)];
      match Sampled_repr.solution sampled with
      | Some i ->
          let w = Local_env.get_word local_env i in
          ( 1,
            List.find_exn moves ~f:(fun { guess; _ } ->
                List.exists guess ~f:(Environment.Word_handle.equal w)) )
      | None ->
          let min, move =
            List.map moves ~f:(fun move ->
                let count, _ = adv_solve { state; move } in
                (count, move))
            |> List.min_elt ~compare:(fun (x, _) (y, _) -> Int.compare x y)
            |> Option.value_exn
          in
          (min + 1, move))

and adv_solve { state; move } =
  split state ~move
  |> List.map ~f:(fun (state', id) ->
         let i, _ = solve state' in
         (i, id))
  |> List.max_elt ~compare:(fun (x, _) (y, _) -> Int.compare x y)
  |> Option.value_exn

let init ~env =
  let sampled =
    Environment.sampled_words env
    |> List.length
    |> Sampled_repr.init ~f:(fun _ -> 1)
  in
  let words = Environment.sampled_words env |> Array.Permissioned.of_list in
  let rev_map =
    Environment.sampled_words env
    |> List.mapi ~f:(fun i w -> (w, i))
    |> Hashtbl.of_alist_exn (module Environment.Word_handle)
  in
  let local_env =
    { Local_env.words; rev_map; memo = Hashtbl.create (module Sampled_repr) }
  in
  let moves =
    Environment.allowed_words env
    |> List.map ~f:(fun guess ->
           let new_id = Id.new_id (module Guess_result) None () in
           let splits =
             Environment.sampled_words env
             |> List.map ~f:(fun hidden ->
                    Environment.Word_handle.guess_result ~env ~guess ~hidden)
             |> List.map ~f:new_id |> Move_repr.Vector.of_list
           in
           { Move_repr.splits; guess = [ guess ] })
    |> dedup sampled local_env
  in
  { sampled; moves; local_env }

let guess ~env:_ state =
  let _, move = solve state in
  (List.hd_exn move.guess, { state; move })

let update ~env { state; move } ~result =
  let id =
    Move_repr.Vector.to_sequence move.splits
    |> Sequence.findi ~f:(fun i id ->
           Id.to_int id <> 0
           && List.exists move.guess ~f:(fun g ->
                  Guess_result.( = )
                    (Environment.Word_handle.guess_result ~env ~guess:g
                       ~hidden:(Local_env.get_word state.local_env i))
                    result))
    |> Option.value_exn |> snd
  in
  split state ~move
  |> List.find_map_exn ~f:(fun (state', id') ->
         Option.some_if Id.(equal id id') state')
