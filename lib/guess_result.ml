open Core

module T = struct
  module Char_result = struct
    type t = None | Yellow | Green [@@deriving compare, sexp]

    let to_int = function None -> 1 | Yellow -> 2 | Green -> 3

    exception Out_of_bounds

    let of_int_exn = function
      | 1 -> None
      | 2 -> Yellow
      | 3 -> Green
      | _ -> raise Out_of_bounds

    let to_char = function None -> '_' | Yellow -> 'Y' | Green -> 'G'
  end

  module Packed_vector = struct
    type t = int [@@deriving compare, sexp]

    let get t i = (t lsr (2 * i)) land 3 |> Char_result.of_int_exn

    (*
  let set_from_to t i x y =
    let z = Char_result.to_int x lxor Char_result.to_int y in
    t lxor (z lsl (2 * i))*)

    let set t i x =
      let t1 = t land ((1 lsl (2 * i)) - 1) in
      let t2 = t lsr (2 * i) in
      let t3 = t2 lxor (t2 land 3) lxor Char_result.to_int x in
      (t3 lsl (2 * i)) lor t1

    let init n ~f =
      List.init n ~f |> List.foldi ~init:0 ~f:(fun i t x -> set t i x)

    let rec fold t ~init ~f =
      match t with
      | 0 -> init
      | _ ->
          fold (t lsr 2) ~init:(f init (t land 3 |> Char_result.of_int_exn)) ~f

    let for_all t ~f = fold t ~init:true ~f:(fun b x -> b && f x)
  end

  type t = Packed_vector.t [@@deriving compare, sexp]

  let obtain guess ~hidden =
    let open Packed_vector in
    let n = String.length guess in
    let e = init n ~f:(fun _ -> None) in
    let gv_green, hv_green =
      String.foldi guess ~init:(e, e) ~f:(fun i (gv, hv) gc ->
          let hc = hidden.[i] in
          if Char.(gc = hc) then (set gv i Green, set hv i Green) else (gv, hv))
    in
    let gv, _ =
      String.foldi guess ~init:(gv_green, hv_green) ~f:(fun i (gv, hv) gc ->
          match
            ( get gv i,
              String.lfindi hidden ~f:(fun j hc ->
                  Char.(gc = hc) && Poly.(get hv j = None)) )
          with
          | Green, _ | _, None -> (gv, hv)
          | (None | Yellow), Some j -> (set gv i Yellow, set hv j Yellow))
    in
    gv

  let guessed t = Packed_vector.for_all t ~f:(fun x -> Poly.(x = Green))

  let to_string t =
    Packed_vector.fold t ~init:"" ~f:(fun s x ->
        s ^ (Char_result.to_char x |> Char.to_string))

  let of_string s =
    let exception Wrong_letter in
    try
      Packed_vector.init (String.length s) ~f:(fun i ->
          match s.[i] with
          | '_' -> None
          | 'Y' -> Yellow
          | 'G' -> Green
          | _ -> raise Wrong_letter)
      |> Option.some
    with Wrong_letter -> None

  let to_int t = t
end

include T
include Comparable.Make (T)
