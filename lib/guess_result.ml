open Core

module T = struct
  module Char_result = struct
    type t = None | Yellow | Green [@@deriving compare, equal]

    let to_int = function None -> 1 | Yellow -> 2 | Green -> 3

    exception Out_of_bounds

    let of_int_exn = function
      | 1 -> None
      | 2 -> Yellow
      | 3 -> Green
      | _ -> raise Out_of_bounds

    let bit_width = 2

    let to_char = function None -> '_' | Yellow -> 'Y' | Green -> 'G'
  end

  module Vector = Packed_vector.Make (Char_result)

  type t = Vector.t [@@deriving compare]

  let obtain ~guess ~hidden =
    let open Vector in
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

  let guessed t = Vector.for_all t ~f:(fun x -> Poly.(x = Green))

  let to_string t =
    Vector.fold t ~init:"" ~f:(fun s x ->
        s ^ (Char_result.to_char x |> Char.to_string))

  let of_string s =
    let exception Wrong_letter in
    try
      Vector.init (String.length s) ~f:(fun i ->
          match s.[i] with
          | '_' -> None
          | 'Y' -> Yellow
          | 'G' -> Green
          | _ -> raise Wrong_letter)
      |> Option.some
    with Wrong_letter -> None

  let to_int t = Vector.to_int t

  let sexp_of_t t = to_string t |> String.sexp_of_t

  let t_of_sexp s = String.t_of_sexp s |> of_string |> Option.value_exn
end

include T
include Comparable.Make (T)
