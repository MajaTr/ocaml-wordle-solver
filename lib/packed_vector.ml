open! Core
open Packed_vector_intf

module type Packable = Packable

module Make (M : Packable) = struct
  type t = int [@@deriving compare, sexp]

  let len_size = 6

  let sl t i = t lsl ((M.bit_width * i) + len_size)

  let sr t i = t lsr ((M.bit_width * i) + len_size)

  let all_on = (1 lsl M.bit_width) - 1

  let get t i = sr t i land all_on |> M.of_int_exn

  let set t i x = t lxor (t land sl all_on i) lxor sl (M.to_int x) i

  (* probably slow but whatever *)
  let init n ~f =
    List.init n ~f |> List.foldi ~init:n ~f:(fun i t x -> set t i x)

  let length t = t land ((1 lsl len_size) - 1)

  let to_sequence t =
    Sequence.range ~start:`inclusive ~stop:`exclusive 0 (length t)
    |> Sequence.map ~f:(get t)

  let fold t ~init ~f = to_sequence t |> Sequence.fold ~init ~f

  let for_all t ~f = to_sequence t |> Sequence.for_all ~f

  let to_int t = t
end
