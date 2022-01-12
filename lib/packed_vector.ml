open! Core
open Packed_vector_intf

module type Packable = Packable

module Make (M : Packable) = struct
  type t = int [@@deriving compare, sexp]

  let sl t i = t lsl (M.bit_width * i)

  let sr t i = t lsr (M.bit_width * i)

  let all_on = (1 lsl M.bit_width) - 1

  let get t i = sr t i land all_on |> M.of_int_exn

  let set t i x = t lxor (t land sl all_on i) lxor sl (M.to_int x) i

  let init n ~f =
    List.init n ~f |> List.foldi ~init:0 ~f:(fun i t x -> set t i x)

  let rec fold t ~init ~f =
    match t with
    | 0 -> init
    | _ -> fold (sr t 1) ~init:(t land all_on |> M.of_int_exn |> f init) ~f

  let for_all t ~f = fold t ~init:true ~f:(fun b x -> b && f x)

  let to_int t = t
end
