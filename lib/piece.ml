open! Core

module Piece = struct
  type t =
    | X
    | O
  [@@deriving sexp, equal, compare, bin_io, enumerate]

  let of_string = Fn.compose Sexp.t_of_sexp Sexp.of_string
  let to_string = Fn.compose Sexp.to_string_hum Sexp.sexp_of_t

  let flip = function
    | X -> O
    | O -> X
  ;;
end