open! Core

type t =
  { name : string
  ; piece : Piece.t
  }
[@@deriving sexp, equal, bin_io, compare]

(* include T *)
(* include Comparable.Make_binable (T) *)
