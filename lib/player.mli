open! Core

type t =
  { name : string
  ; piece : Piece.t
  }
[@@deriving sexp, equal, bin_io, compare, hash]

include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t
