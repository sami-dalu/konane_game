open! Core

type t =
  { starting_pos : Position.t
  ; ending_pos : Position.t option
  }
[@@deriving sexp, equal, bin_io, compare]
