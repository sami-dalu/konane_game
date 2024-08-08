open! Core

type t =
  { row : int
  ; column : int
  }
[@@deriving sexp, equal, bin_io, compare, hash]

val to_string : t -> string

include Comparable.S_binable with type t := t
