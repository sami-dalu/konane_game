open! Core

module Direction : sig
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, equal, bin_io, compare]
end

type t =
  { starting_pos : Position.t
  ; ending_pos : Position.t option
  ; dir : Direction.t option
  }
[@@deriving sexp, equal, bin_io, compare]
