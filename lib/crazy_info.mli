open! Core

type t =
  { mutable turns_since_event : int
  ; mutable obstacle_location_list : (Position.t * int) list
  ; mutable monster_locations_list : (Position.t * int) list
  ; mutable withered_pieces_list : (Position.t * int) list
  ; mutable duplicating_pieces : bool
  }
[@@deriving sexp, bin_io, compare]

val default : unit -> t
