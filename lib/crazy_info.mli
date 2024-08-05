open! Core

type t =
  { turns_since_event : int
  ; obstacle_location_list : Position.t list
  ; monster_locations_list : Position.t list
  ; withered_pieces_list : (Position.t * int) list
  ; duplicating_pieces : bool
  }
[@@deriving sexp, bin_io, compare, hash]

val default : unit -> t
