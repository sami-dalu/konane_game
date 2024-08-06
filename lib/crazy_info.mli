open! Core

module Event : sig
  type t =
    | Eruption
    | Monster
    | Plague
    | Duplicates
    | Flip_all
    | Rotate
  [@@deriving sexp, bin_io, compare, enumerate]
end

type t =
  { mutable turns_since_event_and_event_opt : (int * Event.t) option
  ; mutable obstacle_location_list : (Position.t * int) list
  ; mutable monster_locations_list : (Position.t * int) list
  ; mutable withered_pieces_list : (Position.t * int) list
  ; mutable duplicating_pieces : bool * int
  }
[@@deriving sexp, bin_io, compare]

val default : unit -> t
