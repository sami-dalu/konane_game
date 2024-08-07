open! Core

module Event = struct
  type t =
    | Eruption
    | Monster
    | Plague
    | Duplicates
    | Flip_all
    | Rotate
    | Impending_start
  [@@deriving sexp, bin_io, compare, equal]
end

type t =
  { mutable turns_since_event_and_event : int * Event.t
  ; mutable obstacle_location_list : (Position.t * int) list
  ; mutable monster_locations_list : (Position.t * int) list
  ; mutable withered_pieces_list : (Position.t * int) list
  ; mutable duplicating_pieces : bool * int
  }
[@@deriving sexp, bin_io, compare]

let default () =
  { turns_since_event_and_event = 0, Event.Impending_start
  ; obstacle_location_list = []
  ; monster_locations_list = []
  ; withered_pieces_list = []
  ; duplicating_pieces = false, 0
  }
;;
