open! Core

module Event = struct
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

let default () =
  { turns_since_event_and_event_opt = None
  ; obstacle_location_list = []
  ; monster_locations_list = []
  ; withered_pieces_list = []
  ; duplicating_pieces = false, 0
  }
;;
