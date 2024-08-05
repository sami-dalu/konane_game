open! Core

module Event = struct
  type t =
    | Eruption
    | Monster_loose
    | Plague
  [@@deriving sexp, bin_io, compare]
end

type t =
  { mutable turns_since_event : int
  ; mutable obstacle_location_list : (Position.t * int) list
  ; mutable monster_locations_list : (Position.t * int) list
  ; mutable withered_pieces_list : (Position.t * int) list
  ; mutable duplicating_pieces : bool
  }
[@@deriving sexp, bin_io, compare]

let default () =
  { turns_since_event = 0
  ; obstacle_location_list = []
  ; monster_locations_list = []
  ; withered_pieces_list = []
  ; duplicating_pieces = false
  }
;;
