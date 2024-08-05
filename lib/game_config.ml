open! Core

module Game_mode = struct
  type t =
    | Normal
    | Crazy
  [@@deriving sexp, bin_io, compare, hash]

  let flip_all_pieces game = ()
  let place_obstacles game = ()
end

module T = struct
  type t =
    { height : int
    ; width : int
    ; mode : Game_mode.t
    }
  [@@deriving sexp, bin_io, compare, hash]
end

let default_8_by_8 () = { T.height = 8; width = 8; mode = Game_mode.Normal }

include T
include Comparable.Make_binable (T)
include Hashable.Make_binable (T)
