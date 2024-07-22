open! Core

  module T = struct
    type t =
      { row    : int
      ; column : int
      }
    [@@deriving sexp, equal, bin_io, compare]
  end

  let in_board_range {Position.row = r; column = c} (game : Game.t ) = 
  r >= 0 && c>= 0 && r < game.board_height && c < game.board_width

  include T
  include Comparable.Make_binable (T)
  let to_string = Fn.compose Sexp.to_string_hum sexp_of_t
