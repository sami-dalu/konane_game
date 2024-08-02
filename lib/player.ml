open! Core

module Difficulty = struct
  type t =
    | Easy
    | Medium
    | Hard
  [@@deriving sexp, equal, bin_io, compare, hash]
end

module Player_kind = struct
  type t =
    | Human of { name : string }
    | Bot of { difficulty : Difficulty.t }
  [@@deriving sexp, equal, bin_io, compare, hash]
end

module T = struct
  type t =
    { player_kind : Player_kind.t
    ; piece : Piece.t
    ; id : int
    }
  [@@deriving sexp, equal, bin_io, compare, hash]
end

include T
include Comparable.Make_binable (T)
include Hashable.Make_binable (T)

let init_human ~name ~piece =
  { T.player_kind = Player_kind.Human { name }
  ; piece
  ; id = Random.int 1000000
  }
;;

let init_bot ~piece ~difficulty =
  { T.player_kind = Player_kind.Bot { difficulty }
  ; piece
  ; id = Random.int 1000000
  }
;;

let get_piece t = t.piece

let get_name t =
  match t.player_kind with Human { name } -> name | Bot _ -> "BOT"
;;
