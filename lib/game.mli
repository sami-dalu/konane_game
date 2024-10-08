open! Core

module Game_state : sig
  type t =
    | First_moves
    | Game_continues
    | Game_over of { winner : Piece.t }
  [@@deriving sexp, equal, bin_io]
end

type t =
  { mutable board_height : int
  ; mutable board_width : int
  ; mutable board : Piece.t Position.Map.t
  ; mutable game_state : Game_state.t
  ; mutable piece_to_move : Piece.t
  ; mutable last_move_from_piece_to_move : Move.t option
  ; bot_difficulty : Player.Difficulty.t option
  ; mutable player1 : Player.t option
  ; mutable player2 : Player.t option
  ; mutable last_move_played : Move.t option
  ; mutable crazy_info : Crazy_info.t option
  ; mutable inverse_board : bool
  }
[@@deriving sexp, bin_io]

val make_move_exn : game:t -> Move.t -> unit
val new_board : height:int -> width:int -> Piece.t Position.Map.t

val new_game
  :  ?bot_diff:Player.Difficulty.t
  -> height:int
  -> width:int
  -> unit
  -> t

val print : t -> unit
val restart : t -> unit
val in_board_range : Position.t -> t -> bool

val possible_captures_from_occupied_pos_exn
  :  ?dir_opt:Move.Direction.t
  -> t
  -> Position.t
  -> Move.t list

val available_captures_for_player : t -> my_piece:Piece.t -> Move.t list
val check_for_win : t -> unit
val evaluate : t -> Game_state.t
val rotate_game_cw : t -> unit
val flip_all_pieces : t -> unit
val place_obstacle : t -> unit
val wither_piece : t -> unit
val activate_duplicates : t -> unit
val decrement_and_prune_crazy_stuff : t -> unit
val spawn_monster : t -> unit
