open! Core

module Game_state : sig
    type t =
    | First_moves
    | Game_continues
    | Game_over of { winner: Piece.t }
  end
       type t =
       {
         board_height:int;
         board_width:int;
         mutable board:Piece.t Position.Map.t;
         mutable game_state:Game_state.t;
         mutable piece_to_move:Piece.t
       }
       val make_move_exn : game:t -> Move.t -> unit
       val new_board : height:int -> width:int -> Piece.t Position.Map.t
       val new_game : height:int -> width:int -> t
       val print : t -> unit
       val possible_captures_from_occupied_pos_exn : t -> Position.t -> Move.t list
       val available_captures_for_player : t -> my_piece:Piece.t -> Move.t list
