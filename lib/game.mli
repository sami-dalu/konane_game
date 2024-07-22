open! Core

module Game_state : sig
    type t =
    | First_moves
    | Game_continues
    | Game_over of { winner: Piece.t option}
  end
       type t =
       {
         board_height:int;
         board_width:int;
         board:Piece.t Position.Map.t;
         game_state:Game_state.t
       }

       val new_board : height:int -> width:int -> Piece.t Position.Map.t
       val new_game : height:int -> width:int -> t
       val print : t -> unit