open! Core

module Action : sig
  type t =
    | Move of Move.t
    | Restart
    | End_turn
    | None
end

val init_exn : Game_config.t -> Game.t
val render : Client.t -> unit
val read_key : Client.t -> Action.t

val display_win_message
  :  Piece.t
  -> Player.t
  -> board_height:int
  -> board_width:int
  -> player1:Player.t option
  -> player2:Player.t option
  -> unit
