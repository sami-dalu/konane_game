open! Core

(** This module handles the graphics for the game. We have implemented this for
    you so you don't need to change anything here, but feel free to look around
    and once you have the game, feel free to alter this file to make things
    fancier! *)

module Action : sig
  type t =
    | Move of Move.t
    | Restart
    | End_turn
    | None
end

(** [init_exn] fails if called twice. *)
val init_exn : Game_config.t -> Game.t

(** [render] renders the entire playing area along with snakes and apples. *)
val render : Client.t -> unit

(** [read_key] returns a keyboard input, if it's available. *)
val read_key : Client.t -> Action.t

val display_win_message
  :  Piece.t
  -> Player.t
  -> board_height:int
  -> board_width:int
  -> unit
