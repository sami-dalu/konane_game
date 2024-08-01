open! Core

type t =
  { mutable game : Game.t
  ; player : Player.t
  ; mutable moves_to_highlight : Move.t list
  }
