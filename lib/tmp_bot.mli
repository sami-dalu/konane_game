open! Core

val use_minimax_to_find_best_moves
  :  Game.t
  -> depth:int
  -> me:Piece.t
  -> Move.t list
