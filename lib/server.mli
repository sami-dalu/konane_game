open! Core
open! Async

(* val handle_turn : unit -> Rpcs.Take_turn.Query.t ->
   Rpcs.Take_turn.Response.t Deferred.t *)

(* val start_game_impl : 'a -> Rpcs.Start_game.Query.t ->
   Rpcs.Start_game.Response.t Deferred.t *)
(* val handle_test_query : 'a -> Rpcs.Test.Query.t -> Rpcs.Test.Response.t
   Deferred.t *)

type t =
  { config_queue_tbl : (Game_config.t, Player.t Queue.t) Hashtbl.t
  ; game_player_piece_tbl : (Player.t, Game.t) Hashtbl.t
  }

(* val _handle_turn_query : 'a -> Rpcs.Test.Query.t -> Rpcs.Test.Response.t
   Deferred.t *)

val handle_start_query
  :  t
  -> 'a
  -> Rpcs.Start_game.Query.t
  -> Rpcs.Start_game.Response.t Deferred.t

val handle_move_query
  :  t
  -> 'a
  -> Rpcs.Take_turn.Query.t
  -> Rpcs.Take_turn.Response.t Deferred.t

val handle_wait_query
  :  t
  -> 'a
  -> Rpcs.Wait_turn.Query.t
  -> Game.t Deferred.t

val handle_end_query
  :  t
  -> 'a
  -> Rpcs.End_turn.Query.t
  -> Rpcs.End_turn.Response.t Deferred.t

val handle_restart_query : t -> 'a -> Player.t -> Game.t Deferred.t
