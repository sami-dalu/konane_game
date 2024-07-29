open! Core
open! Async

(* val handle_turn : unit -> Rpcs.Take_turn.Query.t ->
   Rpcs.Take_turn.Response.t Deferred.t *)

(* val start_game_impl : 'a -> Rpcs.Start_game.Query.t ->
   Rpcs.Start_game.Response.t Deferred.t *)
val handle_test_query
  :  'a
  -> Rpcs.Test.Query.t
  -> Rpcs.Test.Response.t Deferred.t
