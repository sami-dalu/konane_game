open! Core
open! Async

(* let handle_turn (_client : unit) (query : Rpcs.Take_turn.Query.t) = let
   response = { Rpcs.Take_turn.Response.piece = query.you_play ; move = {
   starting_pos = { Position.row = 0; column = 0 } ; ending_pos = None ; dir
   = None } } in return response ;; *)

(* let start_game_impl _client (_query : Rpcs.Start_game.Query.t) = *)
(* print_s [%message "Query received" (query : Rpcs.Start_game.Query.t)]; let
   game_over_query = { Rpcs.Game_over.Query.game = Game.new_game ~height:8
   ~width:8 ; Rpcs.Game_over.Query.evaluation = Game.Game_state.Game_over {
   winner = Piece.X } } in let%bind start_game_response =
   Rpc.Connection.with_client (Tcp.Where_to_connect.of_host_and_port
   query.host_and_port) (fun conn -> Rpc.Rpc.dispatch_exn Rpcs.Game_over.rpc
   conn game_over_query) in print_s [%message "start_game_response"
   (start_game_response : (Rpcs.Game_over.Response.t, exn) Result.t)]; *)
(* return Rpcs.Start_game.Response.Game_not_started *)

(* function which tries to start a test game *)
(* let start_test_impl _client (query : Rpcs.Start_game.Query.t) = let ;; *)

let handle_test_query _client (query : Rpcs.Test.Query.t)
  : Rpcs.Test.Response.t Deferred.t
  =
  return (query + 1)
;;
