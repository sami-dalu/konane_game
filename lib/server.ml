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

type t =
  { player_queue : Player.t Queue.t
  ; game_player_piece_tbl : (Player.t, Game.t) Hashtbl.t
  }

(* let _handle_test_query _client (query : Rpcs.Test.Query.t) :
   Rpcs.Test.Response.t Deferred.t = return (query + 1) ;; *)

let handle_start_query (server : t) _client (query : Rpcs.Start_game.Query.t)
  : Rpcs.Start_game.Response.t Deferred.t
  =
  if Queue.is_empty server.player_queue
  then (
    let _ =
      Queue.enqueue
        server.player_queue
        (Player.init ~name:query.name ~piece:Piece.X)
    in
    return Rpcs.Start_game.Response.Game_not_started)
  else (
    (* there's someone in the queue already *)
    let existing_player = Queue.peek_exn server.player_queue in
    if Hashtbl.existsi server.game_player_piece_tbl ~f:(fun ~key ~data ->
         ignore data;
         String.equal (Player.get_name key) (Player.get_name existing_player))
    then return Rpcs.Start_game.Response.Game_not_started
    else (
      let g = Game.new_game ~height:8 ~width:8 in
      let _ =
        Hashtbl.add_exn
          server.game_player_piece_tbl
          ~key:existing_player
          ~data:g
      in
      let new_p = Player.init ~name:query.name ~piece:Piece.O in
      let _ =
        Hashtbl.add_exn server.game_player_piece_tbl ~key:new_p ~data:g
      in
      let response =
        Rpcs.Start_game.Response.Game_started { your_player = new_p }
      in
      return response))
;;
