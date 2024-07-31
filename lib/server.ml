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
    let first_player = Player.init ~name:query.name ~piece:Piece.X in
    let _ = Queue.enqueue server.player_queue first_player in
    return
      (Rpcs.Start_game.Response.Game_not_started
         { your_player = first_player }))
  else (
    (* there's someone in the queue already *)
    let existing_player = Queue.dequeue_exn server.player_queue in
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
    return response)
;;

let handle_move_query (server : t) _client (query : Rpcs.Take_turn.Query.t) =
  match Hashtbl.find server.game_player_piece_tbl query.player with
  | None ->
    print_string "You're trying to make a move but not in the game.";
    return Rpcs.Take_turn.Response.Failure
  | Some game ->
    let move = query.move in
    Game.make_move_exn ~game move;
    (* Game_graphics.render game; *)
    (match move.ending_pos with
     | None -> game.piece_to_move <- Piece.flip game.piece_to_move
     | Some pos ->
       let possible_moves =
         Game.possible_captures_from_occupied_pos_exn
           ?dir_opt:move.dir
           game
           pos
       in
       if List.length possible_moves = 0
       then (
         game.piece_to_move <- Piece.flip game.piece_to_move;
         game.last_move_from_piece_to_move <- None)
       else game.last_move_from_piece_to_move <- Some move);
    return (Rpcs.Take_turn.Response.Success { game })
;;

let handle_wait_query (server : t) _client (query : Rpcs.Wait_turn.Query.t) =
  let g = Hashtbl.find_exn server.game_player_piece_tbl query in
  return g
;;

let handle_end_query (server : t) _client (query : Rpcs.End_turn.Query.t) =
  let g = Hashtbl.find_exn server.game_player_piece_tbl query.player in
  (* let new_g = { g with piece_to_move = Piece.flip g.piece_to_move ;
     last_move_from_piece_to_move = None } in *)
  g.piece_to_move <- Piece.flip g.piece_to_move;
  g.last_move_from_piece_to_move <- None;
  return { Rpcs.End_turn.Response.game = g }
;;

let handle_restart_query
  (server : t)
  _client
  (query : Rpcs.Restart_game.Query.t)
  =
  let g = Hashtbl.find_exn server.game_player_piece_tbl query in
  Game.restart g;
  return g
;;
