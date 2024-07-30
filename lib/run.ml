open! Core
open! Async

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)
let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else (
      let%bind () = Clock.after (Time_float.Span.of_sec seconds) in
      let%bind () = f () in
      loop ())
  in
  don't_wait_for (loop ())
;;

let handle_keys (game : Game.t ref) ~game_over host port player =
  every ~stop:game_over 0.001 ~f:(fun () ->
    Game_graphics.render !game;
    match Game_graphics.read_key !game with
    | None -> Deferred.return ()
    (* | Some 'r' -> Game.restart ~height:600 ~width:675
       ~initial_snake_length:2 *)
    | Move move ->
      let make_move_query = { Rpcs.Take_turn.Query.player; move } in
      let%bind make_move_response =
        Rpc.Connection.with_client
          (Tcp.Where_to_connect.of_host_and_port
             { Host_and_port.port; Host_and_port.host })
          (fun conn ->
            Rpc.Rpc.dispatch_exn Rpcs.Take_turn.rpc conn make_move_query)
      in
      (match make_move_response with
       | Error _ -> print_string "error start"
       | Ok response ->
         let new_game = response.game in
         game := new_game);
      Deferred.return ()
      (* Game.make_move_exn ~game move; (* Game_graphics.render game; *)
         (match move.ending_pos with | None -> game.piece_to_move <-
         Piece.flip game.piece_to_move | Some pos -> let possible_moves =
         Game.possible_captures_from_occupied_pos_exn ?dir_opt:move.dir game
         pos in if List.length possible_moves = 0 then ( game.piece_to_move
         <- Piece.flip game.piece_to_move; game.last_move_from_piece_to_move
         <- None) else game.last_move_from_piece_to_move <- Some move) *)
      (* Game_graphics.render game *)
    | Restart ->
      Game.restart !game;
      Deferred.return ()
    | End_turn ->
      (* flip the piece on the server side and set
         last_move_from_piece_to_move to None *)
      let end_turn_query = { Rpcs.End_turn.Query.player } in
      let%bind end_turn_response =
        Rpc.Connection.with_client
          (Tcp.Where_to_connect.of_host_and_port
             { Host_and_port.port; Host_and_port.host })
          (fun conn ->
            Rpc.Rpc.dispatch_exn Rpcs.End_turn.rpc conn end_turn_query)
      in
      (match end_turn_response with
       | Error _ -> print_string "error end"
       | Ok response ->
         let new_game = response.game in
         game := new_game);
      Deferred.return ())
;;

let handle_steps (game : Game.t ref) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
    Game.check_for_win !game;
    match !game.game_state with
    | Game_over { winner } ->
      game_over := true;
      print_endline (Piece.to_string winner ^ " WINSSSSSS!!!!!");
      Deferred.return ()
    | _ -> Deferred.return ())
;;

(* let handle_restart (game : Game.t) ~game_over = every ~stop:(ref (not
   !game_over)) 0.01 ~f:(fun () -> match Game_graphics.read_key game with |
   Restart -> Game.restart game; game_over := false; print_endline "test" | _
   -> ()) ;; *)

let run host port who_am_i =
  (* let query = { Rpcs.Start_game.Query.name ; host_and_port = {
     Host_and_port.host; port } } in let%bind start_game_response =
     Rpc.Connection.with_client (Tcp.Where_to_connect.of_host_and_port {
     Host_and_port.port; Host_and_port.host = "localhost" }) (fun conn ->
     Rpc.Rpc.dispatch_exn Demo1.Rpcs.Test.rpc conn num) in *)
  let game = Game_graphics.init_exn () in
  Game_graphics.render game;
  let game_over = ref false in
  let game = ref (Game.new_game ~height:8 ~width:8) in
  handle_keys game ~game_over host port who_am_i;
  handle_steps game ~game_over
;;
(* handle_restart game ~game_over *)
