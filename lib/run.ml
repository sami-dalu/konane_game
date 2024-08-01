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

let handle_keys (client_state : Client.t) ~game_over host port =
  every ~stop:game_over 0.0001 ~f:(fun () ->
    (* send query to server to get game state and update the game_ref *)
    let wait_turn_query = client_state.player in
    let%bind wait_turn_response =
      Rpc.Connection.with_client
        (Tcp.Where_to_connect.of_host_and_port
           { Host_and_port.port; Host_and_port.host })
        (fun conn ->
          Rpc.Rpc.dispatch_exn Rpcs.Wait_turn.rpc conn wait_turn_query)
    in
    (match wait_turn_response with
     | Error _ -> print_string "error wait"
     | Ok response -> client_state.game <- response);
    Game_graphics.render client_state;
    match Game_graphics.read_key client_state with
    | None -> Deferred.return ()
    (* | Some 'r' -> Game.restart ~height:600 ~width:675
       ~initial_snake_length:2 *)
    | Move move ->
      Core.print_s (Move.sexp_of_t move);
      if Piece.equal
           client_state.game.piece_to_move
           (Player.get_piece client_state.player)
      then (
        let make_move_query =
          { Rpcs.Take_turn.Query.player = client_state.player; move }
        in
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
           (match response with
            | Success { game = new_game } -> client_state.game <- new_game
            | Failure -> ()));
        Deferred.return ())
      else Deferred.return ()
    | Restart ->
      let restart_query = client_state.player in
      let%bind restart_response =
        Rpc.Connection.with_client
          (Tcp.Where_to_connect.of_host_and_port
             { Host_and_port.port; Host_and_port.host })
          (fun conn ->
            Rpc.Rpc.dispatch_exn Rpcs.Restart_game.rpc conn restart_query)
      in
      (match restart_response with
       | Error _ -> print_string "error start"
       | Ok response ->
         client_state.moves_to_highlight <- [];
         client_state.game <- response);
      Deferred.return ()
    | End_turn ->
      (* flip the piece on the server side and set
         last_move_from_piece_to_move to None *)
      if Piece.equal
           client_state.game.piece_to_move
           (Player.get_piece client_state.player)
      then (
        print_endline "got end turn";
        let end_turn_query =
          { Rpcs.End_turn.Query.player = client_state.player }
        in
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
           print_s (Piece.sexp_of_t new_game.piece_to_move);
           client_state.moves_to_highlight <- [];
           client_state.game <- new_game);
        Deferred.return ())
      else Deferred.return ())
;;

let handle_steps (client_state : Client.t) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
    Game.check_for_win client_state.game;
    match client_state.game.game_state with
    | Game_over { winner } ->
      game_over := true;
      print_endline (Piece.to_string winner ^ " WINSSSSSS!!!!!");
      Game_graphics.render client_state;
      Deferred.return ()
    | _ -> Deferred.return ())
;;

(* let handle_restart (game : Game.t) ~game_over = every ~stop:(ref (not
   !game_over)) 0.01 ~f:(fun () -> match Game_graphics.read_key game with |
   Restart -> Game.restart game; game_over := false; print_endline "test" | _
   -> ()) ;; *)

let run host port who_am_i =
  let client_state =
    { Client.game = Game_graphics.init_exn ()
    ; player = who_am_i
    ; moves_to_highlight = []
    }
  in
  (* let game = Game_graphics.init_exn () in let confirming_move = ref false
     in let moves_to_highlight = ref [] in *)
  Game_graphics.render client_state;
  let game_over = ref false in
  (* let game = ref (Game.new_game ~height:8 ~width:8) in *)
  handle_keys client_state ~game_over host port;
  handle_steps client_state ~game_over
;;
(* handle_restart game ~game_over *)
