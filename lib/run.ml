open! Core
open! Async

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

let handle_keys client_state host port =
  Game_graphics.render client_state;
  match Game_graphics.read_key client_state with
  | None -> Deferred.return ()
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
      match make_move_response with
      | Error _ ->
        print_string "error start";
        Deferred.return ()
      | Ok response ->
        (match response with
         | Success { game = new_game; event_opt = _new_event_opt } ->
           client_state.game <- new_game;
           if Piece.equal
                new_game.piece_to_move
                (Player.get_piece client_state.player)
           then ()
           else client_state.show_message <- true;
           (match _new_event_opt with
            | None ->
              Game_graphics.render client_state;
              Deferred.return ()
            | Some _evt ->
              Game_graphics.render client_state;
              Deferred.return ())
         | Failure -> Deferred.return ()))
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
       client_state.game <- response;
       client_state.show_message <- false;
       client_state.last_event <- None);
    Game_graphics.render client_state;
    Deferred.return ()
  | End_turn ->
    (* flip the piece on the server side and set last_move_from_piece_to_move
       to None *)
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
         client_state.show_message <- true;
         let new_game = response.game in
         print_s (Piece.sexp_of_t new_game.piece_to_move);
         client_state.moves_to_highlight <- [];
         client_state.game <- new_game);
      Game_graphics.render client_state;
      Deferred.return ())
    else Deferred.return ()
;;

let check_server (client_state : Client.t) ~game_over host port =
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
    match wait_turn_response with
    | Error _ -> handle_keys client_state host port
    | Ok response ->
      client_state.game <- response;
      (match response.crazy_info with
       | None -> handle_keys client_state host port
       | Some crazy ->
         (match crazy.turns_since_event_and_event with
          | _, Impending_start -> handle_keys client_state host port
          | turns, evt ->
            if turns = 0 && client_state.show_message
            then (
              client_state.last_event <- Some evt;
              Game_graphics.render client_state;
              let%bind () = Clock.after (Time_float.Span.of_sec 3.) in
              client_state.show_message <- false;
              Deferred.return ())
            else handle_keys client_state host port))
    (* if event happened, set client event to it and set show_message to
       true *))
;;

let run host port who_am_i (game_config : Game_config.t) =
  let client_state =
    { Client.game = Game_graphics.init_exn game_config
    ; player = who_am_i
    ; moves_to_highlight = []
    ; last_event = None
    ; show_message = false
    }
  in
  Game_graphics.render client_state;
  let game_over = ref false in
  check_server client_state ~game_over host port
;;
