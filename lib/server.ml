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
  { config_queue_tbl : (Game_config.t, Player.t Queue.t) Hashtbl.t
  ; game_player_piece_tbl : (Player.t, Game.t) Hashtbl.t
  }

let make_moves_exn (game : Game.t) (move_list : Move.t list) =
  List.iter move_list ~f:(fun move -> Game.make_move_exn ~game move);
  let _ = game.piece_to_move <- Piece.flip game.piece_to_move in
  game.last_move_from_piece_to_move <- None
;;

(* let _handle_test_query _client (query : Rpcs.Test.Query.t) :
   Rpcs.Test.Response.t Deferred.t = return (query + 1) ;; *)

let handle_start_query (server : t) _client (query : Rpcs.Start_game.Query.t)
  : Rpcs.Start_game.Response.t Deferred.t
  =
  print_string "handling a start query";
  let bot_info = query.bot_difficulty_and_piece in
  match bot_info with
  | None ->
    let queue_opt = Hashtbl.find server.config_queue_tbl query.game_config in
    if Option.is_none queue_opt
    then (
      let queue = Queue.create () in
      Hashtbl.add_exn
        server.config_queue_tbl
        ~key:query.game_config
        ~data:queue;
      let first_player = Player.init_human ~name:query.name ~piece:Piece.X in
      Queue.enqueue queue first_player;
      return
        (Rpcs.Start_game.Response.Game_not_started
           { your_player = first_player }))
    else (
      (* there's someone in the queue already *)
      let existing_queue = Option.value_exn queue_opt in
      let existing_player = Queue.dequeue_exn existing_queue in
      Hashtbl.remove server.config_queue_tbl query.game_config;
      let g =
        Game.new_game
          ~height:query.game_config.height
          ~width:query.game_config.width
          ()
      in
      g.player1 <- Some existing_player;
      let _ =
        Hashtbl.add_exn
          server.game_player_piece_tbl
          ~key:existing_player
          ~data:g
      in
      let new_p = Player.init_human ~name:query.name ~piece:Piece.O in
      g.player2 <- Some new_p;
      let _ =
        Hashtbl.add_exn server.game_player_piece_tbl ~key:new_p ~data:g
      in
      let response =
        Rpcs.Start_game.Response.Game_started { your_player = new_p }
      in
      return response)
  | Some (difficulty, bot_piece) ->
    let g =
      Game.new_game
        ~bot_diff:difficulty
        ~height:query.game_config.height
        ~width:query.game_config.width
        ()
    in
    let bot_player = Player.init_bot ~piece:bot_piece ~difficulty in
    Hashtbl.add_exn server.game_player_piece_tbl ~key:bot_player ~data:g;
    let new_p =
      Player.init_human ~name:query.name ~piece:(Piece.flip bot_piece)
    in
    (match bot_piece with
     | X ->
       g.player1 <- Some bot_player;
       g.player2 <- Some new_p
     | O ->
       g.player1 <- Some new_p;
       g.player2 <- Some bot_player
     | _ -> print_endline "Game thinks the bot piece is an Obstacle!");
    Hashtbl.add_exn server.game_player_piece_tbl ~key:new_p ~data:g;
    let response =
      Rpcs.Start_game.Response.Game_started { your_player = new_p }
    in
    return response
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
    let event_opt_ref = ref None in
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
         game.last_move_from_piece_to_move <- None;
         match game.crazy_info with
         | None -> ()
         | Some info ->
           (match info.turns_since_event_and_event_opt with
            | None -> ()
            | Some (count, _evt) ->
              let rand_num = Random.int 10 in
              if count >= rand_num
              then (
                let events = Crazy_info.Event.all in
                let event = List.random_element_exn events in
                event_opt_ref := Some event;
                match event with
                | Eruption ->
                  Game.place_obstacle game;
                  Game.place_obstacle game
                | Plague ->
                  Game.wither_piece game;
                  Game.wither_piece game
                | Duplicates -> Game.activate_duplicates game
                | Rotate -> Game.rotate_game_cw game
                | Flip_all -> Game.flip_all_pieces game
                | Monster -> ())))
       else game.last_move_from_piece_to_move <- Some move);
    Game.check_for_win game;
    game.last_move_played <- Some move;
    return
      (Rpcs.Take_turn.Response.Success { game; event_opt = !event_opt_ref })
;;

let handle_wait_query (server : t) _client (query : Rpcs.Wait_turn.Query.t) =
  let g = Hashtbl.find_exn server.game_player_piece_tbl query in
  (* print_s [%message "before moves are made " (g : Game.t)]; *)
  match g.bot_difficulty with
  | None -> return g
  | Some difficulty ->
    if not (Piece.equal g.piece_to_move (Player.get_piece query))
    then (
      let depth =
        match difficulty with
        | Player.Difficulty.Easy -> 0
        | Medium -> 1
        | Hard -> 3
      in
      let moves =
        Tmp_bot.use_minimax_to_find_best_moves g ~depth ~me:g.piece_to_move
      in
      let%bind () = Clock.after (Time_float.Span.of_sec 2.5) in
      make_moves_exn g (List.rev moves);
      g.last_move_played <- Some (List.hd_exn moves);
      Game.check_for_win g;
      return g)
    else return g
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
