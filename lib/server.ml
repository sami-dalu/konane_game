open! Core
open! Async

type t =
  { config_queue_tbl : (Game_config.t, Player.t Queue.t) Hashtbl.t
  ; game_player_piece_tbl : (Player.t, Game.t) Hashtbl.t
  }

let make_moves_exn (game : Game.t) (move_list : Move.t list) =
  List.iter move_list ~f:(fun move -> Game.make_move_exn ~game move);
  let _ = game.piece_to_move <- Piece.flip game.piece_to_move in
  game.last_move_from_piece_to_move <- None
;;

let do_disaster (game : Game.t) =
  let disasters =
    [ Crazy_info.Event.Eruption
    ; Crazy_info.Event.Plague
    ; Crazy_info.Event.Duplicates
    ; Crazy_info.Event.Rotate
    ; Crazy_info.Event.Flip_all
    ; Crazy_info.Event.Monster
    ]
  in
  let event = List.random_element_exn disasters in
  (match event with
   | Crazy_info.Event.Eruption ->
     Game.place_obstacle game;
     Game.place_obstacle game;
     Game.place_obstacle game
   | Crazy_info.Event.Plague ->
     Game.wither_piece game;
     Game.wither_piece game
   | Crazy_info.Event.Duplicates -> Game.activate_duplicates game
   | Crazy_info.Event.Rotate ->
     Game.rotate_game_cw game;
     game.inverse_board <- not game.inverse_board
   | Crazy_info.Event.Flip_all ->
     Game.flip_all_pieces game;
     game.inverse_board <- not game.inverse_board
   | Crazy_info.Event.Monster -> Game.spawn_monster game
   | _ -> ());
  event
;;

let make_monsters_feast_or_move (game : Game.t) =
  Core.print_endline "function callewd!";
  match game.crazy_info with
  | None -> ()
  | Some info ->
    let monster_pos_list = info.monster_locations_list in
    let new_mons_list =
      List.map
        monster_pos_list
        ~f:(fun ({ Position.row = r; column = c }, n) ->
          let above_my = { Position.row = r - 1; column = c } in
          let below_my = { Position.row = r + 1; column = c } in
          let left_my = { Position.row = r; column = c - 1 } in
          let right_my = { Position.row = r; column = c + 1 } in
          let valid_positions_to_move_or_eat_list =
            List.filter
              [ above_my; below_my; left_my; right_my ]
              ~f:(fun pos -> Game.in_board_range pos game)
          in
          let possible_location_to_eat_at =
            List.find valid_positions_to_move_or_eat_list ~f:(fun pos ->
              match Map.find game.board pos with
              | None -> false
              | Some piece -> Piece.equal X piece || Piece.equal O piece)
          in
          let new_position_of_monster =
            match possible_location_to_eat_at with
            | Some eating_loc -> eating_loc
            | None ->
              List.random_element_exn valid_positions_to_move_or_eat_list
          in
          let map_with_monster_removed =
            Map.remove game.board { Position.row = r; column = c }
          in
          let map_with_monster_moved =
            Map.set
              map_with_monster_removed
              ~key:new_position_of_monster
              ~data:Piece.Monster
          in
          game.board <- map_with_monster_moved;
          new_position_of_monster, n)
    in
    info.monster_locations_list <- new_mons_list
;;

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
      Hashtbl.add_exn
        server.game_player_piece_tbl
        ~key:existing_player
        ~data:g;
      if Game_config.Game_mode.equal
           query.game_config.mode
           Game_config.Game_mode.Crazy
      then g.crazy_info <- Some (Crazy_info.default ());
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
    if Game_config.Game_mode.equal
         query.game_config.mode
         Game_config.Game_mode.Crazy
    then g.crazy_info <- Some (Crazy_info.default ());
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
           print_s (Crazy_info.sexp_of_t info);
           let count, evt = info.turns_since_event_and_event in
           if Crazy_info.Event.equal evt Crazy_info.Event.Impending_start
           then (
             let new_count = count + 1 in
             info.turns_since_event_and_event <- new_count, evt;
             if new_count >= 3
             then (
               let event = do_disaster game in
               event_opt_ref := Some event))
           else (
             let rand_num = Random.int 5 in
             if count >= rand_num && count > 0
             then event_opt_ref := Some (do_disaster game)
             else info.turns_since_event_and_event <- count + 1, evt);
           Game.decrement_and_prune_crazy_stuff game;
           (match !event_opt_ref with
            | None -> make_monsters_feast_or_move game
            | Some ev ->
              (match ev with
               | Crazy_info.Event.Monster -> ()
               | _ -> make_monsters_feast_or_move game)))
       else game.last_move_from_piece_to_move <- Some move);
    Game.check_for_win game;
    game.last_move_played <- Some move;
    return
      (Rpcs.Take_turn.Response.Success { game; event_opt = !event_opt_ref })
;;

let handle_wait_query (server : t) _client (query : Rpcs.Wait_turn.Query.t) =
  let g = Hashtbl.find_exn server.game_player_piece_tbl query in
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
  g.piece_to_move <- Piece.flip g.piece_to_move;
  g.last_move_from_piece_to_move <- None;
  let event_opt_ref = ref None in
  match g.crazy_info with
  | None -> return { Rpcs.End_turn.Response.game = g }
  | Some info ->
    print_s (Crazy_info.sexp_of_t info);
    let count, evt = info.turns_since_event_and_event in
    if Crazy_info.Event.equal evt Crazy_info.Event.Impending_start
    then (
      let new_count = count + 1 in
      info.turns_since_event_and_event <- new_count, evt;
      if new_count >= 3
      then (
        let event = do_disaster g in
        event_opt_ref := Some event))
    else (
      let rand_num = Random.int 10 in
      if count >= rand_num
      then event_opt_ref := Some (do_disaster g)
      else info.turns_since_event_and_event <- count + 1, evt);
    Game.decrement_and_prune_crazy_stuff g;
    (match !event_opt_ref with
     | None -> make_monsters_feast_or_move g
     | Some ev ->
       (match ev with
        | Crazy_info.Event.Monster -> ()
        | _ -> make_monsters_feast_or_move g));
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
