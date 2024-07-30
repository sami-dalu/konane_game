open! Core
open! Fzf
open! Async
(* open! Async *)
(* module Exercises = struct let new_game = Game.new_game 8 8 end

   let () = Game.print Exercises.new_game *)

let implementations_w_server server =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement
          Demo1.Rpcs.Start_game.rpc
          (Demo1.Server.handle_start_query server)
      ; Rpc.Rpc.implement
          Demo1.Rpcs.Take_turn.rpc
          (Demo1.Server.handle_move_query server)
      ; Rpc.Rpc.implement
          Demo1.Rpcs.Wait_turn.rpc
          (Demo1.Server.handle_wait_query server)
      ; Rpc.Rpc.implement
          Demo1.Rpcs.End_turn.rpc
          (Demo1.Server.handle_end_query server)
      ]
;;

let start_server =
  Command.async
    ~summary:"Server start"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"INT server port" in
     fun () ->
       let _ = print_int port in
       let initial_server_t =
         { Demo1.Server.player_queue = Queue.create ()
         ; game_player_piece_tbl = Demo1.Player.Table.create ()
         }
       in
       let%bind server =
         Rpc.Connection.serve
           ~implementations:(implementations_w_server initial_server_t)
           ~initial_connection_state:(fun _client_identity _client_addr ->
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let start_game =
  Command.async
    ~summary:"connect to server and begin game"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"INT server port"
     and host = flag "-hostname" (required string) ~doc:"string of hostname"
     and name = flag "-name" (required string) ~doc:"name of player" in
     let query =
       { Demo1.Rpcs.Start_game.Query.name
       ; host_and_port = { Host_and_port.host = "localhost"; port }
       }
     in
     fun () ->
       let%bind start_game_response =
         Rpc.Connection.with_client
           (Tcp.Where_to_connect.of_host_and_port
              { Host_and_port.port; Host_and_port.host })
           (fun conn ->
             Rpc.Rpc.dispatch_exn Demo1.Rpcs.Start_game.rpc conn query)
       in
       (match start_game_response with
        | Error _ -> print_string "error lol"
        | Ok response ->
          print_s (Demo1.Rpcs.Start_game.Response.sexp_of_t response);
          (match response with
           | Game_started { your_player = who_am_i } ->
             Demo1.Run.run host port who_am_i
           | Game_not_started { your_player = who_am_i } ->
             Demo1.Run.run host port who_am_i));
       Deferred.never ())
;;

let command =
  Command.group
    ~summary:"Konane Game"
    [ "start-server", start_server; "start-game", start_game ]
;;

(* Menu stuff lol *)
(* Command.async ~summary:"start menu" (Core.print_endline "Welcome to
   Jonane!"; let results = Fzf.Blocking.pick_one ~prompt_at_top:()
   (Pick_from.inputs [ "Player v. Player" ]) in print_s [%message (results :
   string option)]; Demo1.Run.run (); Core.never_returns (Async.Scheduler.go
   ())) *)

let () =
  (* let results = Fzf.Blocking.pick_one (Pick_from.inputs [ "a"; "b"; "c" ])
     in print_s [%message (results : string option)]; let initial_game =
     Demo1.Game.new_game ~height:8 ~width:8 in *)
  (* Demo1.Game.print initial_game; *)
  print_string "hello, about to run command\n";
  (* Demo1.Game.make_move_exn ~game:(initial_game) {Demo1.Move.starting_pos =
     {Demo1.Position.row = 0; column = 0}; Demo1.Move.ending_pos = None};
     Demo1.Game.make_move_exn ~game:(initial_game) {Demo1.Move.starting_pos =
     {Demo1.Position.row = 0; column = 1}; Demo1.Move.ending_pos = None};
     Demo1.Game.make_move_exn ~game:(initial_game) {Demo1.Move.starting_pos =
     {Demo1.Position.row = 2; column = 0}; Demo1.Move.ending_pos = Some
     {Demo1.Position.row = 0; column = 0}}; Demo1.Game.make_move_exn
     ~game:(initial_game) {Demo1.Move.starting_pos = {Demo1.Position.row = 0;
     column = 3}; Demo1.Move.ending_pos = Some {Demo1.Position.row = 0;
     column = 1}}; Demo1.Game.make_move_exn ~game:(initial_game)
     {Demo1.Move.starting_pos = {Demo1.Position.row = 4; column = 0};
     Demo1.Move.ending_pos = Some {Demo1.Position.row = 2; column = 0}}; let
     av_moves = Demo1.Game.available_captures_for_player initial_game
     ~my_piece:(Demo1.Piece.O) in Core.print_s [%sexp (av_moves :
     Demo1.Move.t list )]; *)
  (* Demo1.Run.run (); Core.never_returns (Async.Scheduler.go ()) *)
  Command_unix.run command
;;
