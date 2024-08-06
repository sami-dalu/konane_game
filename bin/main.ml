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
          Konanelib.Rpcs.Start_game.rpc
          (Konanelib.Server.handle_start_query server)
      ; Rpc.Rpc.implement
          Konanelib.Rpcs.Take_turn.rpc
          (Konanelib.Server.handle_move_query server)
      ; Rpc.Rpc.implement
          Konanelib.Rpcs.Wait_turn.rpc
          (Konanelib.Server.handle_wait_query server)
      ; Rpc.Rpc.implement
          Konanelib.Rpcs.End_turn.rpc
          (Konanelib.Server.handle_end_query server)
      ; Rpc.Rpc.implement
          Konanelib.Rpcs.Restart_game.rpc
          (Konanelib.Server.handle_restart_query server)
      ]
;;

let _start_server =
  Command.async
    ~summary:"Server start"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"INT server port" in
     fun () ->
       let _ = print_int port in
       let initial_server_t =
         { Konanelib.Server.config_queue_tbl =
             Konanelib.Game_config.Table.create ()
         ; game_player_piece_tbl = Konanelib.Player.Table.create ()
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

(* let _start_game = Command.async ~summary:"connect to server and begin
   game" (let%map_open.Command () = return () and port = flag "-port"
   (required int) ~doc:"INT server port" and host = flag "-hostname"
   (required string) ~doc:"string of hostname" and name = flag "-name"
   (required string) ~doc:"name of player" in let query = {
   Konanelib.Rpcs.Start_game.Query.name ; host_and_port = {
   Host_and_port.host = "localhost"; port } } in fun () -> let%bind
   start_game_response = Rpc.Connection.with_client
   (Tcp.Where_to_connect.of_host_and_port { Host_and_port.port;
   Host_and_port.host }) (fun conn -> Rpc.Rpc.dispatch_exn
   Konanelib.Rpcs.Start_game.rpc conn query) in (match start_game_response
   with | Error _ -> print_string "error lol" | Ok response -> print_s
   (Konanelib.Rpcs.Start_game.Response.sexp_of_t response); (match response
   with | Game_started { your_player = who_am_i } -> Konanelib.Run.run host
   port who_am_i | Game_not_started { your_player = who_am_i } ->
   Konanelib.Run.run host port who_am_i)); Deferred.never ()) ;; *)

(* let command = Command.group ~summary:"Konane Game" [ "start-server",
   start_server; "start-game", start_game ] ;; *)

(* Menu stuff lol *)
(* Command.async ~summary:"start menu" (Core.print_endline "Welcome to
   Jonane!"; let results = Fzf.Blocking.pick_one ~prompt_at_top:()
   (Pick_from.inputs [ "Player v. Player" ]) in print_s [%message (results :
   string option)]; Konanelib.Run.run (); Core.never_returns
   (Async.Scheduler.go ())) *)

let rec stubborn_read_int (reading_port : bool) () =
  let stdin = Lazy.force Reader.stdin in
  match%bind Reader.read_line stdin with
  | `Ok s ->
    if String.equal s "" && not reading_port
    then return 8
    else if String.equal s ""
    then return 10001
    else (
      let num_opt = Int.of_string_opt s in
      match num_opt with
      | Some num -> return num
      | None ->
        print_string "Invalid number, please try again.\n";
        stubborn_read_int reading_port ())
  | `Eof ->
    print_string "Invalid number, please try again.\n";
    stubborn_read_int reading_port ()
;;

let rec stubborn_read_str () =
  let stdin = Lazy.force Reader.stdin in
  let host =
    match%bind Reader.read_line stdin with
    | `Ok s -> return s
    | `Eof -> stubborn_read_str ()
  in
  host
;;

let rec stubborn_read_difficulty () =
  let%bind bot_difficulty_result =
    Fzf.pick_one
      ~header:"Choose a bot difficulty."
      ~prompt_at_top:()
      (Pick_from.inputs [ "Easy"; "Medium"; "Hard" ])
  in
  match bot_difficulty_result with
  | Ok (Some s) ->
    (match s with
     | "Easy" -> return Konanelib.Player.Difficulty.Easy
     | "Medium" -> return Konanelib.Player.Difficulty.Medium
     | "Hard" -> return Konanelib.Player.Difficulty.Hard
     | _ -> stubborn_read_difficulty ())
  | _ -> stubborn_read_difficulty ()
;;

let rec stubborn_read_piece () =
  let%bind bot_piece_result =
    Fzf.pick_one
      ~header:"Choose a piece to play."
      ~prompt_at_top:()
      (Pick_from.inputs [ "Black"; "White" ])
  in
  match bot_piece_result with
  | Ok (Some s) ->
    (match s with
     | "Black" -> return Konanelib.Piece.X
     | "White" -> return Konanelib.Piece.O
     | _ -> stubborn_read_piece ())
  | _ -> stubborn_read_piece ()
;;

let rec stubborn_read_game_mode () =
  let%bind bot_difficulty_result =
    Fzf.pick_one
      ~header:"Choose a game mode."
      ~prompt_at_top:()
      (Pick_from.inputs [ "Normal"; "Crazy" ])
  in
  match bot_difficulty_result with
  | Ok (Some s) ->
    (match s with
     | "Normal" -> return Konanelib.Game_config.Game_mode.Normal
     | "Crazy" -> return Konanelib.Game_config.Game_mode.Crazy
     | _ -> stubborn_read_game_mode ())
  | _ -> stubborn_read_game_mode ()
;;

(* let stubborn_read_host_and_port () = let%bind host = stubborn_read_str ()
   in let _ = print_string "What's your port?\n" in let%bind port =
   stubborn_read_int () in return ({ Host_and_port.host; port }, host) ;; *)

(* Out_channel.(flush stdout); In_channel.(input_line_exn stdin) in let _ =
   print_string "\nEnter the port.\n" in let port = stubborn_read_int () in {
   Host_and_port.port; Host_and_port.host }, host *)
(* return using async *)

let menu =
  Command.async
    ~summary:"game menu"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind main_menu_result =
         Fzf.pick_one
           (Pick_from.inputs [ "Player v. Player"; "Player v. Bot" ])
       in
       match main_menu_result with
       | Ok (Some s) ->
         (match s with
          | "Player v. Player" ->
            let%bind pvp_result =
              Fzf.pick_one
                (Pick_from.inputs [ "Join a game"; "Start a server" ])
            in
            (match pvp_result with
             | Ok None | Error _ -> Deferred.unit
             | Ok (Some str) ->
               (match str with
                | "Start a server" ->
                  print_string
                    "Enter the port the server should listen in on (default \
                     to 10001) \n";
                  (* start the server *)
                  let%bind port = stubborn_read_int true () in
                  print_string "Starting the server!";
                  let initial_server_t =
                    { Konanelib.Server.config_queue_tbl =
                        Konanelib.Game_config.Table.create ()
                    ; game_player_piece_tbl =
                        Konanelib.Player.Table.create ()
                    }
                  in
                  let%bind server =
                    Rpc.Connection.serve
                      ~implementations:
                        (implementations_w_server initial_server_t)
                      ~initial_connection_state:
                        (fun
                          _client_identity _client_addr -> ())
                      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
                      ()
                  in
                  Tcp.Server.close_finished server
                | "Join a game" ->
                  print_string "Enter your name.\n";
                  let%bind name = stubborn_read_str () in
                  print_string "Enter your host.\n";
                  let%bind host = stubborn_read_str () in
                  print_string
                    "Enter your port (hit enter for default of 10001).\n";
                  let%bind port = stubborn_read_int true () in
                  print_string
                    "Enter your desired game height (press enter to default \
                     to 8): ";
                  let%bind mode = stubborn_read_game_mode () in
                  let%bind height = stubborn_read_int false () in
                  print_string
                    "Enter your desired game width (press enter to default \
                     to 8): ";
                  let%bind width = stubborn_read_int false () in
                  let game_config =
                    { Konanelib.Game_config.height; width; mode }
                  in
                  let query =
                    { Konanelib.Rpcs.Start_game.Query.name
                    ; bot_difficulty_and_piece = None
                    ; game_config
                    }
                  in
                  let host_and_port = { Host_and_port.host; port } in
                  let%bind start_game_response =
                    Rpc.Connection.with_client
                      (Tcp.Where_to_connect.of_host_and_port host_and_port)
                      (fun conn ->
                         Rpc.Rpc.dispatch_exn
                           Konanelib.Rpcs.Start_game.rpc
                           conn
                           query)
                  in
                  (match start_game_response with
                   | Error _ -> print_string "Error starting game"
                   | Ok response ->
                     print_s
                       (Konanelib.Rpcs.Start_game.Response.sexp_of_t
                          response);
                     (match response with
                      | Game_started { your_player = who_am_i } ->
                        Konanelib.Run.run host port who_am_i game_config
                      | Game_not_started { your_player = who_am_i } ->
                        Konanelib.Run.run host port who_am_i game_config));
                  Deferred.never ()
                | _ ->
                  print_string "invalid option";
                  Deferred.unit))
          | "Player v. Bot" ->
            print_string "Enter your name.\n";
            let%bind name = stubborn_read_str () in
            let%bind mode = stubborn_read_game_mode () in
            print_string "Choose a bot difficulty.\n";
            let%bind difficulty = stubborn_read_difficulty () in
            let%bind piece = stubborn_read_piece () in
            let initial_server_t =
              { Konanelib.Server.config_queue_tbl =
                  Konanelib.Game_config.Table.create ()
              ; game_player_piece_tbl = Konanelib.Player.Table.create ()
              }
            in
            print_string
              "Enter your desired game height (press enter to default to \
               8): ";
            let%bind height = stubborn_read_int false () in
            print_string
              "Enter your desired game width (press enter to default to 8). ";
            let%bind width = stubborn_read_int false () in
            let game_config =
              { Konanelib.Game_config.height; width; mode }
            in
            let%bind server =
              Rpc.Connection.serve
                ~implementations:(implementations_w_server initial_server_t)
                ~initial_connection_state:
                  (fun
                    _client_identity _client_addr -> ())
                ~where_to_listen:(Tcp.Where_to_listen.of_port 14624)
                ()
            in
            let _ = Tcp.Server.close_finished server in
            let query =
              { Konanelib.Rpcs.Start_game.Query.name
              ; bot_difficulty_and_piece =
                  Some (difficulty, Konanelib.Piece.flip piece)
              ; game_config
              }
            in
            let host_and_port =
              { Host_and_port.host = "localhost"; port = 14624 }
            in
            let%bind start_game_response =
              Rpc.Connection.with_client
                (Tcp.Where_to_connect.of_host_and_port host_and_port)
                (fun conn ->
                   Rpc.Rpc.dispatch_exn
                     Konanelib.Rpcs.Start_game.rpc
                     conn
                     query)
            in
            (match start_game_response with
             | Error _ -> print_string "Error starting game"
             | Ok response ->
               print_s
                 (Konanelib.Rpcs.Start_game.Response.sexp_of_t response);
               (match response with
                | Game_started { your_player = who_am_i } ->
                  Konanelib.Run.run "localhost" 14624 who_am_i game_config
                | Game_not_started { your_player = who_am_i } ->
                  Konanelib.Run.run "localhost" 14624 who_am_i game_config));
            Deferred.never ()
          | _ -> Deferred.unit)
       | _ -> Deferred.unit)
;;

let () = Command_unix.run menu
(* let results = Fzf.Blocking.pick_one (Pick_from.inputs [ "a"; "b"; "cn" ])
   in print_s [%message (results : string option)]; let initial_game =
   Konanelib.Game.new_game ~height:8 ~width:8 in *)
(* Konanelib.Game.print initial_game; *)

(* Konanelib.Game.make_move_exn ~game:(initial_game)
   {Konanelib.Move.starting_pos = {Konanelib.Position.row = 0; column = 0};
   Konanelib.Move.ending_pos = None}; Konanelib.Game.make_move_exn
   ~game:(initial_game) {Konanelib.Move.starting_pos =
   {Konanelib.Position.row = 0; column = 1}; Konanelib.Move.ending_pos =
   None}; Konanelib.Game.make_move_exn ~game:(initial_game)
   {Konanelib.Move.starting_pos = {Konanelib.Position.row = 2; column = 0};
   Konanelib.Move.ending_pos = Some {Konanelib.Position.row = 0; column =
   0}}; Konanelib.Game.make_move_exn ~game:(initial_game)
   {Konanelib.Move.starting_pos = {Konanelib.Position.row = 0; column = 3};
   Konanelib.Move.ending_pos = Some {Konanelib.Position.row = 0; column =
   1}}; Konanelib.Game.make_move_exn ~game:(initial_game)
   {Konanelib.Move.starting_pos = {Konanelib.Position.row = 4; column = 0};
   Konanelib.Move.ending_pos = Some {Konanelib.Position.row = 2; column =
   0}}; let av_moves = Konanelib.Game.available_captures_for_player
   initial_game ~my_piece:(Konanelib.Piece.O) in Core.print_s [%sexp
   (av_moves : Konanelib.Move.t list )]; *)
(* Konanelib.Run.run (); Core.never_returns (Async.Scheduler.go ()) *)
