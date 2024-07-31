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

let _start_server =
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

let _start_game =
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

(* let command = Command.group ~summary:"Konane Game" [ "start-server",
   start_server; "start-game", start_game ] ;; *)

(* Menu stuff lol *)
(* Command.async ~summary:"start menu" (Core.print_endline "Welcome to
   Jonane!"; let results = Fzf.Blocking.pick_one ~prompt_at_top:()
   (Pick_from.inputs [ "Player v. Player" ]) in print_s [%message (results :
   string option)]; Demo1.Run.run (); Core.never_returns (Async.Scheduler.go
   ())) *)

let rec stubborn_read_int () =
  let stdin = Lazy.force Reader.stdin in
  match%bind Reader.read_line stdin with
  | `Ok s ->
    let num_opt = Int.of_string_opt s in
    (match num_opt with
     | Some num ->
       let _ = print_string "GOT YOUR NUMBER " in
       return num
     | None ->
       let _ = print_string "Invalid number, please try again.\n" in
       stubborn_read_int ())
  | `Eof ->
    let _ = print_string "Invalid number, please try again.\n" in
    stubborn_read_int ()
;;

let rec stubborn_read_host_and_port () =
  print_string "Enter the host name.\n";
  let stdin = Lazy.force Reader.stdin in
  let host =
    match%bind Reader.read_line stdin with
    | `Ok s -> s
    | `Eof ->
      print_string "Invalid number, please try again.\n";
      stubborn_read_host_and_port ()
  in
  return host
;;

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
         Fzf.pick_one (Pick_from.inputs [ "Player v. Player" ])
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
                  let _ =
                    print_string
                      "Enter the port the server should listen in on:\n"
                  in
                  (* start the server *)
                  let port = stubborn_read_int () in
                  let _ = print_string "awesome, starting server!" in
                  let initial_server_t =
                    { Demo1.Server.player_queue = Queue.create ()
                    ; game_player_piece_tbl = Demo1.Player.Table.create ()
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
                  let _ = print_string "Enter your name.\n" in
                  let name =
                    Out_channel.(flush stdout);
                    In_channel.(input_line_exn stdin)
                  in
                  let _ = print_string "Enter your port.\n" in
                  let port = stubborn_read_int () in
                  let host_and_port, host = stubborn_read_host_and_port () in
                  let query =
                    { Demo1.Rpcs.Start_game.Query.name
                    ; host_and_port =
                        { Host_and_port.host = "localhost"; port }
                    }
                  in
                  let%bind start_game_response =
                    Rpc.Connection.with_client
                      (Tcp.Where_to_connect.of_host_and_port host_and_port)
                      (fun conn ->
                         Rpc.Rpc.dispatch_exn
                           Demo1.Rpcs.Start_game.rpc
                           conn
                           query)
                  in
                  (match start_game_response with
                   | Error _ -> print_string "error lol"
                   | Ok response ->
                     print_s
                       (Demo1.Rpcs.Start_game.Response.sexp_of_t response);
                     (match response with
                      | Game_started { your_player = who_am_i } ->
                        Demo1.Run.run host port who_am_i
                      | Game_not_started { your_player = who_am_i } ->
                        Demo1.Run.run host port who_am_i));
                  Deferred.never ()
                | _ ->
                  print_string "invalid option";
                  Deferred.unit))
          | _ -> Deferred.unit)
       | _ -> Deferred.unit)
;;

let () = Command_unix.run menu
(* let results = Fzf.Blocking.pick_one (Pick_from.inputs [ "a"; "b"; "c" ])
   in print_s [%message (results : string option)]; let initial_game =
   Demo1.Game.new_game ~height:8 ~width:8 in *)
(* Demo1.Game.print initial_game; *)

(* Demo1.Game.make_move_exn ~game:(initial_game) {Demo1.Move.starting_pos =
   {Demo1.Position.row = 0; column = 0}; Demo1.Move.ending_pos = None};
   Demo1.Game.make_move_exn ~game:(initial_game) {Demo1.Move.starting_pos =
   {Demo1.Position.row = 0; column = 1}; Demo1.Move.ending_pos = None};
   Demo1.Game.make_move_exn ~game:(initial_game) {Demo1.Move.starting_pos =
   {Demo1.Position.row = 2; column = 0}; Demo1.Move.ending_pos = Some
   {Demo1.Position.row = 0; column = 0}}; Demo1.Game.make_move_exn
   ~game:(initial_game) {Demo1.Move.starting_pos = {Demo1.Position.row = 0;
   column = 3}; Demo1.Move.ending_pos = Some {Demo1.Position.row = 0; column
   = 1}}; Demo1.Game.make_move_exn ~game:(initial_game)
   {Demo1.Move.starting_pos = {Demo1.Position.row = 4; column = 0};
   Demo1.Move.ending_pos = Some {Demo1.Position.row = 2; column = 0}}; let
   av_moves = Demo1.Game.available_captures_for_player initial_game
   ~my_piece:(Demo1.Piece.O) in Core.print_s [%sexp (av_moves : Demo1.Move.t
   list )]; *)
(* Demo1.Run.run (); Core.never_returns (Async.Scheduler.go ()) *)
