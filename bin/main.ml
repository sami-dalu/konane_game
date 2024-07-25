open! Core
open! Fzf
open! Async
(* open! Async *)
(* module Exercises = struct let new_game = Game.new_game 8 8 end

   let () = Game.print Exercises.new_game *)

let command =
  Command.async
    ~summary:"start menu"
    (Core.print_endline "Welcome to Joenane!";
     let results =
       Fzf.Blocking.pick_one
         ~prompt_at_top:()
         (Pick_from.inputs [ "Player v. Player" ])
     in
     print_s [%message (results : string option)];
     Demo1.Run.run ();
     Core.never_returns (Async.Scheduler.go ()))
;;

let () =
  (* let results = Fzf.Blocking.pick_one (Pick_from.inputs [ "a"; "b"; "c" ])
     in print_s [%message (results : string option)]; let initial_game =
     Demo1.Game.new_game ~height:8 ~width:8 in *)
  (* Demo1.Game.print initial_game; *)
  print_string "hello\n";
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
