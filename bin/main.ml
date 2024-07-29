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
     (* let new_game = Demo1.Game.new_game ~height:8 ~width:8 in let game =
        List.fold (List.init 55 ~f:(fun i -> i)) ~init:new_game ~f:(fun acc
        _i -> let moves_to_make =
        Demo1.Tmp_bot.use_minimax_to_find_best_moves ~depth:4
        ~me:acc.piece_to_move acc in List.iter (List.rev moves_to_make)
        ~f:(fun move -> Core.print_s (Demo1.Move.sexp_of_t move);
        Demo1.Game.make_move_exn ~game:acc move); acc.piece_to_move <-
        Demo1.Piece.flip acc.piece_to_move; acc) in Demo1.Game.print game; *)
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
