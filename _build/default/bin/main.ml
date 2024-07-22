(* open! Core
open! Async


(* module Exercises = struct
  let new_game = Game.new_game 8 8
end



let () = Game.print Exercises.new_game *) *)
open! Core
open! Async

let () =
let initial_game = Demo1.Move.Exercises.new_game in
  let next_game = Demo1.Move.Exercises.make_move_exn ~game:(initial_game) {Demo1.Move.Exercises.Move.starting_pos = {Demo1.Position.row = 0; column = 0}; Demo1.Move.Exercises.Move.ending_pos = None} in
Demo1.Game.print next_game;
  Demo1.Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;