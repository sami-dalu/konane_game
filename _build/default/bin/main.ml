(* open! Core
open! Async


(* module Exercises = struct
  let new_game = Game.new_game 8 8
end



let () = Game.print Exercises.new_game *) *)
open! Core
open! Async

let () =
Demo1.Game.print Demo1.Move.Exercises.new_game;
  Demo1.Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;