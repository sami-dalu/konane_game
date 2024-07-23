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
let next = Demo1.Move.Exercises.make_move_exn ~game:(initial_game) {Demo1.Move.Exercises.Move.starting_pos = {Demo1.Position.row = 0; column = 0}; Demo1.Move.Exercises.Move.ending_pos = None} in
let next2 = Demo1.Move.Exercises.make_move_exn ~game:(next) {Demo1.Move.Exercises.Move.starting_pos = {Demo1.Position.row = 0; column = 1}; Demo1.Move.Exercises.Move.ending_pos = None} in
let next3 = Demo1.Move.Exercises.make_move_exn ~game:(next2) {Demo1.Move.Exercises.Move.starting_pos = {Demo1.Position.row = 2; column = 0}; 
Demo1.Move.Exercises.Move.ending_pos = Some {Demo1.Position.row = 0; column = 0}} in
Demo1.Game.print next3;
let  
  Demo1.Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;