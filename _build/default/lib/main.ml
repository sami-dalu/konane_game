open! Core
open! Async


module Exercises = struct
  let new_game = Game.new_game ~height:8 ~width:8
end



let () = Game.print Exercises.new_game
