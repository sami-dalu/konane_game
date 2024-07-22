open! Core
open! Async


module Exercises = struct
  let new_game = Game.new_game ~height:8 ~width:8

  let%expect_test "print_initial_game" =
  Game.print new_game;
  [%expect {|
  X | O | X | O | X | O | X | O
  -----------------------------
  O | X | O | X | O | X | O | X
  -----------------------------
  X | O | X | O | X | O | X | O
  -----------------------------
  O | X | O | X | O | X | O | X
  -----------------------------
  X | O | X | O | X | O | X | O
  -----------------------------
  O | X | O | X | O | X | O | X
  -----------------------------
  X | O | X | O | X | O | X | O
  -----------------------------
  O | X | O | X | O | X | O | X
  |}];
  return ();;


  let demo_one =
    Command.async
    ~summary:"Demo 1: Printing a game board"


end


