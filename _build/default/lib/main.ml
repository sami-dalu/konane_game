open! Core
open! Async


module Exercises = struct
  module Move = struct
    type t =
      { starting_pos : Position.t
      ; ending_pos : Position.t option
      }
    [@@deriving sexp_of, equal, bin_io, compare]
  end
  let new_game = Game.new_game ~height:8 ~width:8
  let available_moves (game : Game.t) ~(my_piece : Piece.t) : Move.t list =
    match game.game_state with 
    | First_moves -> 
      let top_left = {Position.row = 0; column = 0} in
      let bottom_right = {Position.row = game.board_height -1; column = game.board_height -1} in
      let board_middle_bottom = {Position.row = game.board_height/2; column = game.board_height/2} in
      let board_middle_top = {Position.row = (game.board_height/2) -1 ; column = (game.board_height/2) -1 } in
      let first_positions_for_black = [top_left; bottom_right; board_middle_bottom; board_middle_top]
    in
      if Piece.equal my_piece Piece.X then List.map first_positions_for_black ~f:(fun pos -> {Move.starting_pos = pos; Move.ending_pos = None}) 
        (* ending move is None because you're not moving a piece from a starting pos to an ending pos *)
    else (
      (* otherwise, white has to move *)
      let black_move_pos = List.find_exn first_positions_for_black ~f:(fun pos -> Option.is_none (Map.find game.board pos)) in
      let above = {Position.row = black_move_pos.row -1; column = black_move_pos.column} in
      let below = {Position.row = black_move_pos.row + 1; column = black_move_pos.column} in
      let left = {Position.row = black_move_pos.row; column = black_move_pos.column - 1 } in
      let right = {Position.row = black_move_pos.row; column = black_move_pos.column + 1 } in
      let adjacent_pos = [above; below; left; right] in
      let adjacent_positions_on_board = [] in
        (* List.filter adjacent_pos ~f:(fun pos -> Position.in_board_range pos) in *)
      List.map adjacent_positions_on_board ~f:(fun valid_pos -> {Move.starting_pos = valid_pos; ending_pos = None})
    )
    | _ -> 
    (* let rows = List.range 0 len in
    let cols = List.range 0 len in
    let positions_grid =
      List.map rows ~f:(fun r ->
        List.map cols ~f:(fun c -> {Position.row = r; column = c }))
    in
    let positions_list = List.concat positions_grid in
    List.filter positions_list ~f:(fun pos ->
      not
        (List.exists (Map.keys game.board) ~f:(fun p ->
           Position.equal p pos))) *)
           []
  ;;

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


