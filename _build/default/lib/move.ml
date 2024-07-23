open! Core
open! Async

let in_board_range {Position.row = r; column = c} (game : Game.t ) = 
  r >= 0 && c>= 0 && r < game.board_height && c < game.board_width

module Exercises = struct
  module Move = struct
    type t =
      { starting_pos : Position.t
      ; ending_pos : Position.t option
      }
    [@@deriving sexp_of, equal, bin_io, compare]
  end

  let possible_captures_from_occupied_pos_exn (game : Game.t) (pos : Position.t) = 
  let my_piece = Map.find_exn (game.board) pos in
  let opp_piece = Piece.flip my_piece in
  let above_my = {Position.row = pos.row -1; column = pos.column} in
  let below_my = {Position.row = pos.row + 1; column = pos.column} in
  let left_my = {Position.row = pos.row; column = pos.column - 1 } in
  let right_my = {Position.row = pos.row; column = pos.column + 1 } in
  let capture_below = (
    let piece_below = Map.find game.board below_my in
    match piece_below with 
    (* None => no piece to capture *)
    | None -> None
    | Some piece -> if (Piece.equal piece opp_piece) then (
      let pos_two_below = {Position.row = below_my.row + 1; column = pos.column} in
      match Map.find game.board pos_two_below with 
      (* some => you can't jump to there *)
      | Some _ -> None 
      | None -> if (in_board_range pos_two_below game) then Some {Move.starting_pos = pos; ending_pos = Some pos_two_below} else None
    )
else None
  )
in
  let capture_right = (
    let piece_right = Map.find game.board right_my in
    match piece_right with 
    (* None => no piece to capture *)
    | None -> None
    | Some piece -> if (Piece.equal piece opp_piece) then (
      let pos_two_right = {Position.row = pos.row; column = right_my.column + 1} in
      match Map.find game.board pos_two_right with 
      (* some => you can't jump to there *)
      | Some _ -> None 
      | None -> if (in_board_range pos_two_right game) then (Some {Move.starting_pos = pos; ending_pos = Some pos_two_right}) else None
    )
else None
  )
in let capture_left = (
  let piece_left = Map.find game.board left_my in
  match piece_left with 
  (* None => no piece to capture *)
  | None -> None
  | Some piece -> if (Piece.equal piece opp_piece) then (
    let pos_two_left = {Position.row = pos.row; column = left_my.column - 1} in
    match Map.find game.board pos_two_left with 
    (* some => you can't jump to there *)
    | Some _ -> None 
    | None -> if (in_board_range pos_two_left game) then (Some {Move.starting_pos = pos; ending_pos = Some pos_two_left}) else None
  )
else None
)
in
let capture_above = (
  let piece_above = Map.find game.board above_my in
  match piece_above with 
  (* None => no piece to capture *)
  | None -> None
  | Some piece -> if (Piece.equal piece opp_piece) then (
    let pos_two_above = {Position.row = above_my.row - 1; column = pos.column} in
    match Map.find game.board pos_two_above with 
    (* some => you can't jump to there *)
    | Some _ -> None 
    | None -> if (in_board_range pos_two_above game) then (Some {Move.starting_pos = pos; ending_pos = Some pos_two_above}) else None
  )
else None
)
in
let potential_moves = [capture_above; capture_below; capture_left; capture_right] in
let move_opts = List.filter potential_moves ~f:(fun potential_move -> Option.is_some potential_move) in
match move_opts with 
| [] -> []
| move_list -> List.map move_list ~f:(fun move_opt -> Option.value_exn move_opt)

;;
  let new_game = Game.new_game ~height:8 ~width:8
  let available_captures_for_player (game : Game.t) ~(my_piece : Piece.t) : Move.t list =
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
      let adjacent_positions = [above; below; left; right] in
      let adjacent_positions_on_board = List.filter adjacent_positions ~f:(fun pos -> in_board_range pos game) in
      List.map adjacent_positions_on_board ~f:(fun valid_pos -> {Move.starting_pos = valid_pos; ending_pos = None})
    )
    | _ -> List.fold (Map.keys game.board) ~init:[] ~f:(fun captures_so_far pos -> 
      let piece_at_pos =  (Map.find_exn game.board pos) in
      if (Piece.equal piece_at_pos my_piece) then (
        let available_captures = possible_captures_from_occupied_pos_exn game pos in
        available_captures @ captures_so_far) else captures_so_far
      )

  ;;

  let captured_pos (move : Move.t) = 
    match move.ending_pos with
    | None -> None
    | Some end_pos -> 
      let row = (move.starting_pos.row + end_pos.row)/2 in
      let col = (move.starting_pos.column + end_pos.column)/2 in
      Some {Position.row = row; column = col}
    ;;

  let make_move_exn ~(game : Game.t) (move : Move.t) = 
    let ending_position_opt = move.ending_pos in
    let new_board = 
    (match ending_position_opt with
    | None -> (
      (* no ending position <==> one of the intial moves *)
      match game.game_state with
      | Game.Game_state.First_moves -> 
        (* first move so "starting position" simply has to be removed from map *)
        Map.remove game.board (move.starting_pos)
      | _ -> failwith "no ending position for a Move.t but game is in the first_moves state"

    )
    | Some end_pos -> 
      let my_piece = Map.find_exn game.board move.starting_pos in
      let intermediate_map = (Map.set game.board ~key:(end_pos) ~data:(my_piece))
    in 
    let map_with_captured_piece = Map.remove intermediate_map move.starting_pos in
    let pos_of_captured_piece = Option.value_exn (captured_pos move) in
    Map.remove map_with_captured_piece pos_of_captured_piece
    )
  in
  let new_game_state = match ending_position_opt with 
    | None -> (
      let removed_piece = Map.find_exn game.board move.starting_pos in
      if Piece.equal removed_piece Piece.O then
      Game.Game_state.Game_continues
      else Game.Game_state.First_moves
    )
    | _ -> game.game_state
  in
  {game with board = new_board; game_state = new_game_state; piece_to_move=(Piece.flip game.piece_to_move)}
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

  let%expect_test "get_moves_in_initial_game" =
  let initial_game = new_game in
  let moves = available_captures_for_player initial_game ~my_piece:Piece.X in
  print_s [%sexp (moves : Move.t list)];
  [%expect {|
  (((starting_pos ((row 0) (column 0))) (ending_pos ()))
   ((starting_pos ((row 7) (column 7))) (ending_pos ()))
   ((starting_pos ((row 4) (column 4))) (ending_pos ()))
   ((starting_pos ((row 3) (column 3))) (ending_pos ())))
  |}];
  return ();;

  let%expect_test "black_remove_top_left" =
  let initial_game = new_game in
  let next_game = make_move_exn ~game:(initial_game) {Move.starting_pos = {Position.row = 0; column = 0}; Move.ending_pos = None} in
  Game.print next_game;
  [%expect {|
    | O | X | O | X | O | X | O
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
    
end
