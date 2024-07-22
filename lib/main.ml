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


    
end


let () =
  Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;
