open! Core

module Game_state = struct
  type t =
    | First_moves
    | Game_continues
    | Game_over of { winner : Piece.t }
  [@@deriving sexp, equal, bin_io]
end

(* module Direction = struct type t = | Up | Down | Left | Right end *)

type t =
  { board_height : int
  ; board_width : int
  ; mutable board : Piece.t Position.Map.t
  ; mutable game_state : Game_state.t
  ; mutable piece_to_move : Piece.t
  ; mutable last_move_from_piece_to_move : Move.t option
  }
[@@deriving sexp, bin_io]

let new_board ~height ~width : Piece.t Position.Map.t =
  let board = Position.Map.empty in
  let all_positions =
    List.init height ~f:(fun row ->
      List.init width ~f:(fun column -> { Position.row; Position.column }))
  in
  let better_all_positions = List.concat all_positions in
  List.fold
    better_all_positions
    ~init:board
    ~f:(fun acc { Position.row = r; column = c } ->
      Map.set
        acc
        ~key:{ Position.row = r; column = c }
        ~data:
          (match r % 2 = 0 with
           | true -> if c % 2 = 0 then Piece.X else O
           | false -> if c % 2 = 0 then O else X))
;;

let in_board_range { Position.row = r; column = c } (game : t) =
  r >= 0 && c >= 0 && r < game.board_height && c < game.board_width
;;

let new_game ~height ~width =
  { board_height = height
  ; board_width = width
  ; board = new_board ~height ~width
  ; game_state = Game_state.First_moves
  ; piece_to_move = Piece.X
  ; last_move_from_piece_to_move = None
  }
;;

let restart game =
  game.board <- new_board ~height:game.board_height ~width:game.board_width;
  game.game_state <- Game_state.First_moves;
  game.last_move_from_piece_to_move <- None;
  game.piece_to_move <- Piece.X
;;

let print t =
  let height = t.board_height in
  let width = t.board_width in
  let lists_to_print =
    List.init height ~f:(fun row ->
      List.init width ~f:(fun col ->
        if Map.mem t.board { Position.row; Position.column = col }
        then
          Piece.to_string
            (Map.find_exn t.board { Position.row; Position.column = col })
        else " "))
  in
  let rows_as_strings =
    List.map lists_to_print ~f:(fun row_list ->
      String.concat ~sep:" | " row_list)
  in
  List.iteri rows_as_strings ~f:(fun row_num row ->
    print_endline row;
    if row_num < height - 1
    then print_endline "-----------------------------")
;;

let possible_captures_from_occupied_pos_exn
  ?(dir_opt : Move.Direction.t option)
  (game : t)
  (pos : Position.t)
  =
  let my_piece = Map.find_exn game.board pos in
  let opp_piece = Piece.flip my_piece in
  let above_my = { Position.row = pos.row - 1; column = pos.column } in
  let below_my = { Position.row = pos.row + 1; column = pos.column } in
  let left_my = { Position.row = pos.row; column = pos.column - 1 } in
  let right_my = { Position.row = pos.row; column = pos.column + 1 } in
  let capture_below =
    let piece_below = Map.find game.board below_my in
    match piece_below with
    (* None => no piece to capture *)
    | None -> None
    | Some piece ->
      if Piece.equal piece opp_piece
      then (
        let pos_two_below =
          { Position.row = below_my.row + 1; column = pos.column }
        in
        match Map.find game.board pos_two_below with
        (* some => you can't jump to there *)
        | Some _ -> None
        | None ->
          if in_board_range pos_two_below game
          then
            Some
              { Move.starting_pos = pos
              ; ending_pos = Some pos_two_below
              ; dir = Some Move.Direction.Down
              }
          else None)
      else None
  in
  let capture_right =
    let piece_right = Map.find game.board right_my in
    match piece_right with
    (* None => no piece to capture *)
    | None -> None
    | Some piece ->
      if Piece.equal piece opp_piece
      then (
        let pos_two_right =
          { Position.row = pos.row; column = right_my.column + 1 }
        in
        match Map.find game.board pos_two_right with
        (* some => you can't jump to there *)
        | Some _ -> None
        | None ->
          if in_board_range pos_two_right game
          then
            Some
              { Move.starting_pos = pos
              ; ending_pos = Some pos_two_right
              ; dir = Some Move.Direction.Right
              }
          else None)
      else None
  in
  let capture_left =
    let piece_left = Map.find game.board left_my in
    match piece_left with
    (* None => no piece to capture *)
    | None -> None
    | Some piece ->
      if Piece.equal piece opp_piece
      then (
        let pos_two_left =
          { Position.row = pos.row; column = left_my.column - 1 }
        in
        match Map.find game.board pos_two_left with
        (* some => you can't jump to there *)
        | Some _ -> None
        | None ->
          if in_board_range pos_two_left game
          then
            Some
              { Move.starting_pos = pos
              ; ending_pos = Some pos_two_left
              ; dir = Some Move.Direction.Left
              }
          else None)
      else None
  in
  let capture_above =
    let piece_above = Map.find game.board above_my in
    match piece_above with
    (* None => no piece to capture *)
    | None -> None
    | Some piece ->
      if Piece.equal piece opp_piece
      then (
        let pos_two_above =
          { Position.row = above_my.row - 1; column = pos.column }
        in
        match Map.find game.board pos_two_above with
        (* some => you can't jump to there *)
        | Some _ -> None
        | None ->
          if in_board_range pos_two_above game
          then
            Some
              { Move.starting_pos = pos
              ; ending_pos = Some pos_two_above
              ; dir = Some Move.Direction.Up
              }
          else None)
      else None
  in
  let potential_moves =
    if Option.is_none dir_opt
    then [ capture_above; capture_below; capture_left; capture_right ]
    else (
      match Option.value_exn dir_opt with
      | Move.Direction.Up -> [ capture_above ]
      | Down -> [ capture_below ]
      | Left -> [ capture_left ]
      | Right -> [ capture_right ])
  in
  let move_opts =
    List.filter potential_moves ~f:(fun potential_move ->
      Option.is_some potential_move)
  in
  match move_opts with
  | [] -> []
  | move_list ->
    List.map move_list ~f:(fun move_opt -> Option.value_exn move_opt)
;;

let available_captures_for_player (game : t) ~(my_piece : Piece.t)
  : Move.t list
  =
  match game.game_state with
  | First_moves ->
    let top_left = { Position.row = 0; column = 0 } in
    let bottom_right =
      { Position.row = game.board_height - 1
      ; column = game.board_height - 1
      }
    in
    let board_middle_bottom =
      { Position.row = game.board_height / 2
      ; column = game.board_height / 2
      }
    in
    let board_middle_top =
      { Position.row = (game.board_height / 2) - 1
      ; column = (game.board_height / 2) - 1
      }
    in
    let first_positions_for_black =
      [ top_left; bottom_right; board_middle_bottom; board_middle_top ]
    in
    if Piece.equal my_piece Piece.X
    then
      List.map first_positions_for_black ~f:(fun pos ->
        { Move.starting_pos = pos; Move.ending_pos = None; dir = None })
      (* ending move is None because you're not moving a piece from a
         starting pos to an ending pos *)
    else (
      (* otherwise, white has to move *)
      let black_move_pos =
        List.find_exn first_positions_for_black ~f:(fun pos ->
          Option.is_none (Map.find game.board pos))
      in
      let above =
        { Position.row = black_move_pos.row - 1
        ; column = black_move_pos.column
        }
      in
      let below =
        { Position.row = black_move_pos.row + 1
        ; column = black_move_pos.column
        }
      in
      let left =
        { Position.row = black_move_pos.row
        ; column = black_move_pos.column - 1
        }
      in
      let right =
        { Position.row = black_move_pos.row
        ; column = black_move_pos.column + 1
        }
      in
      let adjacent_positions = [ above; below; left; right ] in
      let adjacent_positions_on_board =
        List.filter adjacent_positions ~f:(fun pos ->
          in_board_range pos game)
      in
      List.map adjacent_positions_on_board ~f:(fun valid_pos ->
        { Move.starting_pos = valid_pos; ending_pos = None; dir = None }))
  | _ ->
    List.fold (Map.keys game.board) ~init:[] ~f:(fun captures_so_far pos ->
      let piece_at_pos = Map.find_exn game.board pos in
      if Piece.equal piece_at_pos my_piece
      then (
        let available_captures =
          possible_captures_from_occupied_pos_exn game pos
        in
        available_captures @ captures_so_far)
      else captures_so_far)
;;

let%expect_test "print_initial_game" =
  let new_game = new_game ~height:8 ~width:8 in
  print new_game;
  [%expect
    {|
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
          |}]
;;

let%expect_test "get_moves_in_initial_game" =
  let new_game = new_game ~height:8 ~width:8 in
  let initial_game = new_game in
  let moves = available_captures_for_player initial_game ~my_piece:Piece.X in
  print_s [%sexp (moves : Move.t list)];
  [%expect
    {|
          (((starting_pos ((row 0) (column 0))) (ending_pos ()))
           ((starting_pos ((row 7) (column 7))) (ending_pos ()))
           ((starting_pos ((row 4) (column 4))) (ending_pos ()))
           ((starting_pos ((row 3) (column 3))) (ending_pos ())))
          |}]
;;

let make_move_exn ~game (move : Move.t) =
  let captured_pos () =
    match move.ending_pos with
    | None -> None
    | Some end_pos ->
      let row = (move.starting_pos.row + end_pos.row) / 2 in
      let col = (move.starting_pos.column + end_pos.column) / 2 in
      Some { Position.row; column = col }
  in
  let ending_position_opt = move.ending_pos in
  let new_board =
    match ending_position_opt with
    | None ->
      (* no ending position <==> one of the intial moves *)
      (match game.game_state with
       | Game_state.First_moves ->
         (* first move so "starting position" simply has to be removed from
            map *)
         Map.remove game.board move.starting_pos
       | _ ->
         failwith
           "no ending position for a Move.t but game is in the first_moves \
            state")
    | Some end_pos ->
      let my_piece = Map.find_exn game.board move.starting_pos in
      let intermediate_map =
        Map.set game.board ~key:end_pos ~data:my_piece
      in
      let map_with_captured_piece =
        Map.remove intermediate_map move.starting_pos
      in
      let pos_of_captured_piece = Option.value_exn (captured_pos ()) in
      Map.remove map_with_captured_piece pos_of_captured_piece
  in
  let new_game_state =
    match ending_position_opt with
    | None ->
      let removed_piece = Map.find_exn game.board move.starting_pos in
      if Piece.equal removed_piece Piece.O
      then Game_state.Game_continues
      else Game_state.First_moves
    | _ -> game.game_state
  in
  game.board <- new_board;
  game.game_state <- new_game_state;
  (* game.piece_to_move <- Piece.flip game.piece_to_move; *)
  print game
;;

let check_for_win game =
  match available_captures_for_player game ~my_piece:game.piece_to_move with
  | [] ->
    game.game_state
    <- Game_state.Game_over { winner = Piece.flip game.piece_to_move }
  | _ -> ()
;;

let evaluate t =
  match available_captures_for_player t ~my_piece:t.piece_to_move with
  | [] -> Game_state.Game_over { winner = Piece.flip t.piece_to_move }
  | _ -> Game_state.Game_continues
;;

let%expect_test "black_remove_top_left" =
  let initial_game = new_game ~height:8 ~width:8 in
  let _ =
    make_move_exn
      ~game:initial_game
      { Move.starting_pos = { Position.row = 0; column = 0 }
      ; Move.ending_pos = None
      ; dir = None
      }
  in
  print initial_game;
  [%expect
    {|
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
    |}]
;;
