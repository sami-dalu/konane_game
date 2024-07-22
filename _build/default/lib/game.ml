open! Core


    module Game_state = struct
    type t =
    | First_moves
    | Game_continues
    | Game_over of { winner: Piece.t option}
  end

  type t =
  {
    board_height:int;
    board_width:int;
    board:Piece.t Position.Map.t;
    game_state:Game_state.t
  }
  let new_board ~height ~width : Piece.t Position.Map.t=
    let board = Position.Map.empty in
    let all_positions = List.init height ~f:(fun row -> List.init width ~f:(fun column -> {Position.row; Position.column})) in
    let better_all_positions = List.concat all_positions in
    List.fold better_all_positions ~init:board ~f:( fun acc {Position.row = r;column = c} -> Map.set acc ~key:{Position.row = r;column = c} ~data:(match r%2=0 with | true -> (if c%2=0 then Piece.X else O) | false -> (if c%2=0 then O else X)))


  let new_game ~height ~width =
    {board_height=height; board_width=width; board=(new_board ~height ~width); game_state=Game_state.First_moves}

  let print t =
    let height = t.board_height in
    let width = t.board_width in
    let lists_to_print = List.init height ~f:(fun row -> List.init width ~f:(fun col -> if Map.mem t.board {Position.row; Position.column=col} then Piece.to_string (Map.find_exn t.board {Position.row; Position.column=col}) else " ")) in
    let rows_as_strings =
      List.map lists_to_print ~f:(fun row_list -> String.concat ~sep:" | " row_list ) in 
      List.iteri rows_as_strings ~f:(fun row_num row -> print_endline row;
      if row_num < height -1 then print_endline "-----------------------------")


