open! Core


  module Game_state = struct
    type t =
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


  let new_board h w =
    let board = Position.Map.empty in
    let all_positions = List.init h ~f:(fun row -> List.init w ~f:(fun column -> {Position.row; Position.column})) in
    List.iter all_positions


    (* Map.set board ~key:{Position.Position.row; Position.Position.column=col} ~data:(match row%2=0 with | true -> (if col%2=0 then X else O) | false -> (if col%2=0 then O else X)) *)

  let new_game height width =
    {board_height=height; board_width=width; board=new_board height width; game_state=Game_state.Game_continues}

  let print t =
    let height = t.board_height in
    let width = t.board_width in
    let lists_to_print = List.init height ~f:(fun row -> List.init width ~f:(fun col -> if Map.mem t.board {Position.row=row; Position.column=col} then Piece.to_string (Map.find_exn t.board {Position.row=row; Position.column=col}) else " ")) in
    let rows_as_strings =
      List.map lists_to_print ~f:(fun row_list -> String.concat ~sep:" | " row_list ) in 
      List.iteri rows_as_strings ~f:(fun row_num row -> Core.print_endline row;
      if row_num < height -1 then Core.print_endline "-----------------------------");;