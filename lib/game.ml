open! Core


module Game = struct
  type t =
  {
    board_height:int;
    board_width:int;
    board:Piece.t Position.Map.t;
    game_state:Game_state.t
  }

  module Game_state = struct
    type t =
    | Game_continues
    | Game_over of { winner: Piece.t option}
  end

  let new_game height width =
    {board_height=height; board_width=width; board=Position.Map.empty; game_state=Game_state.Game_continues}

  let print (game: Game.t) =
    let height = game.board_height in
    let width = game.board_width in
    let lists_to_print = List.init height ~f:(fun row -> List.init width ~f:(fun col -> if Map.mem game.board {Position.row; Position.col} then Piece.to_string (Map.find_exn game.board {Position.row; Position.col}) else " ")) in
    let rows_as_strings =
      List.map lists_to_print ~f:(fun row_list -> String.concat ~sep:" | " row_list ) in 
      List.iteri rows_as_strings ~f:(fun row_num row -> Core.print_endline row;
      if row_num < height -1 then Core.print_endline "-----------------------------")
end


