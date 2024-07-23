open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let dark_gray = Graphics.rgb 100 100 100
  let white = Graphics.rgb 255 255 255
  let light_gray = Graphics.rgb 200 200 200
  let _green = Graphics.rgb 000 255 000
  let _blue = Graphics.rgb 000 000 255
  (* let red = Graphics.rgb 255 000 000 *)
  let gold = Graphics.rgb 255 223 0

  let yellow = Graphics.rgb 255 248 150

  let game_in_progress = Graphics.rgb 100 100 200

end

module Constants = struct
  let scaling_factor = 1.
  let play_area_height = 600. *. scaling_factor |> Float.iround_down_exn
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let play_area_width = 600. *. scaling_factor |> Float.iround_down_exn
  let block_size = 75. *. scaling_factor |> Float.iround_down_exn
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (play_area_height + header_height)
       play_area_width);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  let game = Game.new_game ~height ~width in
  (* let one_move_game = Move.Exercises.make_move_exn ~game {Move.Exercises.Move.starting_pos = {Position.row = 0; column = 0}; Move.Exercises.Move.ending_pos = None} in
  one_move_game *)
  game
;;

(* let draw_board ~color1 ~color2 =
  List.iter  *)
let draw_block { Position.row; column } ~color  =
  let open Constants in
  let col = column * block_size in
  let row = row * block_size in
  (* Graphics.set_color color2;
  Graphics.fill_rect (col + 1) (row + 1) (block_size - 1) (block_size - 1); *)

  Graphics.set_color color;
  
  Graphics.fill_circle (row+(block_size/2)) (col+(block_size/2)) (block_size/2)
;;

let draw_header ~game_state =
  let open Constants in
  let header_color =
    match (game_state : Game.Game_state.t) with
    | First_moves -> Colors.game_in_progress
    | Game_continues -> Colors.game_in_progress
    | Game_over _ -> Colors.gold
  in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  let header_text = (match game_state with | Game.Game_state.First_moves | Game.Game_state.Game_continues -> "GAME ON!" | Game.Game_state.Game_over {winner} -> match winner with | Piece.X -> "X WINS" | Piece.O -> "O WINS") in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 20;
  Graphics.moveto 0 (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text);

;;

let draw_play_area ~board_height ~board_width =
  let open Constants in
  Map.iteri (Game.new_game ~height:board_height ~width:board_width).board ~f:(fun ~key:pos ~data:piece -> match piece with 
  | Piece.X -> Graphics.set_color Colors.light_gray;
  Graphics.fill_rect (pos.column*block_size) (pos.row*block_size) (block_size) (block_size)
  | Piece.O -> Graphics.set_color Colors.dark_gray;
  Graphics.fill_rect (pos.column*block_size) (pos.row*block_size) (block_size) (block_size))

;;


let draw_pieces board_map =
    Map.iteri board_map ~f:(fun ~key:pos ~data:piece -> match piece with | Piece.X -> draw_block pos ~color:Colors.black| Piece.O -> draw_block pos ~color:Colors.white);
    (* Snake head is a different color *)

;;

let draw_highlighted_blocks (available_moves_list:Move.Exercises.Move.t list) = 
  let open Constants in
  List.iter available_moves_list ~f:(fun move -> let row = move.starting_pos.row in let col = move.starting_pos.column in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color Colors.yellow;
  Graphics.fill_circle (row+(block_size/2)) (col+(block_size/2)) (block_size/4));;

  let undraw_highlighted_blocks (available_moves_list:Move.Exercises.Move.t list) ~init_color = 
    let open Constants in
    List.iter available_moves_list ~f:(fun move -> let row = move.starting_pos.row in let col = move.starting_pos.column in
    let col = col * block_size in
    let row = row * block_size in
    Graphics.set_color init_color;
    Graphics.fill_circle (row+(block_size/2)) (col+(block_size/2)) (block_size/4));;
  
let highlight_ending_positions (move_list:Move.Exercises.Move.t list) =
  let open Constants in
  List.iter move_list ~f:(fun move -> match move.ending_pos with | Some pos ->let row = pos.row in let col = pos.column in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color Colors.gold;
  Graphics.fill_circle (row+(block_size/2)) (col+(block_size/2)) (block_size/4) | _ -> ());;



  let mouse_in_piece_to_move_spot game =
    let open Constants in
    let mouse_pos_x, mouse_pos_y = Graphics.mouse_pos () in

    List.filter_map (Move.Exercises.available_captures_for_player game ~my_piece:game.piece_to_move) ~f:(fun move -> let row = move.starting_pos.row in let col = move.starting_pos.column in
    let col = col * block_size in
    let row = row * block_size in if (col<=mouse_pos_x && row<=mouse_pos_y && mouse_pos_x<col+block_size && mouse_pos_y<row+block_size) then Some move else None)

    let mouse_in_place_to_move ~mouse_x ~mouse_y (move_list:Move.Exercises.Move.t list) =
      let open Constants in
      List.filter_map move_list ~f:(fun move -> match move.ending_pos with | Some pos ->let row = pos.row in let col = pos.column in
      let col = col * block_size in
      let row = row * block_size in if (col<=mouse_x && row<=mouse_y && mouse_x<col+block_size && mouse_y<row+block_size) then Some move else None | _ -> None)





let render (game:Game.t)=
  (* We want double-buffering. See
     https://v2.ocaml.org/releases/4.03/htmlman/libref/Graphics.html for more
     info!

     So, we set [display_mode] to false, draw to the background buffer, set
     [display_mode] to true and then synchronize. This guarantees that there
     won't be flickering! *)
  Graphics.display_mode false;

  let game_state = game.game_state in
  let board = game.board in
  let board_width = game.board_width in
  let board_height = game.board_height in
  draw_header ~game_state;
  draw_play_area ~board_height ~board_width;
  draw_pieces board;
  draw_highlighted_blocks (Move.Exercises.available_captures_for_player game ~my_piece:game.piece_to_move);
  
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

let read_key game =
  let move_list_to_take = mouse_in_piece_to_move_spot game in
  match game.game_state with | Game.Game_state.First_moves ->
  if Graphics.button_down () && not ((List.length move_list_to_take)=0) then Some (List.hd_exn move_list_to_take) else None
  | Game.Game_state.Game_continues -> if Graphics.button_down () && not ((List.length move_list_to_take)=0) then 
    (undraw_highlighted_blocks move_list_to_take ~init_color:(match game.piece_to_move with |Piece.X -> Colors.black | Piece.O -> Colors.white);
  highlight_ending_positions move_list_to_take;
    let status = Graphics.wait_next_event [Button_down] in
    let mouse_x = status.mouse_x in let mouse_y = status.mouse_y in
    let move_to = mouse_in_place_to_move ~mouse_x ~mouse_y move_list_to_take in
    if not ((List.length move_to)=0) then Some (List.hd_exn move_to) else None

    ) else None
  | _ -> None
;;