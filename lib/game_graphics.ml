open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let dark_gray = Graphics.rgb 100 100 100
  let white = Graphics.rgb 255 255 255
  let light_gray = Graphics.rgb 200 200 200
  let _green = Graphics.rgb 000 255 000
  let _blue = Graphics.rgb 000 000 255
  let red = Graphics.rgb 255 000 000
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
  (* let one_move_game = Move.Exercises.make_move_exn ~game
     {Move.Exercises.Move.starting_pos = {Position.row = 0; column = 0};
     Move.Exercises.Move.ending_pos = None} in one_move_game *)
  game
;;

let convert row = 7 - row

(* let draw_board ~color1 ~color2 = List.iter *)
let draw_block { Position.row; column } ~color =
  let open Constants in
  let col = column * block_size in
  let row = convert row * block_size in
  (* Graphics.set_color color2; Graphics.fill_rect (col + 1) (row + 1)
     (block_size - 1) (block_size - 1); *)
  Graphics.set_color color;
  Graphics.fill_circle
    (col + (block_size / 2))
    (row + (block_size / 2))
    (block_size / 2)
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
  let header_text =
    match game_state with
    | Game.Game_state.First_moves | Game.Game_state.Game_continues ->
      "GAME ON!"
    | Game.Game_state.Game_over { winner } ->
      (match winner with Piece.X -> "X WINS" | Piece.O -> "O WINS")
  in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 20;
  Graphics.moveto 0 (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text)
;;

let draw_play_area ~board_height ~board_width =
  let open Constants in
  Map.iteri
    (Game.new_game ~height:board_height ~width:board_width).board
    ~f:(fun ~key:pos ~data:piece ->
      match piece with
      | Piece.X ->
        Graphics.set_color Colors.light_gray;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row * block_size)
          block_size
          block_size
      | Piece.O ->
        Graphics.set_color Colors.dark_gray;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row * block_size)
          block_size
          block_size)
;;

let draw_pieces board_map =
  Map.iteri board_map ~f:(fun ~key:pos ~data:piece ->
    match piece with
    | Piece.X -> draw_block pos ~color:Colors.black
    | Piece.O -> draw_block pos ~color:Colors.white)
;;

(* Snake head is a different color *)

let draw_highlighted_blocks (available_moves_list : Move.t list) =
  let open Constants in
  List.iter available_moves_list ~f:(fun move ->
    let row = move.starting_pos.row in
    let col = move.starting_pos.column in
    let col = col * block_size in
    let row = convert row * block_size in
    Graphics.set_color Colors.yellow;
    Graphics.fill_circle
      (col + (block_size / 2))
      (row + (block_size / 2))
      (block_size / 4))
;;

let undraw_highlighted_blocks
  (available_moves_list : Move.t list)
  ~init_color
  =
  let open Constants in
  List.iter available_moves_list ~f:(fun move ->
    let row = move.starting_pos.row in
    let col = move.starting_pos.column in
    let col = col * block_size in
    let row = convert row * block_size in
    Graphics.set_color init_color;
    Graphics.fill_circle
      (col + (block_size / 2))
      (row + (block_size / 2))
      (block_size / 4))
;;

let highlight_ending_positions (move_list : Move.t list) =
  let open Constants in
  List.iter move_list ~f:(fun move ->
    match move.ending_pos with
    | Some pos ->
      let row = pos.row in
      let col = pos.column in
      let col = col * block_size in
      let row = convert row * block_size in
      Graphics.set_color Colors.gold;
      Graphics.fill_circle
        (col + (block_size / 2))
        (row + (block_size / 2))
        (block_size / 4)
    | _ -> ())
;;

let display_win_message winner =
  let open Constants in
  Graphics.set_color Colors.yellow;
  Graphics.fill_ellipse
    (play_area_width / 2)
    (play_area_height / 2)
    (block_size * 2)
    block_size;
  Graphics.set_color Colors.gold;
  Graphics.fill_ellipse
    (play_area_width / 2)
    (play_area_height / 2)
    (block_size * 2)
    block_size;
  Graphics.moveto
    ((play_area_width / 2) - (2 * 16))
    ((play_area_height / 2) - 8);
  Graphics.set_color Colors.black;
  Graphics.set_text_size 36;
  Graphics.draw_string
    (match winner with Piece.X -> "BLACK WINS!" | Piece.O -> "WHITE WINS!")
;;

let mouse_in_piece_to_move_spot (game : Game.t) =
  let open Constants in
  let mouse_pos_x, mouse_pos_y = Graphics.mouse_pos () in
  match game.last_move_from_piece_to_move with
  | None ->
    List.filter_map
      (Game.available_captures_for_player game ~my_piece:game.piece_to_move)
      ~f:(fun move ->
        let row = move.starting_pos.row in
        let col = move.starting_pos.column in
        let col = col * block_size in
        let row = convert row * block_size in
        if col <= mouse_pos_x
           && row <= mouse_pos_y
           && mouse_pos_x < col + block_size
           && mouse_pos_y < row + block_size
        then Some move
        else None)
  | Some move ->
    (match move.ending_pos with
     | None -> []
     | Some pos ->
       List.filter_map
         (Game.possible_captures_from_occupied_pos_exn
            ?dir_opt:move.dir
            game
            pos)
         ~f:(fun move ->
           let row = move.starting_pos.row in
           let col = move.starting_pos.column in
           let col = col * block_size in
           let row = convert row * block_size in
           if col <= mouse_pos_x
              && row <= mouse_pos_y
              && mouse_pos_x < col + block_size
              && mouse_pos_y < row + block_size
           then Some move
           else None))
;;

(* call available moves from move.ending_pos and return the list *)

let mouse_in_place_to_move ~mouse_x ~mouse_y (move_list : Move.t list) =
  let open Constants in
  List.filter_map move_list ~f:(fun move ->
    match move.ending_pos with
    | Some pos ->
      let row = pos.row in
      let col = pos.column in
      let col = col * block_size in
      let row = convert row * block_size in
      if col <= mouse_x
         && row <= mouse_y
         && mouse_x < col + block_size
         && mouse_y < row + block_size
      then Some move
      else None
    | _ -> None)
;;

let draw_end_turn_button () =
  let open Constants in
  Graphics.set_color Colors._green;
  Graphics.fill_rect play_area_width 0 block_size (block_size / 2);
  Graphics.set_color Colors.black;
  Graphics.moveto (play_area_width + (block_size / 4)) (block_size / 8);
  Graphics.draw_string "END TURN"
;;

let undraw_end_turn_button (game : Game.t) =
  let open Constants in
  Graphics.set_color Colors.white;
  Graphics.fill_rect play_area_width 0 block_size (block_size / 2);
  Graphics.set_color Colors.red;
  Graphics.moveto (play_area_width + (block_size / 8)) (block_size / 8);
  Graphics.draw_string
    (match game.piece_to_move with
     | Piece.X -> "Black Move"
     | Piece.O -> "White Move")
;;

let mouse_in_end_move_button () =
  let open Constants in
  let mouse_x, mouse_y = Graphics.mouse_pos () in
  mouse_x > play_area_width
  && mouse_x < play_area_width + block_size
  && mouse_y < block_size / 2
  && mouse_y > 0
;;

let render (game : Game.t) =
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
  (match game.game_state with
   | Game_over { winner } -> display_win_message winner
   | _ ->
     (match game.last_move_from_piece_to_move with
      | None ->
        draw_highlighted_blocks
          (Game.available_captures_for_player
             game
             ~my_piece:game.piece_to_move);
        undraw_end_turn_button game
      | Some move ->
        draw_end_turn_button ();
        draw_highlighted_blocks
          (match move.ending_pos with
           | None -> []
           | Some pos ->
             Game.possible_captures_from_occupied_pos_exn
               ?dir_opt:move.dir
               game
               pos)));
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

(* (match move.ending_pos with | None -> [] | Some pos ->
   Game.possible_captures_from_occupied_pos_exn ?dir_opt:move.dir game
   pos) *)

module Action = struct
  type t =
    | Move of Move.t
    | Restart
    | End_turn
    | None
end

let read_key (game : Game.t) : Action.t =
  let move_list_to_take = mouse_in_piece_to_move_spot game in
  if Graphics.button_down ()
  then
    if not (List.length move_list_to_take = 0)
    then (
      match game.game_state with
      | Game.Game_state.First_moves -> Move (List.hd_exn move_list_to_take)
      | Game.Game_state.Game_continues ->
        undraw_highlighted_blocks
          move_list_to_take
          ~init_color:
            (match game.piece_to_move with
             | Piece.X -> Colors.black
             | Piece.O -> Colors.white);
        highlight_ending_positions move_list_to_take;
        let status = Graphics.wait_next_event [ Button_down ] in
        let mouse_x = status.mouse_x in
        let mouse_y = status.mouse_y in
        let move_to =
          mouse_in_place_to_move ~mouse_x ~mouse_y move_list_to_take
        in
        if not (List.length move_to = 0)
        then Move (List.hd_exn move_to)
        else None
      | _ ->
        None
        (* else if Graphics.key_pressed () then if Char.equal
           (Graphics.read_key ()) 'r' then Restart else None *))
    else if mouse_in_end_move_button ()
    then (
      match game.last_move_from_piece_to_move with
      | None -> None
      | Some _ -> End_turn)
    else None
  else None
;;
