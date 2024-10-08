open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let dark_gray = Graphics.rgb 100 100 100
  let white = Graphics.rgb 255 255 255
  let light_gray = Graphics.rgb 200 200 200
  let _green = Graphics.rgb 000 255 000
  let _blue = Graphics.rgb 000 000 255
  let _red = Graphics.rgb 255 000 000
  let light_red = Graphics.rgb 255 74 77
  let gold = Graphics.rgb 255 223 0
  let _yellow = Graphics.rgb 255 248 150
  let game_in_progress = Graphics.rgb 100 100 200
  let _brown = Graphics.rgb 212 134 0
  let _light_blue = Graphics.rgb 88 182 237
  let _darker_green = Graphics.rgb 53 166 53
  let light_pink = Graphics.rgb 255 201 202
  let darker_pink = Graphics.rgb 252 146 148
  let wither_color = Graphics.rgb 103 153 108
  let purple = Graphics.rgb 244 120 255
  let _dark_purple = Graphics.rgb 116 3 153
  let _orange = Graphics.rgb 250 157 35
  let _darker_purple = Graphics.rgb 69 0 110
end

module Constants = struct
  let scaling_factor = 1.
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let block_size = 75. *. scaling_factor |> Float.iround_down_exn
end

let only_one : bool ref = ref false

let init_exn (game_config : Game_config.t) =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.set_window_title
    "Welcome to Konane! Wating for Opponent to Connect";
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (block_size * game_config.width)
       ((block_size * game_config.height) + header_height));
  let game =
    Game.new_game ~height:game_config.height ~width:game_config.width ()
  in
  game
;;

let convert row ~board_height = board_height - 1 - row

let draw_block { Position.row; column } ~color ~board_height =
  let open Constants in
  let col = column * block_size in
  let row = convert row ~board_height * block_size in
  Graphics.set_color color;
  Graphics.fill_circle
    (col + (block_size / 2))
    (row + (block_size / 2))
    (block_size / 2)
;;

let draw_smaller_block { Position.row; column } ~color ~board_height =
  let open Constants in
  let col = column * block_size in
  let row = convert row ~board_height * block_size in
  Graphics.set_color color;
  Graphics.fill_circle
    (col + (block_size / 2))
    (row + (block_size / 2))
    (block_size / 3)
;;

let draw_header
  ~(game : Game.t)
  ~piece_to_move
  ~game_state
  ~player
  ~board_height
  ~board_width
  =
  let open Constants in
  let header_color =
    match (game_state : Game.Game_state.t) with
    | First_moves -> Colors.game_in_progress
    | Game_continues -> Colors.game_in_progress
    | Game_over { winner } ->
      if Piece.equal winner (Player.get_piece player)
      then Colors._darker_green
      else Colors.light_red
  in
  Graphics.set_color header_color;
  (match game.crazy_info with
   | None -> ()
   | Some _ -> Graphics.set_color Colors.light_pink);
  Graphics.fill_rect
    0
    (block_size * board_height)
    (board_width * block_size)
    header_height;
  Graphics.set_color Colors.black;
  Graphics.draw_rect
    0
    (block_size * board_height)
    (board_width * block_size)
    header_height;
  let header_text =
    match game_state with
    | Game.Game_state.First_moves | Game.Game_state.Game_continues ->
      (match game.player2 with
       | None -> "WAITING FOR OPPONENT TO CONNECT"
       | _ ->
         if Piece.equal (Player.get_piece player) piece_to_move
         then "         MAKE YOUR MOVE"
         else "   WAITING FOR OPPONENT MOVE")
    | Game.Game_state.Game_over { winner } ->
      (match Piece.equal winner (Player.get_piece player) with
       | true -> "           YOU WIN"
       | false -> "          YOU LOSE")
  in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 20;
  Graphics.moveto
    ((board_width * block_size / 2) - 100)
    ((board_height * block_size) + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text)
;;

let draw_play_area ~board_height ~board_width ~inverse_board =
  let open Constants in
  Map.iteri
    (Game.new_game ~height:board_height ~width:board_width ()).board
    ~f:(fun ~key:pos ~data:piece ->
      match piece with
      | Piece.X ->
        if inverse_board
        then Graphics.set_color Colors.dark_gray
        else Graphics.set_color Colors.light_gray;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row ~board_height * block_size)
          block_size
          block_size
      | Piece.O ->
        if inverse_board
        then Graphics.set_color Colors.light_gray
        else Graphics.set_color Colors.dark_gray;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row ~board_height * block_size)
          block_size
          block_size
      | _ ->
        Graphics.set_color Colors.light_red;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row ~board_height * block_size)
          block_size
          block_size)
;;

let draw_monster_stuff { Position.row; column } ~board_height =
  let open Constants in
  let col = column * block_size in
  let row = convert row ~board_height * block_size in
  Graphics.set_color Colors._darker_purple;
  Graphics.fill_arc
    (col + (block_size / 2) - (block_size / 4))
    (row + (block_size / 2))
    (block_size / 8)
    (block_size / 8)
    180
    360;
  Graphics.fill_arc
    (col + (block_size / 2) + (block_size / 4))
    (row + (block_size / 2))
    (block_size / 8)
    (block_size / 8)
    180
    360;
  Graphics.fill_arc
    (col + (block_size / 2))
    (row + (block_size / 2) - (block_size * 3 / 8))
    (block_size / 4)
    (block_size / 4)
    0
    180
;;

let draw_lava_stuff { Position.row; column } ~board_height =
  let open Constants in
  let col = column * block_size in
  let row = convert row ~board_height * block_size in
  Graphics.set_color Colors._yellow;
  (* Graphics.fill_arc (col + (block_size / 2) - (block_size / 4)) (row +
     (block_size / 2)) (block_size / 8) (block_size / 8) 180 360;
     Graphics.fill_arc (col + (block_size / 2) + (block_size / 4)) (row +
     (block_size / 2)) (block_size / 8) (block_size / 8) 180 360; *)
  Graphics.fill_arc
    (col + (block_size / 2))
    (row + (block_size / 2) + (block_size / 8) + 8)
    (block_size / 16)
    (block_size / 8)
    270
    450;
  Graphics.fill_arc
    (col + (block_size / 2))
    (row + (block_size / 2) - (block_size / 8) + 8)
    (block_size / 8)
    (block_size / 4)
    90
    270;
  Graphics.fill_arc
    (col + (block_size / 2))
    (row + (block_size / 2) - (block_size / 8) + 8)
    (block_size / 8)
    (block_size / 4)
    135
    315;
  Graphics.fill_arc
    (col + (block_size / 2) - (block_size / 8))
    (row + (block_size / 2) + (block_size / 8) + 8)
    (block_size / 16)
    (block_size / 8)
    270
    450;
  Graphics.fill_arc
    (col + (block_size / 2) - (block_size / 8))
    (row + (block_size / 2) - (block_size / 8) + 8)
    (block_size / 8)
    (block_size / 4)
    90
    270;
  Graphics.fill_arc
    (col + (block_size / 2) - (block_size / 8))
    (row + (block_size / 2) - (block_size / 8) + 8)
    (block_size / 8)
    (block_size / 4)
    135
    315;
  Graphics.fill_arc
    (col + (block_size / 2) + (block_size / 8))
    (row + (block_size / 2) + (block_size / 8) + 8)
    (block_size / 16)
    (block_size / 8)
    270
    450;
  Graphics.fill_arc
    (col + (block_size / 2) + (block_size / 8))
    (row + (block_size / 2) - (block_size / 8) + 8)
    (block_size / 8)
    (block_size / 4)
    90
    270;
  Graphics.fill_arc
    (col + (block_size / 2) + (block_size / 8))
    (row + (block_size / 2) - (block_size / 8) + 8)
    (block_size / 8)
    (block_size / 4)
    135
    315
;;

let draw_pieces board_map ~board_height =
  Map.iteri board_map ~f:(fun ~key:pos ~data:piece ->
    match piece with
    | Piece.X -> draw_block pos ~color:Colors.black ~board_height
    | Piece.O -> draw_block pos ~color:Colors.white ~board_height
    | Piece.Obstacle ->
      draw_block pos ~color:Colors._red ~board_height;
      draw_smaller_block pos ~color:Colors._orange ~board_height;
      draw_lava_stuff pos ~board_height
    | Monster ->
      draw_block pos ~color:Colors._dark_purple ~board_height;
      draw_monster_stuff pos ~board_height)
;;

let draw_highlighted_blocks
  (available_moves_list : Move.t list)
  ~board_height
  =
  let open Constants in
  List.iter available_moves_list ~f:(fun move ->
    let row = move.starting_pos.row in
    let col = move.starting_pos.column in
    let col = col * block_size in
    let row = convert row ~board_height * block_size in
    Graphics.set_color Colors._light_blue;
    Graphics.fill_circle
      (col + (block_size / 2))
      (row + (block_size / 2))
      (block_size / 4))
;;

let highlight_ending_positions (move_list : Move.t list) ~board_height =
  let open Constants in
  List.iter move_list ~f:(fun move ->
    match move.ending_pos with
    | Some pos ->
      let row = pos.row in
      let col = pos.column in
      let col = col * block_size in
      let row = convert row ~board_height * block_size in
      Graphics.set_color Colors.gold;
      Graphics.fill_circle
        (col + (block_size / 2))
        (row + (block_size / 2))
        (block_size / 4)
    | _ -> ())
;;

let display_win_message
  winner
  player
  ~board_height
  ~board_width
  ~player1
  ~player2
  =
  let open Constants in
  if Piece.equal winner (Player.get_piece player)
  then Graphics.set_color Colors._darker_green
  else Graphics.set_color Colors.light_red;
  Graphics.fill_ellipse
    (board_width * block_size / 2)
    (board_height * block_size / 2)
    (block_size * 2)
    block_size;
  if Piece.equal winner (Player.get_piece player)
  then Graphics.set_color Colors._green
  else Graphics.set_color Colors._red;
  Graphics.draw_ellipse
    (board_width * block_size / 2)
    (board_height * block_size / 2)
    (block_size * 2)
    block_size;
  Graphics.moveto
    ((board_width * block_size / 2) - (2 * 16))
    ((board_height * block_size / 2) - 8);
  Graphics.set_color Colors.black;
  Graphics.set_text_size 36;
  Graphics.draw_string
    (match winner with
     | Piece.X ->
       (match player1 with
        | Some p -> Player.get_name p ^ " WINS!"
        | _ -> "ERROR")
     | Piece.O ->
       (match player2 with
        | Some p -> Player.get_name p ^ " WINS!"
        | _ -> "ERROR")
     | _ -> "lol what")
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
        let row = convert row ~board_height:game.board_height * block_size in
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
           let row =
             convert row ~board_height:game.board_height * block_size
           in
           if col <= mouse_pos_x
              && row <= mouse_pos_y
              && mouse_pos_x < col + block_size
              && mouse_pos_y < row + block_size
           then Some move
           else None))
;;

(* call available moves from move.ending_pos and return the list *)

let mouse_in_place_to_move
  ~mouse_x
  ~mouse_y
  (move_list : Move.t list)
  ~board_height
  =
  let open Constants in
  List.filter_map move_list ~f:(fun move ->
    match move.ending_pos with
    | Some pos ->
      let row = pos.row in
      let col = pos.column in
      let col = col * block_size in
      let row = convert row ~board_height * block_size in
      if col <= mouse_x
         && row <= mouse_y
         && mouse_x < col + block_size
         && mouse_y < row + block_size
      then Some move
      else None
    | _ -> None)
;;

let mouse_in_spot_just_clicked
  ~mouse_x
  ~mouse_y
  (move : Move.t)
  ~board_height
  =
  let open Constants in
  let pos = move.starting_pos in
  let row = pos.row in
  let col = pos.column in
  let col = col * block_size in
  let row = convert row ~board_height * block_size in
  if col <= mouse_x
     && row <= mouse_y
     && mouse_x < col + block_size
     && mouse_y < row + block_size
  then true
  else false
;;

let draw_restart_button ~board_height =
  let open Constants in
  Graphics.set_color Colors.light_red;
  Graphics.fill_rect
    20
    ((board_height * block_size) + (block_size / 4))
    block_size
    (block_size / 2);
  Graphics.set_color Colors.black;
  Graphics.draw_rect
    20
    ((board_height * block_size) + (block_size / 4))
    block_size
    (block_size / 2);
  Graphics.set_color Colors.black;
  Graphics.moveto 23 ((board_height * block_size) + (block_size * 3 / 8));
  Graphics.draw_string "RESTART GAME"
;;

let _undraw_restart_button (_game : Game.t) ~board_height =
  let open Constants in
  Graphics.set_color Colors.game_in_progress;
  Graphics.fill_rect
    20
    ((board_height * block_size) + (block_size / 4))
    block_size
    (block_size / 2)
;;

let mouse_in_restart_button ~board_height =
  let open Constants in
  let mouse_x, mouse_y = Graphics.mouse_pos () in
  mouse_x > 20
  && mouse_x < 20 + block_size
  && mouse_y
     < (board_height * block_size) + (block_size / 4) + (block_size / 2)
  && mouse_y > (board_height * block_size) + (block_size / 4)
;;

let draw_end_turn_button ~board_height ~board_width =
  let open Constants in
  Graphics.set_color Colors._darker_green;
  Graphics.fill_rect
    ((board_width * block_size) - (block_size * 3 / 2))
    ((board_height * block_size) + (block_size / 4))
    block_size
    (block_size / 2);
  Graphics.set_color Colors.black;
  Graphics.draw_rect
    ((board_width * block_size) - (block_size * 3 / 2))
    ((board_height * block_size) + (block_size / 4))
    block_size
    (block_size / 2);
  Graphics.set_color Colors.black;
  Graphics.moveto
    ((board_width * block_size) - (block_size * 3 / 2) + (block_size * 3 / 16)
    )
    ((board_height * block_size) + (block_size * 3 / 8));
  Graphics.draw_string "END TURN"
;;

let _undraw_end_turn_button (_game : Game.t) ~board_height ~board_width =
  let open Constants in
  Graphics.set_color Colors.game_in_progress;
  Graphics.fill_rect
    ((board_width * block_size) - (block_size * 3 / 2))
    ((board_height * block_size) + (block_size / 4))
    block_size
    (block_size / 2)
;;

let mouse_in_end_move_button ~board_height ~board_width =
  let open Constants in
  let mouse_x, mouse_y = Graphics.mouse_pos () in
  mouse_x > (board_width * block_size) - (block_size * 3 / 2)
  && mouse_x < (board_width * block_size) - (block_size * 3 / 2) + block_size
  && mouse_y
     < (board_height * block_size) + (block_size / 4) + (block_size / 2)
  && mouse_y > (board_height * block_size) + (block_size / 4)
;;

let draw_opp_last_move (last_move : Move.t option) ~board_height =
  let open Constants in
  match last_move with
  | Some move ->
    let starting_pos = move.starting_pos in
    let srow = starting_pos.row in
    let scol = starting_pos.column in
    let srow = convert srow ~board_height * block_size in
    let scol = scol * block_size in
    Graphics.set_color Colors.light_pink;
    Graphics.fill_circle
      (scol + (block_size / 2))
      (srow + (block_size / 2))
      (block_size / 4);
    (match move.ending_pos with
     | Some pos ->
       let row = pos.row in
       let col = pos.column in
       let col = col * block_size in
       let row = convert row ~board_height * block_size in
       Graphics.set_color Colors.darker_pink;
       Graphics.fill_circle
         (col + (block_size / 2))
         (row + (block_size / 2))
         (block_size / 4)
     | _ -> ())
  | None -> ()
;;

let _undraw_opp_last_move
  (last_move : Move.t option)
  ~board_height
  ~init_color_board
  ~init_color_piece
  =
  let open Constants in
  match last_move with
  | Some move ->
    let starting_pos = move.starting_pos in
    let srow = starting_pos.row in
    let scol = starting_pos.column in
    let srow = convert srow ~board_height * block_size in
    let scol = scol * block_size in
    Graphics.set_color init_color_board;
    Graphics.fill_circle
      (scol + (block_size / 2))
      (srow + (block_size / 2))
      (block_size / 4);
    (match move.ending_pos with
     | Some pos ->
       let row = pos.row in
       let col = pos.column in
       let col = col * block_size in
       let row = convert row ~board_height * block_size in
       Graphics.set_color init_color_piece;
       Graphics.fill_circle
         (col + (block_size / 2))
         (row + (block_size / 2))
         (block_size / 4)
     | _ -> ())
  | None -> ()
;;

let draw_withering_pieces ~board_height ~(crazy : Crazy_info.t) =
  let open Constants in
  let withered_pieces_list = crazy.withered_pieces_list in
  List.iter withered_pieces_list ~f:(fun (pos, _timer) ->
    let row = pos.row in
    let col = pos.column in
    let col = col * block_size in
    let row = convert row ~board_height * block_size in
    Graphics.set_color Colors.wither_color;
    Graphics.fill_circle
      (col + (block_size / 2))
      (row + (block_size / 2))
      (block_size / 3))
;;

let _draw_info_slide ~board_height ~board_width _crazy_info =
  let open Constants in
  Graphics.moveto (board_width * block_size) (board_height * block_size);
  Graphics.draw_string
    "Click on blue and yellow circles to move. Click the red Restart button \
     to reset the game. Click the green End Turn button to cancel a double \
     move early. Pink circles show the last move of your opponent."
;;

let display_event_message event ~board_height ~board_width =
  let open Constants in
  Graphics.set_color Colors.purple;
  Graphics.fill_ellipse
    (board_width * block_size / 2)
    (board_height * block_size / 2)
    (block_size * 2)
    block_size;
  Graphics.set_color Colors.purple;
  Graphics.draw_ellipse
    (board_width * block_size / 2)
    (board_height * block_size / 2)
    (block_size * 2)
    block_size;
  Graphics.moveto
    (board_width * block_size * 3 / 8)
    ((board_height * block_size / 2) + 10);
  Graphics.set_color Colors.black;
  Graphics.set_text_size 36;
  match event with
  | Crazy_info.Event.Eruption ->
    Graphics.draw_string "A volcanic eruption has";
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 19)
      ((board_height * block_size / 2) - 7);
    Graphics.draw_string "caused magma to cover spaces!"
  | Crazy_info.Event.Monster ->
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 25)
      ((board_height * block_size / 2) - 10);
    Graphics.draw_string "An Evil Spirit is loose! Fight or flee!"
  | Crazy_info.Event.Plague ->
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 5)
      ((board_height * block_size / 2) + 10);
    Graphics.draw_string "A sickness has spread!";
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 20)
      ((board_height * block_size / 2) - 10);
    Graphics.draw_string "Affected pieces will die shortly!"
  | Crazy_info.Event.Duplicates ->
    Graphics.moveto
      (board_width * block_size * 3 / 8)
      ((board_height * block_size / 2) + 10);
    Graphics.draw_string "Duplication time!";
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 25)
      ((board_height * block_size / 2) - 10);
    Graphics.draw_string "Moving pieces will leave a duplicate"
  | Flip_all ->
    Graphics.draw_string "Let's shake things up!";
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 20)
      ((board_height * block_size / 2) - 10);
    Graphics.draw_string "All pieces have been swapped"
  | Rotate ->
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 20)
      ((board_height * block_size / 2) + 10);
    Graphics.draw_string "Now for a change in perspective";
    Graphics.moveto
      ((board_width * block_size * 3 / 8) - 5)
      ((board_height * block_size / 2) - 10);
    Graphics.draw_string "The board has been rotated!"
  | Impending_start -> Graphics.draw_string "LETS GET CRAZY!!!"
;;

let render (client_state : Client.t) =
  let open Constants in
  Graphics.display_mode false;
  let game_state = client_state.game.game_state in
  let board = client_state.game.board in
  let board_width = client_state.game.board_width in
  let board_height = client_state.game.board_height in
  let player1 = client_state.game.player1 in
  let player2 = client_state.game.player2 in
  Graphics.resize_window
    (board_width * block_size)
    ((board_height * block_size) + header_height);
  (match player1, player2 with
   | Some p1, Some p2 ->
     if Player.equal client_state.player p1
     then
       Graphics.set_window_title
         (Player.get_name p1 ^ " versus " ^ Player.get_name p2)
     else
       Graphics.set_window_title
         (Player.get_name p2 ^ " versus " ^ Player.get_name p1)
   | _, _ -> ());
  draw_header
    ~game:client_state.game
    ~piece_to_move:client_state.game.piece_to_move
    ~game_state
    ~player:client_state.player
    ~board_height
    ~board_width;
  draw_play_area
    ~board_height
    ~board_width
    ~inverse_board:client_state.game.inverse_board;
  draw_pieces board ~board_height;
  (match client_state.game.crazy_info with
   | None -> ()
   | Some crazy -> draw_withering_pieces ~board_height ~crazy);
  draw_restart_button ~board_height;
  (match client_state.game.game_state with
   | Game_over { winner } ->
     display_win_message
       winner
       client_state.player
       ~board_height
       ~board_width
       ~player1
       ~player2
   | _ ->
     if Piece.equal
          client_state.game.piece_to_move
          (Player.get_piece client_state.player)
     then (
       if not (List.length client_state.moves_to_highlight = 0)
       then
         highlight_ending_positions
           client_state.moves_to_highlight
           ~board_height
       else
         draw_opp_last_move client_state.game.last_move_played ~board_height;
       match client_state.game.last_move_from_piece_to_move with
       | None ->
         draw_highlighted_blocks
           (Game.available_captures_for_player
              client_state.game
              ~my_piece:client_state.game.piece_to_move)
           ~board_height
       | Some move ->
         draw_end_turn_button ~board_height ~board_width;
         draw_highlighted_blocks
           (match move.ending_pos with
            | None -> []
            | Some pos ->
              Game.possible_captures_from_occupied_pos_exn
                ?dir_opt:move.dir
                client_state.game
                pos)
           ~board_height));
  (match client_state.last_event with
   | None -> ()
   | Some event ->
     display_event_message event ~board_height ~board_width;
     client_state.last_event <- None);
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

module Action = struct
  type t =
    | Move of Move.t
    | Restart
    | End_turn
    | None
end

let read_key (client_state : Client.t) : Action.t =
  let move_list_to_take = mouse_in_piece_to_move_spot client_state.game in
  if Graphics.button_down ()
  then
    if (not (List.length move_list_to_take = 0))
       || not (List.length client_state.moves_to_highlight = 0)
    then (
      match client_state.game.game_state with
      | Game.Game_state.First_moves ->
        Graphics.sound 20 1000;
        Move (List.hd_exn move_list_to_take)
      | Game.Game_state.Game_continues ->
        if not (List.length client_state.moves_to_highlight = 0)
        then (
          Core.print_endline "confirming move";
          let mouse_x, mouse_y = Graphics.mouse_pos () in
          let move_taken = List.hd_exn client_state.moves_to_highlight in
          (* if x and y are in starting pos of move taken then return None*)
          if mouse_in_spot_just_clicked
               move_taken
               ~mouse_x
               ~mouse_y
               ~board_height:client_state.game.board_height
          then None
          else (
            let move_to =
              mouse_in_place_to_move
                ~mouse_x
                ~mouse_y
                client_state.moves_to_highlight
                ~board_height:client_state.game.board_height
            in
            client_state.moves_to_highlight <- [];
            if not (List.length move_to = 0)
            then (
              Graphics.sound 20 1000;
              Move (List.hd_exn move_to))
            else None))
        else (
          Core.print_endline "initializing move";
          client_state.moves_to_highlight <- move_list_to_take;
          None)
      | _ -> None)
    else if mouse_in_end_move_button
              ~board_height:client_state.game.board_height
              ~board_width:client_state.game.board_width
    then (
      match client_state.game.last_move_from_piece_to_move with
      | None -> None
      | Some _ -> End_turn)
    else if mouse_in_restart_button
              ~board_height:client_state.game.board_height
    then (
      Graphics.sound 300 1000;
      Restart)
    else None
  else None
;;
