open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let dark_gray = Graphics.rgb 100 100 100
  let white = Graphics.rgb 255 255 255
  let light_gray = Graphics.rgb 200 200 200
  let _green = Graphics.rgb 000 255 000
  let _blue = Graphics.rgb 000 000 255
  let _red = Graphics.rgb 255 000 000
  let light_red = Graphics.rgb 255 132 130
  let gold = Graphics.rgb 255 223 0
  let _yellow = Graphics.rgb 255 248 150
  let game_in_progress = Graphics.rgb 100 100 200
  let _brown = Graphics.rgb 212 134 0
  let _light_blue = Graphics.rgb 88 182 237
  let _darker_green = Graphics.rgb 53 166 53
end

module Constants = struct
  let scaling_factor = 1.

  (* let play_area_height = 600. *. scaling_factor |>
     Float.iround_down_exn *)
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn

  (* let play_area_width = 600. *. scaling_factor |> Float.iround_down_exn *)
  let block_size = 75. *. scaling_factor |> Float.iround_down_exn
end

let only_one : bool ref = ref false

let init_exn (game_config : Game_config.t) =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.set_window_title "Welcome to Konane!";
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (block_size * game_config.width)
       ((block_size * game_config.height) + header_height));
  (* let height = play_area_height / block_size in let width =
     play_area_width / block_size in *)
  let game =
    Game.new_game ~height:game_config.height ~width:game_config.width ()
  in
  (* let one_move_game = Move.Exercises.make_move_exn ~game
     {Move.Exercises.Move.starting_pos = {Position.row = 0; column = 0};
     Move.Exercises.Move.ending_pos = None} in one_move_game *)
  game
;;

let convert row ~board_height = board_height - 1 - row

(* let draw_board ~color1 ~color2 = List.iter *)
let draw_block { Position.row; column } ~color ~board_height =
  let open Constants in
  let col = column * block_size in
  let row = convert row ~board_height * block_size in
  (* Graphics.set_color color2; Graphics.fill_rect (col + 1) (row + 1)
     (block_size - 1) (block_size - 1); *)
  Graphics.set_color color;
  Graphics.fill_circle
    (col + (block_size / 2))
    (row + (block_size / 2))
    (block_size / 2)
;;

let draw_header ~piece_to_move ~game_state ~player ~board_height ~board_width
  =
  let open Constants in
  let header_color =
    match (game_state : Game.Game_state.t) with
    | First_moves -> Colors.game_in_progress
    | Game_continues -> Colors.game_in_progress
    | Game_over { winner } ->
      if Piece.equal winner (Player.get_piece player)
      then Colors._darker_green
      else Colors._red
  in
  Graphics.set_color header_color;
  Graphics.fill_rect
    0
    (block_size * board_height)
    (board_width * block_size)
    header_height;
  let header_text =
    match game_state with
    | Game.Game_state.First_moves | Game.Game_state.Game_continues ->
      if Piece.equal (Player.get_piece player) piece_to_move
      then "         MAKE YOUR MOVE"
      else "   WAITING FOR OPPONENT MOVE"
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

let draw_play_area ~board_height ~board_width =
  let open Constants in
  Map.iteri
    (Game.new_game ~height:board_height ~width:board_width ()).board
    ~f:(fun ~key:pos ~data:piece ->
      match piece with
      | Piece.X ->
        Graphics.set_color Colors.light_gray;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row ~board_height * block_size)
          block_size
          block_size
      | Piece.O ->
        Graphics.set_color Colors.dark_gray;
        Graphics.fill_rect
          (pos.column * block_size)
          (convert pos.row ~board_height * block_size)
          block_size
          block_size)
;;

let draw_pieces board_map ~board_height =
  Map.iteri board_map ~f:(fun ~key:pos ~data:piece ->
    match piece with
    | Piece.X -> draw_block pos ~color:Colors.black ~board_height
    | Piece.O -> draw_block pos ~color:Colors.white ~board_height)
;;

(* Snake head is a different color *)

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

let undraw_highlighted_blocks
  (available_moves_list : Move.t list)
  ~init_color
  ~board_height
  =
  let open Constants in
  List.iter available_moves_list ~f:(fun move ->
    let row = move.starting_pos.row in
    let col = move.starting_pos.column in
    let col = col * block_size in
    let row = convert row ~board_height * block_size in
    Graphics.set_color init_color;
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
  else Graphics.set_color Colors._red;
  Graphics.fill_ellipse
    (board_width * block_size / 2)
    (board_height * block_size / 2)
    (block_size * 2)
    block_size;
  if Piece.equal winner (Player.get_piece player)
  then Graphics.set_color Colors._green
  else Graphics.set_color Colors.light_red;
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
        | _ -> "ERROR"))
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

let undraw_end_turn_button (_game : Game.t) ~board_height ~board_width =
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

let render (client_state : Client.t) =
  (* We want double-buffering. See
     https://v2.ocaml.org/releases/4.03/htmlman/libref/Graphics.html for more
     info!

     So, we set [display_mode] to false, draw to the background buffer, set
     [display_mode] to true and then synchronize. This guarantees that there
     won't be flickering! *)
  Graphics.display_mode false;
  (* let current_window_width = Graphics.size_x () in let
     current_window_height = Graphics.size_y () in *)
  let game_state = client_state.game.game_state in
  let board = client_state.game.board in
  let board_width = client_state.game.board_width in
  let board_height = client_state.game.board_height in
  let player1 = client_state.game.player1 in
  let player2 = client_state.game.player2 in
  draw_header
    ~piece_to_move:client_state.game.piece_to_move
    ~game_state
    ~player:client_state.player
    ~board_height
    ~board_width;
  draw_play_area ~board_height ~board_width;
  draw_pieces board ~board_height;
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
       then (
         undraw_highlighted_blocks
           client_state.moves_to_highlight
           ~init_color:
             (match client_state.game.piece_to_move with
              | Piece.X -> Colors.black
              | Piece.O -> Colors.white)
           ~board_height;
         highlight_ending_positions
           client_state.moves_to_highlight
           ~board_height);
       match client_state.game.last_move_from_piece_to_move with
       | None ->
         draw_highlighted_blocks
           (Game.available_captures_for_player
              client_state.game
              ~my_piece:client_state.game.piece_to_move)
           ~board_height;
         undraw_end_turn_button client_state.game ~board_height ~board_width
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
        (* undraw_highlighted_blocks move_list_to_take ~init_color: (match
           game.piece_to_move with | Piece.X -> Colors.black | Piece.O ->
           Colors.white); highlight_ending_positions move_list_to_take; *)
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
