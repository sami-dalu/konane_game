open! Core

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)
let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let handle_keys (game : Game.t) ~game_over =
  every ~stop:game_over 0.001 ~f:(fun () ->
    Game_graphics.render game;
    match Game_graphics.read_key game with
    | None -> ()
    (* | Some 'r' -> Game.restart ~height:600 ~width:675
       ~initial_snake_length:2 *)
    | Some move ->
      Game.make_move_exn ~game move;
      Game_graphics.render game;
      (match move.ending_pos with
       | None -> game.piece_to_move <- Piece.flip game.piece_to_move
       | Some pos ->
         let possible_moves =
           Game.possible_captures_from_occupied_pos_exn
             ?dir_opt:move.dir
             game
             pos
         in
         if List.length possible_moves = 0
         then (
           game.piece_to_move <- Piece.flip game.piece_to_move;
           game.last_move_from_piece_to_move <- None)
         else game.last_move_from_piece_to_move <- Some move)
    (* Game_graphics.render game) *))
;;

(* let handle_steps (game : Game.t) ~game_over = every ~stop:game_over 0.1
   ~f:(fun () -> Game.step game; Snake_graphics.render game; match
   Game.game_state game with | Game_over _ | Win -> game_over := true |
   In_progress -> ()) *)
let run () =
  let game = Game_graphics.init_exn () in
  Game_graphics.render game;
  let game_over = ref false in
  handle_keys game ~game_over
;;
(* handle_steps game ~game_over *)
