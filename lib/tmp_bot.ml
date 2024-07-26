open! Core

let rec make_move_no_mutate ~(game : Game.t) (move : Move.t) =
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
       | Game.Game_state.First_moves ->
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
      then Game.Game_state.Game_continues
      else Game.Game_state.First_moves
    | _ -> game.game_state
  in
  let new_game =
    { Game.board_height = game.board_height
    ; board_width = game.board_width
    ; board = new_board
    ; game_state = new_game_state
    ; piece_to_move = game.piece_to_move (* no flip yet *)
    ; last_move_from_piece_to_move = None
    }
  in
  match move.dir, move.ending_pos with
  | Some dir, Some pos ->
    let possible_moves_after_initial_move =
      Game.possible_captures_from_occupied_pos_exn ~dir_opt:dir new_game pos
    in
    if List.length possible_moves_after_initial_move = 0
    then [ new_game ]
    else
      [ new_game ]
      @ make_move_no_mutate
          ~game:new_game
          (List.hd_exn possible_moves_after_initial_move)
  | _, _ -> [ new_game ]
;;

let get_next_game_states (game : Game.t) =
  let available_moves_at_current_state =
    Game.available_captures_for_player game ~my_piece:game.piece_to_move
  in
  List.map
    (List.concat
       (List.map available_moves_at_current_state ~f:(fun move ->
          make_move_no_mutate ~game move)))
    ~f:(fun gamee ->
      gamee.piece_to_move <- Piece.flip gamee.piece_to_move;
      gamee)
;;

(* FLIP THE PIECE NOW *)

let score game ~me ~depth maximizing_player ~evaluated_game =
  match evaluated_game with
  | Game.Game_state.Game_over { winner } ->
    if Piece.equal me winner
    then Int.max_value - 3 + depth
    else Int.min_value + 3 - depth
  | _ ->
    let available_moves_at_current_state =
      Game.available_captures_for_player game ~my_piece:game.piece_to_move
    in
    let number_of_available_moves =
      List.length available_moves_at_current_state
    in
    if maximizing_player
    then Int.max_value - 20 + number_of_available_moves
    else Int.min_value + 20 - number_of_available_moves
;;

let rec minimax game ~me ?(depth = 3) maximizing_player =
  let evaluated_game = Game.evaluate game in
  match depth = 0, evaluated_game with
  | true, _ | _, Game.Game_state.Game_over { winner = _ } ->
    score game ~me ~depth maximizing_player ~evaluated_game
  | _, _ ->
    if maximizing_player
    then (
      let next_possible_games_list = get_next_game_states game in
      (* piece will be flipped *)
      List.fold
        next_possible_games_list
        ~init:Int.min_value
        ~f:(fun acc game_state ->
          let child_minimax =
            minimax game_state ~me ~depth:(depth - 1) (not maximizing_player)
          in
          if acc > child_minimax then acc else child_minimax))
    else (
      let next_possible_games_list =
        get_next_game_states game (*piece will be flipped!!!*)
      in
      List.fold
        next_possible_games_list
        ~init:Int.max_value
        ~f:(fun acc game_state ->
          let child_minimax =
            minimax game_state ~me ~depth:(depth - 1) (not maximizing_player)
          in
          if acc < child_minimax then acc else child_minimax))
;;

let use_minimax_to_find_best_move game ~me =
  let possible_moves =
    Game.available_captures_for_player game ~my_piece:me
  in
  let possible_moves_with_states =
    List.concat
      (List.map possible_moves ~f:(fun move ->
         List.map (make_move_no_mutate ~game move) ~f:(fun g ->
           g.piece_to_move <- Piece.flip g.piece_to_move;
           move, g)))
  in
  let best_move, _heuristic =
    List.fold
      possible_moves_with_states
      ~init:
        ( { Move.starting_pos = { Position.row = 0; column = 0 }
          ; ending_pos = Some { Position.row = 2; column = 0 }
          ; dir = Some Down
          }
        , Int.min_value )
      ~f:(fun (current_best_move, current_highest_heuristic) (move, state) ->
        let heuristic_calculated = minimax state ~me false in
        if heuristic_calculated > current_highest_heuristic
        then move, heuristic_calculated
        else current_best_move, current_highest_heuristic)
  in
  best_move
;;

let%expect_test "print_initial_game" =
  let new_game = Game.new_game ~height:8 ~width:8 in
  print_s
    (Move.sexp_of_t (use_minimax_to_find_best_move new_game ~me:Piece.X));
  [%expect {|
          lol
          |}]
;;
