open! Core

let rec make_move_no_mutate ~(game : Game.t) (moves_made : Move.t list) =
  (* returns the game states and moves made to get to those game state(s )*)
  let move_to_make_now = List.hd_exn moves_made in
  let captured_pos () =
    match move_to_make_now.ending_pos with
    | None -> None
    | Some end_pos ->
      let row = (move_to_make_now.starting_pos.row + end_pos.row) / 2 in
      let col =
        (move_to_make_now.starting_pos.column + end_pos.column) / 2
      in
      Some { Position.row; column = col }
  in
  let ending_position_opt = move_to_make_now.ending_pos in
  let new_board =
    match ending_position_opt with
    | None ->
      (* no ending position <==> one of the intial moves *)
      (match game.game_state with
       | Game.Game_state.First_moves ->
         (* first move so "starting position" simply has to be removed from
            map *)
         Map.remove game.board move_to_make_now.starting_pos
       | _ ->
         failwith
           "no ending position for a Move.t but game is in the first_moves \
            state")
    | Some end_pos ->
      let my_piece = Map.find_exn game.board move_to_make_now.starting_pos in
      let intermediate_map =
        Map.set game.board ~key:end_pos ~data:my_piece
      in
      let map_with_captured_piece =
        Map.remove intermediate_map move_to_make_now.starting_pos
      in
      let pos_of_captured_piece = Option.value_exn (captured_pos ()) in
      Map.remove map_with_captured_piece pos_of_captured_piece
  in
  let new_game_state =
    match ending_position_opt with
    | None ->
      let removed_piece =
        Map.find_exn game.board move_to_make_now.starting_pos
      in
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
    ; bot_difficulty = game.bot_difficulty
    ; player1 = game.player1
    ; player2 = game.player2
    ; last_move_played = None
    ; crazy_info = game.crazy_info
    ; inverse_board = game.inverse_board
    }
  in
  match move_to_make_now.dir, move_to_make_now.ending_pos with
  | Some dir, Some pos ->
    let possible_moves_after_initial_move =
      Game.possible_captures_from_occupied_pos_exn ~dir_opt:dir new_game pos
    in
    if List.length possible_moves_after_initial_move = 0
    then [ new_game, moves_made ]
    else
      [ new_game, moves_made ]
      @ make_move_no_mutate
          ~game:new_game
          ([ List.hd_exn possible_moves_after_initial_move ] @ moves_made)
  | _, _ -> [ new_game, moves_made ]
;;

let get_next_game_states (game : Game.t) =
  let available_moves_at_current_state =
    Game.available_captures_for_player game ~my_piece:game.piece_to_move
  in
  List.map
    (List.concat
       (List.map available_moves_at_current_state ~f:(fun move ->
          make_move_no_mutate ~game [ move ])))
    ~f:(fun (gamee, moves_made) ->
      gamee.piece_to_move <- Piece.flip gamee.piece_to_move;
      gamee, moves_made)
;;

(* FLIP THE PIECE NOW *)
let number_of_pieces_on_edge (game : Game.t) ~piece_to_eval =
  let height = game.board_height in
  let width = game.board_width in
  Map.fold game.board ~init:0 ~f:(fun ~key:pos ~data:piece acc ->
    if Piece.equal piece_to_eval piece
    then (
      let row = pos.row in
      let col = pos.column in
      if row = 0 || col = 0 || row = height - 1 || col = width - 1
      then acc + 1
      else acc)
    else acc)
;;

let score game ~me ~depth maximizing_player ~evaluated_game =
  match evaluated_game with
  | Game.Game_state.Game_over { winner } ->
    if Piece.equal me winner
    then Int.max_value - 6 + depth
    else Int.min_value + 6 - depth
  | _ ->
    let num_of_my_edge_pieces =
      number_of_pieces_on_edge game ~piece_to_eval:me
    in
    let available_moves_at_current_state =
      Game.available_captures_for_player game ~my_piece:me
    in
    let number_of_available_moves =
      List.length available_moves_at_current_state
    in
    let available_moves_for_enemy =
      Game.available_captures_for_player game ~my_piece:(Piece.flip me)
    in
    let number_of_enemy_moves = List.length available_moves_for_enemy in
    if maximizing_player
    then
      Int.max_value
      - 40
      + number_of_available_moves
      - (2 * number_of_enemy_moves)
      + (2 * num_of_my_edge_pieces)
      - 6
      + depth
    else
      Int.min_value
      + 40
      - number_of_available_moves
      + (2 * number_of_enemy_moves)
      - (2 * num_of_my_edge_pieces)
      + 6
      - depth
;;

let rec minimax game ~me ~depth maximizing_player =
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
        ~f:(fun acc (game_state, _) ->
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
        ~f:(fun acc (game_state, _) ->
          let child_minimax =
            minimax game_state ~me ~depth:(depth - 1) (not maximizing_player)
          in
          if acc < child_minimax then acc else child_minimax))
;;

let use_minimax_to_find_best_moves game ~depth ~me =
  let possible_moves =
    Game.available_captures_for_player game ~my_piece:me
  in
  let possible_moves_with_states =
    List.concat
      (List.map possible_moves ~f:(fun move ->
         List.map (make_move_no_mutate ~game [ move ]) ~f:(fun (g, m) ->
           g.piece_to_move <- Piece.flip g.piece_to_move;
           m, g)))
  in
  let best_moves, _heuristic =
    List.fold
      possible_moves_with_states
      ~init:([], Int.min_value)
      ~f:
        (fun
          (current_best_moves, current_highest_heuristic) (moves, state) ->
        let heuristic_calculated = minimax state ~me ~depth false in
        if heuristic_calculated > current_highest_heuristic
        then moves, heuristic_calculated
        else current_best_moves, current_highest_heuristic)
  in
  best_moves
;;
