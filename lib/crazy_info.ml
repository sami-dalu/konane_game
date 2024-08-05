open! Core

type t =
  { turns_since_event : int
  ; obstacle_location_list : Position.t list
  ; monster_locations_list : Position.t list
  ; withered_pieces_list : (Position.t * int) list
  ; duplicating_pieces : bool
  }
[@@deriving sexp, bin_io, compare, hash]

let default () =
  { turns_since_event = 0
  ; obstacle_location_list = []
  ; monster_locations_list = []
  ; withered_pieces_list = []
  ; duplicating_pieces = false
  }
;;

(* let _flip_all_pieces (game : Game.t) = game.board <- Map.map game.board
   ~f:(fun piece -> Piece.flip piece) ;;

   let _place_obstacle (game : Game.t) = let board_list = Map.to_alist
   game.board in let pos, _init_piece = List.random_element_exn board_list in
   game.board <- Map.set game.board ~key:pos ~data:Piece.Obstacle ;; *)

(* let wither_pieces game = () let rotate_game game = () let teleport_pieces
   game = () let monster_pieces game = () let activate_duplicates game =
   () *)
