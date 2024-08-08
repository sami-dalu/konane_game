open! Core
open! Async

module Start_game = struct
  module Query = struct
    type t =
      { name : string
      ; bot_difficulty_and_piece : (Player.Difficulty.t * Piece.t) option
      ; game_config : Game_config.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Game_started of { your_player : Player.t }
      | Game_not_started of { your_player : Player.t }
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"start-game"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Take_turn = struct
  module Query = struct
    type t =
      { player : Player.t
      ; move : Move.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Success of
          { game : Game.t
          ; event_opt : Crazy_info.Event.t option
          }
      | Failure
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"take-turn"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Game_over = struct
  module Query = struct
    type t =
      { game : Game.t
      ; evaluation : Game.Game_state.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = unit [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"game-over"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Wait_turn = struct
  module Query = struct
    type t = Player.t [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = Game.t [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"wait-turn"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module End_turn = struct
  module Query = struct
    type t = { player : Player.t } [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = { game : Game.t } [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"end-turn"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Restart_game = struct
  module Query = struct
    type t = Player.t [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = Game.t [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"restart_game"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
