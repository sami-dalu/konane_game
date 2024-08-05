open! Core
open! Async

(* module Start_test : sig module Query : sig type t = { name : string ;
   host_and_port : Host_and_port.t } [@@deriving sexp_of, bin_io] end

   module Response : sig type t = | Game_started | Game_not_started
   [@@deriving sexp_of, bin_io] end end *)

(* module Test : sig module Query : sig type t = int end

   module Response : sig type t = int end

   val rpc : (Query.t, Response.t) Rpc.Rpc.t end *)

module Start_game : sig
  module Query : sig
    type t =
      { name : string
      ; bot_difficulty_and_piece : (Player.Difficulty.t * Piece.t) option
      ; game_config : Game_config.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      | Game_started of { your_player : Player.t }
      | Game_not_started of { your_player : Player.t }
    [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Take_turn : sig
  module Query : sig
    type t =
      { player : Player.t
      ; move : Move.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      | Success of { game : Game.t }
      | Failure
    [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Game_over : sig
  module Query : sig
    type t =
      { game : Game.t
      ; evaluation : Game.Game_state.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = unit [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module End_turn : sig
  module Query : sig
    type t = { player : Player.t } [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = { game : Game.t } [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Wait_turn : sig
  module Query : sig
    type t = Player.t
  end

  module Response : sig
    type t = Game.t
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Restart_game : sig
  module Query : sig
    type t = Player.t
  end

  module Response : sig
    type t = Game.t
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
