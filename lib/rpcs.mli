open! Core
open! Async

(* module Start_test : sig module Query : sig type t = { name : string ;
   host_and_port : Host_and_port.t } [@@deriving sexp_of, bin_io] end

   module Response : sig type t = | Game_started | Game_not_started
   [@@deriving sexp_of, bin_io] end end *)

module Test : sig
  module Query : sig
    type t = int
  end

  module Response : sig
    type t = int
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Start_game : sig
  module Query : sig
    type t =
      { name : string
      ; host_and_port : Host_and_port.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      | Game_started
      | Game_not_started
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
    type t = { game : Game.t } [@@deriving sexp_of, bin_io]
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
