open! Core

module Game_mode : sig
  type t = Normal [@@deriving sexp_of, bin_io]
end

type t =
  { height : int
  ; width : int
  ; mode : Game_mode.t
  }
[@@deriving sexp_of, bin_io]

val default_8_by_8 : unit -> t
