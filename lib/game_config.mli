open! Core

module Game_mode : sig
  type t = Normal
end

type t =
  { height : int
  ; width : int
  ; mode : Game_mode.t
  }

val default_8_by_8 : unit -> t
