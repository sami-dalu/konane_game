open! Core

module Game_mode = struct
  type t = Normal
end

type t =
  { height : int
  ; width : int
  ; mode : Game_mode.t
  }

let default_8_by_8 () = { height = 8; width = 8; mode = Game_mode.Normal }
