open! Core

module Game_mode : sig
  type t =
    | Normal
    | Crazy
  [@@deriving sexp_of, bin_io]
end

type t =
  { height : int
  ; width : int
  ; mode : Game_mode.t
  }
[@@deriving sexp_of, bin_io]

val default_8_by_8 : unit -> t

include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t
