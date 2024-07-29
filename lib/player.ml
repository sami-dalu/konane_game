open! Core

module T = struct
  type t =
    { name : string
    ; piece : Piece.t
    }
  [@@deriving sexp, equal, bin_io, compare]
end

include T
include Comparable.Make_binable (T)
