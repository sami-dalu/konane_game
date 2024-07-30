open! Core

module T = struct
  type t =
    { name : string
    ; piece : Piece.t
    ; id : int
    }
  [@@deriving sexp, equal, bin_io, compare, hash]
end

include T
include Comparable.Make_binable (T)
include Hashable.Make_binable (T)

let init ~name ~piece = { T.name; piece; id = Random.int 1000000 }
let get_name t = t.name
let get_piece t = t.piece
