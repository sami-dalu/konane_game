open! Core

module T = struct
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, equal, bin_io, compare]
end

include T
include Comparable.Make_binable (T)

let to_string = Fn.compose Sexp.to_string_hum sexp_of_t
