open! Core

module Position = struct
  module T = struct
    type t =
      { row    : int
      ; column : int
      }
    [@@deriving sexp, equal, bin_io, compare]
  end
  include T
  
end