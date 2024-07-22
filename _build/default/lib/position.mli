open! Core

    (* Top-left is [{row = 0; column = 0}].
  
       row indexes increment downwards.
  
       column indexes increment rightwards. *)
    type t =
      { row    : int
      ; column : int
      }
    [@@deriving sexp_of, equal, bin_io, compare]
  
    val to_string : t -> string
    (** [all_offsets] is a list of functions to compute all 8 neighbors of a
      cell (i.e. left, up-left, up, up-right, right, right-down, down,
      down-left). *)
  
    include Comparable.S_binable with type t := t
