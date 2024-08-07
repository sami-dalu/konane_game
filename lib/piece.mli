type t =
  | X
  | O
  | Obstacle
  | Monster
[@@deriving sexp, equal, bin_io, compare, enumerate, hash]

val of_string : string -> t
val to_string : t -> string

(* [flip] gives you the "other" piece. | X -> O | O -> X *)
val flip : t -> t
