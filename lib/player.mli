open! Core
open! Core

type t [@@deriving sexp, equal, bin_io, compare, hash]
(* = { name : string ; piece : Piece.t } [@@deriving sexp, equal, bin_io,
   compare, hash] *)

include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t

val init : name:string -> piece:Piece.t -> t
val get_name : t -> string
