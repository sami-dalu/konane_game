open! Core
open! Core

module Difficulty : sig
  type t =
    | Easy
    | Medium
    | Hard
  [@@deriving bin_io, sexp_of]
end

type t [@@deriving sexp, equal, bin_io, compare, hash]
(* = { name : string ; piece : Piece.t } [@@deriving sexp, equal, bin_io,
   compare, hash] *)

include Comparable.S_binable with type t := t
include Hashable.S_binable with type t := t

val init_human : name:string -> piece:Piece.t -> t
val init_bot : piece:Piece.t -> difficulty:Difficulty.t -> t
val get_piece : t -> Piece.t
