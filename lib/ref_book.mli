(** The reference market's order book, rebuilt from L2 snapshots and deltas (or, for
    sources without depth, from the best bid and offer). Prices and sizes are the
    reference venue's own, unscaled. *)

open! Core
open! Exchange_core

type t [@@deriving sexp_of]

val create : unit -> t

(** A snapshot replaces the book; a delta sets each level's size, removing levels whose
    size is zero. *)
val apply_l2 : t -> Market_data.L2_update.t -> unit

(** Replaces the book with one level per side (sides with a zero size stay empty). *)
val apply_bbo : t -> Market_data.Bbo.t -> unit

val clear : t -> unit
val is_empty : t -> bool

(** Best level of a side: [Buy] for bids, [Sell] for asks. *)
val best : t -> Side.t -> (Price.t * Size.t) option

(** Mid of the best bid and ask, truncated to the price precision; [None] unless both
    sides have a level. *)
val mid : t -> Price.t option

(** The best [n] levels of a side, best first. *)
val top : t -> Side.t -> n:int -> (Price.t * Size.t) list

(** Size resting at a price, zero when there is no level. *)
val size_at : t -> Side.t -> Price.t -> Size.t
