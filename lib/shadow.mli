(** The reference book's top levels, resting in the simulated book as post-only orders of
    the anchor account.

    On each reference book update the desired levels are computed ({!desired}) and
    reconciled with the anchor's resting orders ({!plan}): orders whose side, price and
    remaining size match a desired level are kept, the others cancelled, the missing
    levels placed as [Alo] orders. A level that would cross a simulated order is rejected
    by the engine and skipped until the next update, so the anchor never takes liquidity.

    Size taken from a shadow level by simulated takers stays consumed while the reference
    size at that price is unchanged: the level is shown with the consumed size taken off,
    and comes back in full once the reference size at that price changes. *)

open! Core
open! Exchange_core
open! Exchange_core_engine

module Level : sig
  type t =
    { side : Side.t
    ; px : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Order : sig
  type t =
    { order_id : Order_id.t
    ; level : Level.t (** [size] is the remaining size *)
    }
  [@@deriving sexp_of]
end

module Plan : sig
  type t =
    { cancel : Order.t list
    ; place : Level.t list
    ; keep : Order.t list
    }
  [@@deriving sexp_of]
end

(** Keeps the first resting order matching each desired level exactly (side, price and
    size); cancels every other resting order, places the levels left unmatched. Pure. *)
val plan : desired:Level.t list -> resting:Order.t list -> Plan.t

(** The top [levels] reference levels per side, scaled (bids rounded down, asks up), sizes
    rounded down to the lot, levels on the same simulated price merged and zero sizes
    dropped. Best first on each side, bids before asks. Pure. *)
val full_levels : Ref_book.t -> Scale.t -> levels:int -> Level.t list

module Stats : sig
  type t =
    { placed : int
    ; cancelled : int
    ; skipped : int (** placements the engine rejected (would cross, too small, ...) *)
    ; consumed : int (** fills of shadow orders by simulated takers *)
    }
  [@@deriving sexp_of]
end

type t

val create
  :  Exchange.t
  -> account:Account.t
  -> symbol:Symbol.t
  -> scale:Scale.t
  -> levels:int
  -> t

(** Records fills of the anchor's orders since the last call as consumed size. *)
val sync_fills : t -> unit

(** {!full_levels} minus the consumed size at each price. *)
val desired : t -> Ref_book.t -> Level.t list

(** [sync_fills], then reconciles the resting orders with {!desired}. *)
val reconcile : t -> Ref_book.t -> unit

(** Cancels the resting shadow orders of [side] whose price satisfies [f]. *)
val cancel_where : t -> side:Side.t -> f:(Price.t -> bool) -> unit

(** Cancels every resting shadow order and forgets consumed sizes. *)
val cancel_all : t -> unit

(** The anchor's remaining size resting at a price. *)
val shown : t -> Side.t -> Price.t -> Size.t

(** Resting shadow orders, by order id. *)
val orders : t -> Order.t list

val stats : t -> Stats.t
