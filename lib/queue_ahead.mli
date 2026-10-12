(** Queue position of simulated resting orders behind the reference liquidity.

    A simulated order first seen at a price where the anchor shows size [q] is behind [q].
    That size shrinks with reference trades at the price ({!trade}) and is capped by the
    reference size there when it drops ({!cap}). Trade-replay fills use it: an order at
    the trade price only fills for the trade size beyond its [queue_ahead]. *)

open! Core
open! Exchange_core

type t

val create : unit -> t

(** Starts tracking the resting orders of [book] not owned by an account in [exclude],
    with [shown side px] ahead of them, and forgets orders no longer resting. *)
val refresh
  :  t
  -> Order_book.t
  -> exclude:(Account.t -> bool)
  -> shown:(Side.t -> Price.t -> Size.t)
  -> unit

(** Caps every order's queue by [reference side px] (the reference size now at its price);
    [None] leaves it unchanged. *)
val cap : t -> reference:(Side.t -> Price.t -> Size.t option) -> unit

(** A reference trade of [size] at [px] taking liquidity from [passive] side orders:
    reduces their queue at that price. *)
val trade : t -> passive:Side.t -> px:Price.t -> size:Size.t -> unit

val find : t -> Order_id.t -> Size.t option
val length : t -> int
