(** The first [Queue_ahead]: one table of every tracked order, walked in full by [cap] and
    [trade]. The reference for the indexed one in [test_queue_ahead.ml]. *)

open! Core
open! Exchange_core

type t

val create : unit -> t

val refresh
  :  t
  -> Order_book.t
  -> exclude:(Account.t -> bool)
  -> shown:(Side.t -> Price.t -> Size.t)
  -> unit

val cap : t -> reference:(Side.t -> Price.t -> Size.t option) -> unit
val trade : t -> passive:Side.t -> px:Price.t -> size:Size.t -> unit
val find : t -> Order_id.t -> Size.t option
val length : t -> int
