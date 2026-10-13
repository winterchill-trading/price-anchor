(** The first eligible-size computation: every resting order as a list, filtered and
    sorted by time. The reference for [Eligible.size] in [test_eligible.ml]. *)

open! Core
open! Exchange_core

val size
  :  Order_book.t
  -> passive:Side.t
  -> p:Price.t
  -> q:Size.t
  -> exclude:(Account.t -> bool)
  -> ahead:(Order_book.Resting_order.t -> Size.t)
  -> Size.t
