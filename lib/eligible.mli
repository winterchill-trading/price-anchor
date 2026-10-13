(** The size of simulated resting orders a reference trade reaches. *)

open! Core
open! Exchange_core

(** For a reference trade of [q] at [p] against [passive] side orders of [book] (orders
    owned by an account in [exclude] are skipped): orders strictly through [p] in full,
    orders at [p], oldest first, for what is left of [q] after their [ahead] and the
    orders at [p] before them. One walk of the book, no list and no sort; nothing at all
    when the book's best [passive] price is not at or through [p]. *)
val size
  :  Order_book.t
  -> passive:Side.t
  -> p:Price.t
  -> q:Size.t
  -> exclude:(Account.t -> bool)
  -> ahead:(Order_book.Resting_order.t -> Size.t)
  -> Size.t
