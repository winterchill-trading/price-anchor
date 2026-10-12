(** A reference price as a function of elapsed venue time, for venues with no data source:
    the TradFi simulators pull it on their own clock ({!Anchor.create}'s [path]) instead
    of being pushed points by {!Source.run}.

    A series is a step function of its points, offset so its first point is at
    [elapsed = 0]; it holds its first price before and its last price after. A fixed price
    drifts by [drift_bps_per_day], compounded per day of venue time. Prices are the
    reference's own; {!Anchor}'s [price_factor] scales them and rounds them to the tick. *)

open! Core
open! Exchange_core

type t = private
  | Series of (Time_ns.t * Price.t) array (** non-empty, in time order *)
  | Fixed of
      { price : Price.t
      ; drift_bps_per_day : float
      }
[@@deriving sexp_of]

(** Sorts the points by time (stable); fails when empty or a price is not positive. *)
val series : (Time_ns.t * Price.t) list -> t Or_error.t

(** Fails when the price is not positive or the drift not finite. *)
val fixed : ?drift_bps_per_day:float -> Price.t -> t Or_error.t

(** [PRICE[,DRIFT_BPS_PER_DAY]], the TradFi simulators' [-anchor-fixed] value. *)
val fixed_of_string : string -> t Or_error.t

(** The reference price [elapsed] after the path started. *)
val price_at : t -> elapsed:Time_ns.Span.t -> Price.t
