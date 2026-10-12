(** Converts reference prices and sizes into valid prices and sizes of the simulator's
    instrument.

    A reference listing may quote in other units than the simulated one
    ([binance:1000PEPEUSDT] against [kPEPE]); [price_factor] and [size_factor] convert:
    simulated price = reference price * [price_factor], simulated size = reference size *
    [size_factor]. Factors of [1.] are applied exactly, without going through floats. *)

open! Core
open! Exchange_core

type t =
  { instrument : Instrument.t
  ; price_factor : float
  ; size_factor : float
  }
[@@deriving sexp_of]

(** Raises unless both factors are positive and finite. *)
val create : ?price_factor:float -> ?size_factor:float -> Instrument.t -> t

module Direction : sig
  type t =
    | Down (** bids: never better than the reference *)
    | Up (** asks *)
    | Nearest (** marks and trade prices *)
  [@@deriving sexp_of, compare, equal]
end

(** The scaled price rounded to a valid price of the instrument (its tick, and its
    significant figure limit for non-integer prices) in [direction]. [None] when it rounds
    to zero or below. *)
val price : t -> Price.t -> direction:Direction.t -> Price.t option

(** The scaled size rounded down to the instrument's lot; may be zero. *)
val size : t -> Size.t -> Size.t
