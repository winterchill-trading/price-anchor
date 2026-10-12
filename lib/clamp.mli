(** Ready-made [clamp] functions for {!Anchor.create}. *)

open! Core
open! Exchange_core

(** [within_bands instrument ~lower ~upper px] keeps [px] [margin] (a fraction, default
    [0.003], 30 bps) inside the price bands [[lower, upper]] (LULD bands, daily limits),
    rounded to the instrument's price. The margin keeps noise traders, whose orders reach
    the engine without the venue's band checks, from trading through a band. Bands too
    narrow for the margin leave [px] alone. *)
val within_bands
  :  ?margin:float
  -> Instrument.t
  -> lower:Price.t
  -> upper:Price.t
  -> Price.t
  -> Price.t
