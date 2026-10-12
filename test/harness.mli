(** A simulated exchange driven by a 100ms clock, with an anchor fed reference events, as
    a simulator would run it. Every exchange input is recorded so a run can be replayed. *)

open! Core
open! Exchange_core
open! Exchange_core_engine

type t =
  { exchange : Exchange.t
  ; market_sim : Market_sim.t option
  ; anchor : Price_anchor.Anchor.t
  ; inputs : Input.t Queue.t
  ; events : Event.t Queue.t
  }

val btc : Symbol.t

(** [config] defaults to {!Exchange.Config.default} with no minimum order value. With
    [sim_config], a {!Market_sim.t} is created (after input recording starts). *)
val create
  :  ?config:Exchange.Config.t
  -> ?sim_config:Market_sim.Config.t
  -> (exchange:Exchange.t -> market_sim:Market_sim.t option -> Price_anchor.Anchor.t)
  -> t

(** Applies [events] (sorted by [recv_ts]), stepping the clock every 100ms of simulated
    time: at each tick, the events up to it, then the market simulation's tick, the
    exchange step and the anchor step. Runs until the last event's tick plus [extra_ticks]
    (default 0). [after_tick] is called after each tick with the tick time. *)
val run
  :  ?extra_ticks:int
  -> ?after_tick:(int -> unit)
  -> t
  -> Market_data.Event.t list
  -> unit

(** Replays [inputs] into a fresh exchange and prints whether the events and marks match. *)
val check_replay : ?config:Exchange.Config.t -> t -> unit

val config : Exchange.Config.t
