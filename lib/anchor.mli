(** Ties one simulated instrument to one reference instrument (live or recorded).

    The anchor consumes the reference's {!Market_data.Event.t} values and acts on the
    simulated exchange only through its public, journaled calls ([set_mark],
    [set_funding_rate], [deposit], [place], [cancel], [step]) and through
    {!Exchange_core_engine.Market_sim.set_reference}, so a {!Exchange_core_engine.Journal}
    attached to the exchange replays an anchored run bit for bit without the reference
    data. The anchor never reads the wall clock.

    {2 Time}

    Each reference event is applied at [sim_time = recv_ts + offset], the offset being
    fixed by the first event as the exchange's current time minus its [recv_ts]. Before
    applying an event the anchor steps the exchange to [sim_time] when the exchange clock
    is behind; an event whose [sim_time] is not ahead of the clock is applied at the
    current time.

    {2 Mark price and funding}

    The reference price is, in order of precedence, the venue's mark price (from funding
    events), the mid of the reference book, the last trade price; it is scaled and rounded
    to the simulated instrument's tick. The anchor sets the simulated mark to it when it
    differs from the last one set (a change of at least one tick), at most once per
    [mark_interval] of simulated time; a change held back by the interval is applied by a
    later {!apply} or {!step}. With a [market_sim], the reference is pinned through
    [Market_sim.set_reference] (which also sets the mark), so noise traders quote around
    it. Funding rates are converted to the exchange's funding interval:
    [rate * sim_interval / reference_interval].

    {2 Shadow levels}

    On each reference book update (L2, or BBO for sources without depth) the reference's
    top [levels] per side are kept resting from the anchor account ({!Shadow}), and
    simulated orders get a queue position behind the reference size ({!Queue_ahead}).

    {2 Trade-replay fills}

    Passive simulated orders fill when the reference trades at or through them, not only
    when another simulated client takes them. For a reference trade of size [q] at price
    [p], the simulated resting orders on the passive side at or through [p] are eligible:
    those strictly through [p] for their full size, those at [p] (oldest first) for what
    is left of [q] after their [queue_ahead]. The anchor then, within the one call to
    {!apply}: cancels its own shadow orders at or through [p], places an IOC limit order
    at [p] for the eligible size from {!taker_account} (a separate account, since
    self-trade prevention would stop a taker at the anchor's own orders), and re-places
    the shadow levels. The IOC matches in the book's price-time order, so at [p] it may
    reach an older order with a long queue before a newer eligible one. *)

open! Core
open! Exchange_core
open! Exchange_core_engine

module Stats : sig
  type t =
    { events : int (** reference events applied *)
    ; ignored : int (** events of another instrument *)
    ; marks_set : int
    ; funding_rates_set : int
    ; shadow : Shadow.Stats.t
    ; replays : int (** trade-replay IOC orders that filled *)
    ; replays_rejected : int
    (** trade-replay IOC orders the engine rejected (e.g. below the minimum order value) *)
    }
  [@@deriving sexp_of]
end

type t

(** [reference] is the reference instrument, e.g. [binance:BTCUSDT]; [sim_symbol] the
    simulated one, which must be an instrument of [exchange].

    - [price_factor], [size_factor]: see {!Scale} (default [1.]).
    - [levels]: shadow levels per side (default 5).
    - [mark_interval]: minimum simulated time between two [set_mark] calls (default
      100ms).
    - [funding_interval]: what one reference funding rate covers; defaults to the
      reference's entry in {!Market_data.Instrument_info.catalogue}, else 8 hours.
    - [noise_traders]: with [market_sim], the number of noise traders kept trading
      [sim_symbol] (default 0: the reference provides the liquidity).
    - [balance]: deposited into each of the two anchor accounts (default $10B).
    - [log]: receives one line per notable change (default: dropped). *)
val create
  :  ?price_factor:float
  -> ?size_factor:float
  -> ?levels:int
  -> ?mark_interval:Time_ns.Span.t
  -> ?funding_interval:Time_ns.Span.t
  -> ?noise_traders:int
  -> ?balance:Usd.t
  -> ?log:(Sexp.t -> unit)
  -> ?market_sim:Market_sim.t
  -> exchange:Exchange.t
  -> reference:Market_data.Instrument.t
  -> sim_symbol:Symbol.t
  -> unit
  -> t

(** Applies one reference event. Events of other instruments are counted and ignored. *)
val apply : t -> Market_data.Event.t -> unit

(** Called by the simulator's clock after it steps the exchange to [now] (nanoseconds):
    starts tracking the queue of newly placed simulated orders and applies a mark change
    held back by [mark_interval]. *)
val step : t -> now:int -> unit

val stats : t -> Stats.t

(** [Some offset] once the first event fixed it: [sim_time = recv_ts + offset], in
    nanoseconds. *)
val offset : t -> int option

(** The current reference price, scaled and rounded to the simulated instrument. *)
val reference_price : t -> Price.t option

(** ["anchor:<venue>:<symbol>"], the account resting the shadow levels. *)
val anchor_account : t -> Account.t

(** ["anchor-taker:<venue>:<symbol>"], the account sending trade-replay orders. *)
val taker_account : t -> Account.t

val sim_symbol : t -> Symbol.t

(** The anchor account's resting shadow orders, by order id. *)
val shadow_orders : t -> Shadow.Order.t list

(** Reference size ahead of a simulated resting order at its price ({!Queue_ahead});
    [None] for orders not tracked (anchor orders, orders placed since the last {!apply} or
    {!step}). *)
val queue_ahead : t -> Order_id.t -> Size.t option
