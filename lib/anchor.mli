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
    current time. With [~step_clock:false] the anchor never steps the exchange: the venue
    owns its clock, and every event is applied at the exchange's current time.

    {2 Mark price and funding}

    The reference price is, in order of precedence, a reference series price
    ({!apply_price}), the venue's mark price (from funding events), the mid of the
    reference book, the last trade price; it is scaled and rounded to the simulated
    instrument's tick. The anchor sets the simulated mark to it when it differs from the
    last one set (a change of at least one tick), at most once per [mark_interval] of
    simulated time; a change held back by the interval is applied by a later {!apply} or
    {!step}. With a [market_sim], the reference is pinned through
    [Market_sim.set_reference] (which also sets the mark), so noise traders quote around
    it; with a [half_life] as well, the noise traders' walk is pulled towards it instead
    (mean reversion, the mark following on the next tick), the way the TradFi simulators
    use a reference with no depth. Funding rates are converted to the exchange's funding
    interval: [rate * sim_interval / reference_interval].

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
    reach an older order with a long queue before a newer eligible one.

    {2 Staleness}

    The reference is stale before the first L2 snapshot (or BBO, for sources without
    depth), after a gap ({!gap}) and after [max_staleness] without an event. While stale
    the anchor cancels its shadow levels, stops trade replay and stops setting the mark
    (it stays at the last reference price); with a [market_sim] and [noise_traders > 0]
    the symbol goes back to the noise traders' random walk from the last price. The next
    L2 snapshot (or BBO, without depth) makes it fresh: the reference is pinned and the
    shadow levels placed at once. Each transition is passed to [log] and counted.

    {2:pausing Pausing}

    A venue with its own trading phases passes [may_engage]. When it first says no, the
    anchor hands the symbol to the noise traders' free walk ([Market_sim.clear_reference],
    once) and then counts each held-back update as a wait. When it says yes again, the
    walk restarts from the venue's last trade ([last_trade], pinned through
    [Market_sim.set_reference]) and the reference is set again at once, so with a
    [half_life] the price climbs back from where the venue left it. Pausing leaves the
    shadow levels and trade replay alone; the venues that pause have no depth to shadow. *)

open! Core
open! Exchange_core
open! Exchange_core_engine

module Stale_reason : sig
  type t =
    | Waiting_for_book (** no L2 snapshot (or BBO, for sources without depth) yet *)
    | Silent (** no reference event for [max_staleness] *)
    | Gap of Market_data.Gap.Reason.t (** the source reported a break *)
  [@@deriving sexp_of, compare, equal]
end

module Stats : sig
  type t =
    { stale : Stale_reason.t option (** [None] while fresh *)
    ; went_stale : int (** transitions to stale, counting the initial wait *)
    ; stale_ns : int (** simulated time spent stale so far *)
    ; gaps : int
    ; events : int (** reference events applied *)
    ; ignored : int (** events of another instrument *)
    ; marks_set : int
    ; funding_rates_set : int
    ; shadow : Shadow.Stats.t
    ; replays : int (** trade-replay IOC orders that filled *)
    ; replays_rejected : int
    (** trade-replay IOC orders the engine rejected (e.g. below the minimum order value) *)
    ; waits : int (** mark updates held back because [may_engage] said no *)
    ; resumes : int (** times [may_engage] allowed the anchor again after a wait *)
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
    - [max_staleness]: a reference silent for longer is stale (default 5s), measured in
      simulated time, i.e. on [recv_ts] for recorded sources.
    - [funding_interval]: what one reference funding rate covers; defaults to the
      reference's entry in {!Market_data.Instrument_info.catalogue}, else 8 hours.
    - [noise_traders]: with [market_sim], the number of noise traders kept trading
      [sim_symbol] (default 0: the reference provides the liquidity).
    - [half_life]: with [market_sim], pull the walk towards the reference with this
      half-life ([Market_sim.set_reference ~half_life]) instead of pinning it (default:
      pin). Raises if not positive.
    - [may_engage]: asked before each mark or reference update (default: always). While it
      says no (a session phase without continuous trading, an auction, a volatility pause,
      a limit halt) the venue owns the price: see {!section-pausing}.
    - [last_trade]: the venue's last trade price, where the walk restarts when the anchor
      engages again (default: none, the walk continues from where it is).
    - [step_clock]: whether the anchor steps the exchange clock to each event's simulated
      time (default true); false for venues that own their clock.
    - [balance]: deposited into each of the two anchor accounts (default $10B).
    - [log]: receives one line per notable change (default: dropped). *)
val create
  :  ?price_factor:float
  -> ?size_factor:float
  -> ?levels:int
  -> ?mark_interval:Time_ns.Span.t
  -> ?max_staleness:Time_ns.Span.t
  -> ?funding_interval:Time_ns.Span.t
  -> ?noise_traders:int
  -> ?half_life:Time_ns.Span.t
  -> ?may_engage:(unit -> bool)
  -> ?last_trade:(unit -> Price.t option)
  -> ?step_clock:bool
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

(** A point of a reference series ({!Source.of_series}): a reference price with no book or
    trades, for venues without a data source. It takes precedence over every other
    reference price, and makes the reference fresh unless the source also carries a book.
    Applied at [recv_ts + offset] like an event. *)
val apply_price : t -> recv_ts:Time_ns.t -> Price.t -> unit

(** A break reported by the source ({!Market_data.Feed.With_gaps}, an empty raw line):
    when it concerns the reference, the reference book is dropped and the anchor goes
    stale. *)
val gap : t -> Market_data.Gap.t -> unit

(** Called by the simulator's clock after it steps the exchange to [now] (nanoseconds):
    marks the reference stale when it has been silent for [max_staleness]; otherwise
    starts tracking the queue of newly placed simulated orders and applies a mark change
    held back by [mark_interval]. *)
val step : t -> now:int -> unit

val stats : t -> Stats.t
val is_stale : t -> bool

(** Whether [may_engage] said no at the last update and has not said yes since. *)
val is_paused : t -> bool

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
