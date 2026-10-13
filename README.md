# price-anchor

Ties a simulated exchange's market to a reference market, live or recorded: the mark
price and funding follow the reference, the reference's top levels rest in the simulated
book, and passive simulated orders fill when the reference trades through them. Every
action goes through the engine's public, journaled calls, so an anchored run stays
deterministic and a `Journal` replays it bit for bit without the reference data. Nothing
here reads the wall clock (except the optional pacing in `Source.run`).

Design: [specs/design/price-anchor.md](https://github.com/winterchill-trading/specs/blob/main/design/price-anchor.md).

| Module | Purpose |
| --- | --- |
| `Anchor` | `create ~exchange ?market_sim ~reference ~sim_symbol ()`, `apply` (one `Market_data.Event.t`), `gap`, `apply_price` (a series point), `step ~now`, `stats` |
| `Source` | reference items in `recv_ts` order: `of_feed`, `of_feed_with_gaps`, `of_raw`, `of_norm`, `of_series`; `run ?speed ?time_source source ~anchor`; `to_path` |
| `Path` | a reference price as a function of venue time (a series step function or a drifting fixed price), pulled by `Anchor.step` |
| `Clamp` | `within_bands`: keep a target inside LULD bands or daily limits |
| `Shadow` | the top-N reference levels as post-only orders of the anchor account, reconciled keep / cancel / place, with consumed size |
| `Queue_ahead` | reference size ahead of each simulated resting order |
| `Ref_book` | the reference book from L2 snapshots and deltas (or BBO) |
| `Scale` | price and size factors, rounding to the simulated tick (bids down, asks up) and lot |

## What the anchor does

- **Time.** An event is applied at `sim_time = recv_ts + offset`; the offset is fixed by
  the first event (the exchange's time minus its `recv_ts`). The anchor steps the
  exchange to `sim_time` when the clock is behind, so funding and liquidation happen in
  order.
- **Mark.** Reference price: series point, else the venue's mark (funding events), else
  the book mid, else the last trade; scaled, rounded to the tick, set at most once per
  `mark_interval` (100 ms) and only when it changed. With a `Market_sim` it goes through
  `Market_sim.set_reference`, so noise traders (default 0 for an anchored symbol) quote
  around it. Funding rates are converted with `rate * sim_interval / reference_interval`
  (the reference interval comes from `Market_data.Instrument_info`, default 8 h).
- **Shadow levels.** Account `anchor:<venue>:<symbol>` (deposited $10B) keeps the top
  `levels` (5) per side resting as `Alo` orders. A level that would cross a simulated
  order is skipped until the next update. Size taken by simulated takers stays consumed
  until the reference size at that price changes.
- **Trade replay.** For a reference trade of `q` at `p`: simulated passive orders strictly
  through `p` are eligible in full, orders at `p` (oldest first) for what is left of `q`
  after their `queue_ahead`. The anchor cancels its shadow orders at or through `p`,
  sends an IOC at `p` for the eligible size from `anchor-taker:<venue>:<symbol>`, and
  re-places the shadows, all inside one `apply`.
- **Staleness.** Stale before the first L2 snapshot (BBO for sources without depth), after
  a gap, and after `max_staleness` (5 s of `recv_ts`) without events. While stale: no
  shadows, no replay, no `set_mark`; with noise traders, `Market_sim.clear_reference`
  hands the symbol back to the random walk. The next snapshot makes it fresh. Transitions
  go to `log` and to `stats` (`went_stale`, `stale_ns`, `gaps`).

## Wiring it into a simulator

The simulators (`hl-sim`, `binance-sim`, `bybit-sim`, the TradFi ones) run the anchor in
process, on the Async scheduler thread that drives the engine:

```ocaml
let anchor =
  Price_anchor.Anchor.create
    ~market_sim                    (* optional: the sim's Market_sim.t *)
    ~exchange
    ~reference:(Market_data.Instrument.of_string "binance:BTCUSDT")
    ~sim_symbol:(Symbol.of_string "BTC")
    ~log:(fun s -> Log.Global.info_s s)
    ()
in
(* after the sim's own clock tick has stepped the exchange: *)
Price_anchor.Anchor.step anchor ~now:(Exchange.now exchange);
(* the source, in the background: *)
don't_wait_for (Price_anchor.Source.run ~speed source ~anchor)
```

Flags each simulator should add (from the design):

| Flag | Source |
| --- | --- |
| `-anchor VENUE:SYMBOL=SIMSYM` | live: the venue client's `Live_feed` (any venue for any simulator), `Source.of_feed_with_gaps` |
| `-anchor-raw DIR -anchor-from T -anchor-to T` | recorded raw minute files through the venue's `Market_data.Normalizer.S`, `Source.of_raw` |
| `-anchor-norm DIR` (with `-anchor-from`/`-anchor-to` days) | normalized event logs, `Source.of_norm` |
| `-anchor-series FILE` | `(time, price)` points, `Source.of_series`; the TradFi simulators, with noise traders kept |
| `-anchor-speed X` | pacing of recorded sources: `1` real time, `0` as fast as possible |
| `-anchor-levels N`, `-anchor-price-factor F`, `-anchor-size-factor F` | `Anchor.create` options |

Offline (`Stepped` with `Config.sim = None`): feed the anchor every reference event up to
each step's time before the strategy reacts; `Stepped.check_replay` then checks the whole
run.

## TradFi

The TradFi simulators (`nyse-sim`, `cboe-sim`, `cme-sim`) have no free real-time source:
nothing to shadow and no trade flow to replay. The noise traders stay the book's only
liquidity and the anchor turns their walk into mean reversion around a reference series
(a series file, or a recorded crypto instrument scaled by a factor). The venue owns its
clock and its trading phases, so the anchor neither steps the clock nor moves the price
outside continuous trading:

```ocaml
let%bind path = Price_anchor.Source.of_series file >>=? Price_anchor.Source.to_path in
let anchor =
  Price_anchor.Anchor.create
    ~market_sim
    ~noise_traders:(Market_sim.config market_sim).noise_traders
    ~path:(ok_exn path)                (* pulled on the venue's clock *)
    ~half_life:Time_ns.Span.second   (* pull the walk, do not pin it *)
    ~step_clock:false                (* the venue's clock *)
    ~levels:0                        (* no depth to shadow *)
    ~mark_interval:Time_ns.Span.zero (* follow on every noise tick *)
    ~may_engage:(fun () ->           (* core session, no auction, no LULD pause or limit halt *)
      Venue.continuous_trading venue ~symbol_id)
    ~last_trade:(fun () -> Venue.last_price venue ~symbol_id)
    ~clamp:(fun px ->
      let lower, upper = Venue.bands venue ~symbol_id in
      Price_anchor.Clamp.within_bands instrument ~lower ~upper px)
    ~price_factor
    ~exchange
    ~reference:(Market_data.Instrument.of_string "nyse:IBM")
    ~sim_symbol
    ()
in
(* on each noise tick, after the venue stepped its clock: *)
Price_anchor.Anchor.step anchor ~now:(Exchange.now exchange)
```

- `path` is a `Path.t`: `Path.series`, `Path.fixed ~drift_bps_per_day`,
  `Path.fixed_of_string "PRICE[,DRIFT]"`, or `Source.to_path` over any source (a series
  file, recorded event logs, raw minute files). Each `step` takes its price at
  `now - start` (the exchange's time at `create`). Pushing points with `Source.run`
  instead (`apply_price`) also works, paced on the venue's `time_source`.
- `half_life` sets the reference through `Market_sim.set_reference ~half_life`; without it
  the reference is pinned.
- While `may_engage` says no, the anchor hands the symbol back to the free walk
  (`clear_reference`, once; `Stats.pauses`) and counts waits (`Stats.waits`). When it
  says yes again the walk restarts from `last_trade` (the opening or reopening auction's
  price) and climbs back towards the reference (`Stats.resumes`).
- `clamp` moves the scaled target before it is set; `Stats.clamped` counts the updates it
  moved. `last_set` is the last target set, after the clamp.
- A series alone never goes stale between its points; pass `max_staleness` to change
  that. Feeds keep the 5 s default.
- `Source.of_series` returns `Error` for a missing or malformed file.
- `Anchor.create` calls `Market_sim.set_noise_traders` with `noise_traders` (default 0),
  so a TradFi simulator passes its own count. It also opens and funds the two anchor
  accounts, which stay idle with `levels:0` and no trades.

### TradFi migration

How each simulator's own `Anchoring` module maps onto `Anchor` (wave 6). Everything below
exists here, so the three modules can go; flag parsing (`FILE=SYM`, product lookup) stays
in each simulator's CLI.

| `Anchoring` (sim) | `Anchor` / this repository |
| --- | --- |
| **nyse-sim** `Reference.Series`, `Spec.parse_series`, `-anchor-series` | `Source.of_series` then `Source.to_path` |
| nyse `Reference.Fixed`, `-anchor-fixed` | `Path.fixed_of_string` |
| nyse `Spec.points_of_source ?min_interval` (100 ms), `-anchor-scaled` | `Source.of_norm` then `Source.to_path ~min_interval:(Time_ns.Span.of_int_ms 100)`; the factor is `price_factor` |
| nyse `create ?half_life ~start` (1 s) | `~half_life:Time_ns.Span.second` (the default here pins); `start` is the exchange's time at `create` |
| nyse `price_at` (Scale, nearest tick) | `reference_price` (same `Scale`, `Nearest`) |
| nyse `active ~phase ~halted ~auction` | `may_engage` |
| nyse `on_tick ?bands` (30 bps margin) | `clamp` with `Clamp.within_bands` on `Venue.luld_bands` (`margin` 0.003 by default) |
| nyse `on_venue_tick` | `step ~now:(Exchange.now exchange)` on each noise tick, `step_clock:false`, `levels:0`, `mark_interval:Time_ns.Span.zero` |
| nyse `engaged` | `not (is_paused t)` once `last_set` is `Some _` |
| nyse `Stats` `pulls` / `waits` / `releases` / `clamped` | `marks_set` (changes only, not every tick) / `waits` / `pauses` / `clamped` (applied updates only) |
| nyse: no restart from the venue's price | omit `last_trade` |
| **cboe-sim** `Target.series`, `Target.fixed`, `Target.parse_fixed`, `Target.price_at` | `Path.series`, `Path.fixed`, `Path.fixed_of_string`, `Path.price_at` |
| cboe `Target.load_series ?factor` | `Source.of_series` then `Source.to_path`; `price_factor` |
| cboe `Target.load_norm ~resolution ~factor` (trades and BBO mids, last price per 1 s bucket) | `Source.of_norm` then `Source.to_path ~min_interval:resolution` (book mids including L2, else trades; first change per interval) |
| cboe `create ?half_life`, start fixed by the first `update` | `~half_life`; start at `create` |
| cboe `update ~follow ~sim ~symbol` | `may_engage:(fun () -> anchor_follows ...)` and `step` per tick |
| cboe `State.Following` / `Waiting`, `last_set` | `is_paused`, `last_set` |
| cboe `resumes` / `waits` (transitions) | `resumes` (after a pause only: one fewer, the first follow is not a resume) / `pauses` |
| cboe prices scaled in units, not rounded to the tick | `Scale` rounds to the tick (`Nearest`) |
| **cme-sim** `Target.Series`, `Target.Fixed`, `Target.price_at` (option) | `Path` (empty series and non-positive prices are rejected when built) |
| cme `Spec`, `Norm_range`, `load` | CLI: `Source.of_series` / `Path.fixed_of_string` / `Source.of_norm` (scaled: `price_factor`), each then `Source.to_path` |
| cme `create ?half_life ~start` over a product list | one `Anchor` per product (`Products.engine_symbol`), `~half_life` |
| cme `step` while `Venue.Phase.Open` | `may_engage:(fun () -> Venue.Phase.equal (Venue.phase venue ~security_id) Open)` |
| cme restart from `Venue.last_price` on reopening | `last_trade:(fun () -> Some (Venue.last_price venue ~security_id))` |
| cme `target` (tick-rounded) | `reference_price` (before the clamp) |
| cme `Stats` `pulls` / `waits` / `restarts` / `last_target` | `marks_set` / `waits` / `resumes` / `last_set` |
| cme daily limits (not clamped today) | optional: `clamp` with `Clamp.within_bands` on `Venue.price_limits` |

## Binary

```
price-anchor replay -reference binance:BTCUSDT -norm DIR -from 2026-01-01 -to 2026-01-01 [-symbol BTC] [-journal FILE] [-anchor-speed X] [-verbose]
price-anchor replay -reference cme:ES -series points.txt -symbol BTC
```

It runs the reference into a fresh default exchange, prints the anchor's counters, and
checks that the journal replays to the same mark and book.

## Performance

`bench/bench_anchor.exe`: 100k events, 75% L2 deltas at the top levels, 25% trades, one
event per millisecond, against 0, 50 or 500 resting client orders. In the churn runs the
client cancels and re-places one order before every event (not timed), so the anchor
cannot skip its queue refresh. Microseconds and minor words per event, arm64 laptop (CI is
amd64), before (`9aab036`) and after the performance work:

| Run | dev | release | release `-O3` | words |
| --- | --- | --- | --- | --- |
| 0 orders | 5.9 / 5.8 | 5.7 / 5.6 | 4.9 / 4.8 | 2658 / 2625 |
| 50 orders | 8.5 / 6.1 | 7.9 / 5.8 | 6.7 / 5.0 | 2841 / 2625 |
| 500 orders | 34.8 / 7.0 | 31.3 / 6.6 | 25.6 / 5.5 | 4618 / 2625 |
| 50 orders, churn | 10.3 / 8.7 | 9.6 / 8.0 | 8.2 / 7.0 | 2873 / 2723 |
| 500 orders, churn | 42.9 / 17.4 | 39.3 / 15.2 | 32.8 / 13.3 | 4650 / 2723 |

What changed:

- `Queue_ahead` indexes its entries by price, in order id order, as intrusive lists: a
  reference trade and a cap touch one price's entries, forgetting an order is O(1), and
  `refresh` walks each book level against its list with a cursor (a table lookup only for
  the anchor's own orders).
- The anchor skips the refresh while `Exchange.next_event_seq` has not moved since the
  queue last matched the book; its own shadow reconcile keeps the queue in sync.
- `Eligible.size` walks the book once instead of building, filtering and sorting a list of
  every resting order, and returns at once when the best passive price is not at or
  through the trade.
- Marks, the reference mid (`Ref_book.mid_or_null`, best prices cached on update) and the
  scaled price (`Scale.price_or_null`) are `or_null`, not options.

`test/test_alloc.ml` pins minor words per `apply` with the exchange clock held still: 569
for an unchanged BBO (the shadow reconcile through the engine) and 23 for a trade with no
eligible fills, both independent of the number of resting orders (before: 579 / 80 with
none, 1579 / 4080 with 500). The rest of an event's cost is the engine: placing and
cancelling shadow orders and `Exchange.step`. `test_queue_ahead.ml` and
`test_eligible.ml` check the new code against the old implementations, kept in `test/`,
with quickcheck.

## Limitations and open questions

- Trade-replay IOCs and shadow levels obey the simulator's minimum order value; below it
  the engine rejects them (`replays_rejected`, shadow `skipped`).
- The replay IOC matches in the book's price-time order, so at the trade price it can
  reach an older order with a long queue before a newer eligible one.
- Sources that carry only trades or funding never become fresh; they need a book (L2 or
  BBO) or a series.
- Open (from the design): library inside each simulator (chosen here) or a separate
  process; whether consumed shadow size should refill after a fixed time rather than on
  the next reference change; how to account for the two anchor accounts' positions when
  comparing a simulated strategy's PnL with the real venue's.

## Build

```
opam exec --switch 5.2.0+ox -- dune build @all @runtest
```
