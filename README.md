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
| `Source` | reference items in `recv_ts` order: `of_feed`, `of_feed_with_gaps`, `of_raw`, `of_norm`, `of_series`; `run ?speed ?time_source source ~anchor` |
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
let anchor =
  Price_anchor.Anchor.create
    ~market_sim
    ~noise_traders:(Market_sim.config market_sim).noise_traders
    ~half_life:Time_ns.Span.second   (* pull the walk, do not pin it *)
    ~step_clock:false                (* the venue's clock *)
    ~levels:0                        (* no depth to shadow *)
    ~may_engage:(fun () ->           (* core session, no auction, no LULD pause or limit halt *)
      Venue.continuous_trading venue ~symbol_id)
    ~last_trade:(fun () -> Venue.last_price venue ~symbol_id)
    ~price_factor
    ~exchange
    ~reference:(Market_data.Instrument.of_string "cme:ES")
    ~sim_symbol
    ()
in
(* the series, paced on the venue's time source: *)
let%bind source = Price_anchor.Source.of_series path >>| ok_exn in
don't_wait_for (Price_anchor.Source.run ~speed:1. ~time_source source ~anchor);
(* on each noise tick, after the venue stepped its clock: *)
Price_anchor.Anchor.step anchor ~now:(Exchange.now exchange)
```

- `half_life` sets the reference through `Market_sim.set_reference ~half_life`; without it
  the reference is pinned.
- While `may_engage` says no, the anchor hands the symbol back to the free walk
  (`clear_reference`, once) and counts waits (`Stats.waits`). When it says yes again the
  walk restarts from `last_trade` (the opening or reopening auction's price) and climbs
  back towards the reference (`Stats.resumes`).
- A series alone never goes stale between its points; pass `max_staleness` to change
  that. Feeds keep the 5 s default.
- `Source.of_series` returns `Error` for a missing or malformed file.
- Price bands (LULD, CME daily limits) stay the venue's job: clamp the target before it
  reaches the anchor if the noise traders must not trade through them.

## Binary

```
price-anchor replay -reference binance:BTCUSDT -norm DIR -from 2026-01-01 -to 2026-01-01 [-symbol BTC] [-journal FILE] [-anchor-speed X] [-verbose]
price-anchor replay -reference cme:ES -series points.txt -symbol BTC
```

It runs the reference into a fresh default exchange, prints the anchor's counters, and
checks that the journal replays to the same mark and book.

## Performance

`bench/bench_anchor.exe` (M-series laptop, 100k events, 75% L2 deltas at the top levels,
25% trades, one event per millisecond): 6.5 us per event with an empty simulated book,
9 us with 50 resting client orders, 36 us with 500. Each book update reconciles the
shadows through the engine and scans the simulated book for new client orders.

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
