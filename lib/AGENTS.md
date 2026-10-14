# AGENTS.md: lib/

The `price-anchor` library, module `Price_anchor`. Each module has an `.mli`; change the `.mli`
first and keep it documented with odoc.

- `anchor.ml`: the engine-facing state machine. `apply` takes one `Market_data.Event.t`,
  `apply_price` a series point, `step ~now` moves the exchange clock, `stats` reports counters.
- `source.ml`: reference items in `recv_ts` order from a feed, a raw log, a normalized log or a
  series. Feeds are closed by the caller; hook the pipe close to the feed's `close`.
- `path.ml`, `clamp.ml`: the reference price over time, and limits (LULD bands, daily limits).
- `shadow.ml`, `queue_ahead.ml`, `eligible.ml`, `ref_book.ml`: the shadow orders, the queue
  model, the trade-replay fills and the reference book.
- `scale.ml`: price and size factors, and rounding to the simulated tick (bids down, asks up).

Rules that matter here:

- Do not read the wall clock. Time comes from event `recv_ts` plus the fixed offset.
- Hot-path values are `or_null`; keep allocation counts in `test/test_alloc.ml` in mind.
- Every simulated action goes through the engine's public journaled calls, never through
  internal state.
