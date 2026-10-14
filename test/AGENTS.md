# AGENTS.md: test/

Inline tests (ppx_expect and Quickcheck) for every library module, run with
`dune build --root .. @price-anchor/runtest` from the repository root.

- `naive_eligible.ml` and `naive_queue_ahead.ml` are the slow reference implementations. Keep
  them; the property tests compare the fast code against them.
- `test_alloc.ml` pins minor words per event. The values differ between arm64 and amd64, so keep
  the comment that explains the tolerance.
- `test_tradfi.ml`, `test_source_files.ml` and `test_trade_replay.ml` cover the TradFi path, file
  sources and trade replay.
- `harness.ml` and `helpers.ml` hold shared fixtures; add to them rather than copying setup code.
- Promote expect output with `dune promote` only after reading the diff.
