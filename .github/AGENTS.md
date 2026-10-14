# AGENTS.md: .github/

`workflows/ci.yml`, the only workflow of this repository.

- It runs the `ox-setup` composite action from `winterchill-trading/infra` with a `pins:` block of
  the sibling repositories the library needs, then builds and runs the tests.
- The pin list must match the dependencies in `price-anchor.opam` and `lib/dune`; update both
  when a dependency changes.
- Adding a pin changes the cache key, so the next run is a full cache miss. That is expected.
- To check a run, use `gh run list --limit 1`. Do not poll in a loop.
