# AGENTS.md: price-anchor

## Purpose

price-anchor ties a simulated exchange's market to a reference market, live or recorded:
the mark price and funding follow the reference, the reference's top levels rest in the
simulated book, and passive simulated orders fill when the reference trades through them.
Every action goes through the engine's journaled calls, so an anchored run replays exactly.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `price-anchor` library (module `Price_anchor`): `Anchor`, `Source`, `Path`, `Clamp`, `Shadow`, `Queue_ahead`, `Eligible`, `Ref_book`, `Scale` |
| `bin/` | `price-anchor` CLI: replays a recorded reference into a fresh exchange and checks the journal |
| `test/` | Inline expect and Quickcheck tests, plus `naive_*` reference implementations the fast code is checked against |
| `bench/` | `bench_anchor.exe`: per-event timings with 0, 50 and 500 resting orders |
| `.github/` | `ci.yml` |
| `README.md` | Module table, time and mark rules, TradFi notes, performance table, limitations |

## Build, test, docs

Run from the repository root, inside the workspace (`..` is the workspace root):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @price-anchor/all
opam exec --switch 5.2.0+ox -- dune build --root .. @price-anchor/runtest
```

For parallel work, give each agent its own build directory, for example
`DUNE_BUILD_DIR=_build-price-anchor-<task>`.

Standalone (outside the workspace): add the organisation opam repository
(`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`),
install the dependencies with `opam install --deps-only .`, then run `dune build @all @runtest`.

Benchmark: `dune exec --root .. ./price-anchor/bench/bench_anchor.exe` from the workspace.

## Conventions

- Format only this repository: `dune build --root .. @price-anchor/fmt --auto-promote`, after
  the tests pass. Never run `dune fmt` at the workspace root.
- Every module has an `.mli`. Use labelled arguments, `open! Core`, `ppx_jane`, and
  `or_null` for hot-path values rather than options, as the existing code does.
- The anchor never reads the wall clock. The only wall-clock use is the optional pacing in
  `Source.run`. Keep it that way; replay must be deterministic.
- Tests: ppx_expect with `-source-tree-root .` plus Quickcheck. A fast implementation keeps
  its naive counterpart in `test/` and a property test compares the two.
- `test/test_alloc.ml` pins minor words per `apply`. Those counts differ between arm64 laptops
  and amd64 CI. Keep the comment that says so when you change a pin.
- Benchmark numbers are for one machine class; put the machine in the commit message or in
  the README table, never compare across machines.
- User-visible changes get a line under `Unreleased` in `CHANGES.md` if the file exists, and
  the design change goes to `../specs` in the same change (see `../specs/AGENTS.md`).

## Dependencies and dependents

The library depends on `fixed-decimal`, `exchange-core`, `exchange-core-engine`, `order-book`
and `market-data`, and CI pins `parquet` as well. Dependents: the simulators
`../hl-sim` (library and tests), `../binance-sim` (`anchor/`), `../bybit-sim` (`anchor/`),
`../nyse-sim`, `../cboe-sim` and `../cme-sim`. Those simulators gained `-anchor*` flags.
A change to `Anchor`'s signature needs a build of every one of them.

## CI

`.github/workflows/ci.yml` runs the `ox-setup` composite action from `winterchill-trading/infra`
with a `pins:` block listing the sibling repositories above, then `dune build`, then
`dune test`. The pin list must match the dependencies in `price-anchor.opam` and `lib/dune`.
To check a run, use `gh run list --limit 1`, and at most one `gh run watch <id> --exit-status`.
Do not poll in a loop.

## Pitfalls

- A replay at a speed other than 1 against a wall-clock exchange marks the anchor stale; use
  speed 1 for replays that must match.
- `Command.async` does not run shutdown hooks on SIGINT or SIGTERM. The CLI installs a signal
  handler so journals flush; keep it when you add a command.
- Under `open! Async`, `let%expect_test` expects `unit Deferred.t`. In tests use
  `Thread_safe.block_on_async_exn` with a local `let open Async`.
- Adding a field to a `[@@deriving sexp_of]` stats record changes every expect test that prints
  it. Promote those outputs in the same commit.
- An optional argument added to a module-type function breaks callers that pass no positional
  argument. Pass `?time_source:None` explicitly where needed.
- A simulator's anchor is replayable only with its input journal (every engine input), not
  with the client-request journal.

## Where to read more

- `README.md` (module table, rules for time, mark and fills, limitations).
- `../specs/design/price-anchor.md` for the design, `../specs/repos/price-anchor.md` for the
  repository summary.
- `../oxcaml-claude-skills/AGENTS.md` (workspace guide) and the skill
  `../oxcaml-claude-skills/skills/oxcaml-performance/` for the allocation and benchmark method.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "Price anchor".

## Never do

- Install an opam package that changes the compiler, `core`, `base`, `async` or `ppx_jane`
  on the shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Publish, tag or release.
- Read the wall clock inside `lib/` or let a test depend on real time.
- Commit credentials or recorded venue data that is not in the repository already.
