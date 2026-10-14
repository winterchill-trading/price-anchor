# AGENTS.md: bin/

The `price-anchor` command (`price_anchor_cli.ml`). It replays a recorded reference (`-norm`
event-log directory or `-series` file, between `-from` and `-to` dates) through an anchor into a
fresh simulated exchange, prints the counters, and checks that the exchange's journal replays to
the same state.

- Build from the workspace with `dune build --root .. @price-anchor/all`.
- It is an async command: keep the SIGINT and SIGTERM handler that flushes the journal.
- Use the same symbol and factor flags as the simulators' `-anchor*` options when comparing runs.
- Changing a flag name is a user-visible change: add a `CHANGES.md` entry.
