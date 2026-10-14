# AGENTS.md: bench/

`bench_anchor.exe`: microseconds and minor words per event for 100k events (75% L2 deltas at the
top levels, 25% trades), against 0, 50 or 500 resting client orders, with and without churn.

- Run it with `dune exec --root .. ./price-anchor/bench/bench_anchor.exe` from the workspace.
- Results depend on the machine. The README table records an arm64 laptop; CI runs amd64. Put the
  machine and the commit in any table you update.
- Do not turn a benchmark number into a test pin without a comment that names the platform.
