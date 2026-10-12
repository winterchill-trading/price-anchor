# price-anchor

Ties a simulated exchange's market to a reference market, live or recorded: mark price,
shadow top-N levels and trade-replay fills, all through the engine's public calls so an
anchored run stays deterministic and journaled. Design:
[specs/design/price-anchor.md](https://github.com/winterchill-trading/specs/blob/main/design/price-anchor.md).
