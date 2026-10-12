(lang dune 3.17)

(name price-anchor)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/price-anchor))

(package
 (name price-anchor)
 (synopsis
  "Anchor a simulated exchange's market to a live or recorded reference market")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  fixed-decimal
  exchange-core
  exchange-core-engine
  order-book
  market-data
  ppx_jane))
