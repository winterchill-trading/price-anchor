open! Core
open! Exchange_core

let within_bands ?(margin = 0.003) instrument ~lower ~upper px =
  let lo = Price.to_float lower *. (1. +. margin) in
  let hi = Price.to_float upper *. (1. -. margin) in
  let p = Price.to_float px in
  if Float.( > ) lo hi || (Float.( >= ) p lo && Float.( <= ) p hi)
  then px
  else Instrument.round_price instrument (Float.clamp_exn p ~min:lo ~max:hi)
;;
