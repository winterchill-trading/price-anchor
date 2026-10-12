open! Core
open! Exchange_core

type t =
  { instrument : Instrument.t
  ; price_factor : float
  ; size_factor : float
  }
[@@deriving sexp_of]

let create ?(price_factor = 1.) ?(size_factor = 1.) instrument =
  let check name f =
    if not (Float.is_finite f && Float.( > ) f 0.)
    then raise_s [%message "Scale.create: factor must be positive" name (f : float)]
  in
  check "price_factor" price_factor;
  check "size_factor" size_factor;
  { instrument; price_factor; size_factor }
;;

module Direction = struct
  type t =
    | Down
    | Up
    | Nearest
  [@@deriving sexp_of, compare, equal]
end

let scale_units units ~factor =
  if Float.equal factor 1.
  then units
  else Float.iround_nearest_exn (Float.of_int units *. factor)
;;

(* Rounds [units] to a multiple of [step] in [direction]. *)
let round_to units ~step ~(direction : Direction.t) =
  match direction with
  | Down -> units - (units % step)
  | Up -> if units % step = 0 then units else units - (units % step) + step
  | Nearest ->
    let r = units % step in
    if 2 * r >= step then units - r + step else units - r
;;

let rec pow10 n = if n <= 0 then 1 else 10 * pow10 (n - 1)

let digits n =
  let rec go n acc = if n = 0 then acc else go (n / 10) (acc + 1) in
  go n 0
;;

let price t px ~direction =
  let tick = Price.to_units t.instrument.price_tick in
  let units = scale_units (Price.to_units px) ~factor:t.price_factor in
  let rounded = round_to units ~step:tick ~direction in
  let rounded =
    if Instrument.valid_price t.instrument (Price.of_units rounded)
    then rounded
    else (
      match t.instrument.max_significant_figures with
      | None -> rounded
      | Some figures ->
        (* Too many significant figures: round to the place value of the last allowed
           figure, but never to more than whole units. *)
        let int_digits = digits (units / Price.units_per_one) in
        let step =
          Int.min Price.units_per_one (pow10 (Price.decimals + int_digits - figures))
        in
        let step = Int.max step tick in
        round_to units ~step ~direction)
  in
  if rounded <= 0 then None else Some (Price.of_units rounded)
;;

let size t sz =
  let lot = Size.to_units (Instrument.lot_size t.instrument) in
  let units = scale_units (Size.to_units sz) ~factor:t.size_factor in
  Size.of_units (Int.max 0 (round_to units ~step:lot ~direction:Down))
;;
