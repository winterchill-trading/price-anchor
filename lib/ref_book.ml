open! Core
open! Exchange_core

type t =
  { mutable bids : Size.t Price.Map.t
  ; mutable asks : Size.t Price.Map.t
  }
[@@deriving sexp_of]

let create () = { bids = Price.Map.empty; asks = Price.Map.empty }

let clear t =
  t.bids <- Price.Map.empty;
  t.asks <- Price.Map.empty
;;

let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks

let set_level map (level : Market_data.L2_update.Level.t) =
  if Size.is_positive level.size
  then Map.set map ~key:level.price ~data:level.size
  else Map.remove map level.price
;;

let apply_l2 t (update : Market_data.L2_update.t) =
  (match update.kind with
   | Snapshot -> clear t
   | Delta -> ());
  t.bids <- List.fold update.bids ~init:t.bids ~f:set_level;
  t.asks <- List.fold update.asks ~init:t.asks ~f:set_level
;;

let apply_bbo t (bbo : Market_data.Bbo.t) =
  clear t;
  t.bids <- set_level t.bids { price = bbo.bid_price; size = bbo.bid_size };
  t.asks <- set_level t.asks { price = bbo.ask_price; size = bbo.ask_size }
;;

let best t (side : Side.t) =
  match side with
  | Buy -> Map.max_elt t.bids
  | Sell -> Map.min_elt t.asks
;;

let mid t =
  match best t Buy, best t Sell with
  | Some (bid, _), Some (ask, _) ->
    Some (Price.of_units ((Price.to_units bid + Price.to_units ask) / 2))
  | _ -> None
;;

let top t (side : Side.t) ~n =
  let map, order =
    match side with
    | Buy -> t.bids, `Decreasing_key
    | Sell -> t.asks, `Increasing_key
  in
  Map.to_sequence map ~order |> (fun s -> Sequence.take s n) |> Sequence.to_list
;;

let size_at t (side : Side.t) px =
  let map =
    match side with
    | Buy -> t.bids
    | Sell -> t.asks
  in
  Map.find map px |> Option.value ~default:Size.zero
;;
