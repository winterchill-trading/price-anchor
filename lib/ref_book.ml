open! Core
open! Exchange_core

type t =
  { mutable bids : Size.t Price.Map.t
  ; mutable asks : Size.t Price.Map.t
  ; mutable best_bid : Price.t or_null
  (** cached on every change, so [mid_or_null] reads it without a map lookup *)
  ; mutable best_ask : Price.t or_null
  }

let sexp_of_t { bids; asks; best_bid = _; best_ask = _ } =
  [%sexp { bids : Size.t Price.Map.t; asks : Size.t Price.Map.t }]
;;

let create () =
  { bids = Price.Map.empty; asks = Price.Map.empty; best_bid = Null; best_ask = Null }
;;

let clear t =
  t.bids <- Price.Map.empty;
  t.asks <- Price.Map.empty;
  t.best_bid <- Null;
  t.best_ask <- Null
;;

let key_or_null = function
  | None -> Null
  | Some (px, _) -> This px
;;

let cache_best t =
  t.best_bid <- key_or_null (Map.max_elt t.bids);
  t.best_ask <- key_or_null (Map.min_elt t.asks)
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
  t.asks <- List.fold update.asks ~init:t.asks ~f:set_level;
  cache_best t
;;

let apply_bbo t (bbo : Market_data.Bbo.t) =
  clear t;
  t.bids <- set_level t.bids { price = bbo.bid_price; size = bbo.bid_size };
  t.asks <- set_level t.asks { price = bbo.ask_price; size = bbo.ask_size };
  cache_best t
;;

let best t (side : Side.t) =
  match side with
  | Buy -> Map.max_elt t.bids
  | Sell -> Map.min_elt t.asks
;;

let mid_or_null t : Price.t or_null =
  match t.best_bid, t.best_ask with
  | This bid, This ask -> This (Price.of_units ((Price.to_units bid + Price.to_units ask) / 2))
  | Null, _ | _, Null -> Null
;;

let mid t =
  match mid_or_null t with
  | This px -> Some px
  | Null -> None
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
