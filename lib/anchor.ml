open! Core
open! Exchange_core
open! Exchange_core_engine

module Stats = struct
  type t =
    { events : int
    ; ignored : int
    ; marks_set : int
    ; funding_rates_set : int
    ; shadow : Shadow.Stats.t
    ; replays : int
    ; replays_rejected : int
    }
  [@@deriving sexp_of]
end

type t =
  { exchange : Exchange.t
  ; market_sim : Market_sim.t option
  ; reference : Market_data.Instrument.t
  ; sim_symbol : Symbol.t
  ; scale : Scale.t
  ; levels : int
  ; mark_interval_ns : int
  ; funding_ratio : float (** sim interval / reference interval *)
  ; anchor_account : Account.t
  ; taker_account : Account.t
  ; log : Sexp.t -> unit
  ; book : Ref_book.t
  ; shadow : Shadow.t
  ; queue : Queue_ahead.t
  ; mutable has_l2 : bool
  ; mutable offset : int option
  ; mutable funding_mark : Price.t option
  ; mutable last_trade : Price.t option
  ; mutable mark_set : Price.t option
  ; mutable mark_set_at : int option
  ; mutable funding_rate_set : float option
  ; mutable events : int
  ; mutable ignored : int
  ; mutable marks_set : int
  ; mutable funding_rates_set : int
  ; mutable replays : int
  ; mutable replays_rejected : int
  }

let default_funding_interval = Time_ns.Span.of_hr 8.

let create
  ?price_factor
  ?size_factor
  ?(levels = 5)
  ?(mark_interval = Time_ns.Span.of_int_ms 100)
  ?funding_interval
  ?(noise_traders = 0)
  ?(balance = Usd.of_int 10_000_000_000)
  ?(log = ignore)
  ?market_sim
  ~exchange
  ~reference
  ~sim_symbol
  ()
  =
  let instrument =
    match Exchange.instrument exchange sim_symbol with
    | Some i -> i
    | None -> raise_s [%message "Anchor.create: unknown symbol" (sim_symbol : Symbol.t)]
  in
  let funding_interval =
    match funding_interval with
    | Some i -> i
    | None ->
      Market_data.Instrument_info.lookup reference
      |> Option.bind ~f:(fun (info : Market_data.Instrument_info.t) ->
        info.funding_interval)
      |> Option.value ~default:default_funding_interval
  in
  let funding_ratio =
    Float.of_int (Exchange.config exchange).funding_interval_ns
    /. Time_ns.Span.to_ns funding_interval
  in
  let name prefix =
    Account.of_string
      (sprintf "%s:%s" prefix (Market_data.Instrument.to_string reference))
  in
  let scale = Scale.create ?price_factor ?size_factor instrument in
  let anchor_account = name "anchor" in
  let t =
    { exchange
    ; market_sim
    ; reference
    ; sim_symbol
    ; scale
    ; levels
    ; mark_interval_ns = Time_ns.Span.to_int_ns mark_interval
    ; funding_ratio
    ; anchor_account
    ; taker_account = name "anchor-taker"
    ; log
    ; book = Ref_book.create ()
    ; shadow =
        Shadow.create exchange ~account:anchor_account ~symbol:sim_symbol ~scale ~levels
    ; queue = Queue_ahead.create ()
    ; has_l2 = false
    ; offset = None
    ; funding_mark = None
    ; last_trade = None
    ; mark_set = None
    ; mark_set_at = None
    ; funding_rate_set = None
    ; events = 0
    ; ignored = 0
    ; marks_set = 0
    ; funding_rates_set = 0
    ; replays = 0
    ; replays_rejected = 0
    }
  in
  Exchange.deposit exchange t.anchor_account balance;
  Exchange.deposit exchange t.taker_account balance;
  Option.iter market_sim ~f:(fun sim ->
    Market_sim.set_noise_traders sim sim_symbol noise_traders);
  t
;;

let stats t =
  { Stats.events = t.events
  ; ignored = t.ignored
  ; marks_set = t.marks_set
  ; funding_rates_set = t.funding_rates_set
  ; shadow = Shadow.stats t.shadow
  ; replays = t.replays
  ; replays_rejected = t.replays_rejected
  }
;;

let offset t = t.offset
let anchor_account t = t.anchor_account
let taker_account t = t.taker_account
let sim_symbol t = t.sim_symbol
let shadow_orders t = Shadow.orders t.shadow
let queue_ahead t order_id = Queue_ahead.find t.queue order_id

let is_anchor t account =
  Account.equal account t.anchor_account || Account.equal account t.taker_account
;;

let refresh_queue t =
  Option.iter (Exchange.book t.exchange t.sim_symbol) ~f:(fun book ->
    Queue_ahead.refresh t.queue book ~exclude:(is_anchor t) ~shown:(Shadow.shown t.shadow))
;;

(* Caps queues at prices the shadow levels cover by the reference size there. *)
let cap_queue t =
  let full = Shadow.full_levels t.book t.scale ~levels:t.levels in
  let reference side px =
    let levels =
      List.filter full ~f:(fun (l : Shadow.Level.t) -> Side.equal l.side side)
    in
    match List.last levels with
    | None -> None
    | Some worst ->
      let covered =
        match side with
        | Buy -> Price.( >= ) px worst.px
        | Sell -> Price.( <= ) px worst.px
      in
      if not covered
      then None
      else
        Some
          (List.find levels ~f:(fun l -> Price.equal l.px px)
           |> Option.value_map ~default:Size.zero ~f:(fun l -> l.size))
  in
  Queue_ahead.cap t.queue ~reference
;;

let on_book_update t =
  Shadow.reconcile t.shadow t.book;
  refresh_queue t;
  cap_queue t
;;

let raw_reference_price t =
  match t.funding_mark with
  | Some _ as p -> p
  | None ->
    (match Ref_book.mid t.book with
     | Some _ as p -> p
     | None -> t.last_trade)
;;

let reference_price t =
  Option.bind (raw_reference_price t) ~f:(fun px ->
    Scale.price t.scale px ~direction:Nearest)
;;

let update_mark t ~now =
  match reference_price t with
  | None -> ()
  | Some px ->
    let changed = not ([%equal: Price.t option] (Some px) t.mark_set) in
    let due =
      match t.mark_set_at with
      | None -> true
      | Some at -> now - at >= t.mark_interval_ns
    in
    if changed && due
    then (
      (match t.market_sim with
       | Some sim -> Market_sim.set_reference sim t.sim_symbol px
       | None -> Exchange.set_mark t.exchange t.sim_symbol px);
      t.mark_set <- Some px;
      t.mark_set_at <- Some now;
      t.marks_set <- t.marks_set + 1)
;;

(* [px] is at or through [p] for a resting order of [passive] side. *)
let at_or_through ~(passive : Side.t) ~p px =
  match passive with
  | Buy -> Price.( >= ) px p
  | Sell -> Price.( <= ) px p
;;

(* The size of simulated resting orders a reference trade of [q] at [p] reaches: orders
   strictly through [p] in full, orders at [p], oldest first, for what is left of [q]
   after their queue_ahead and the orders at [p] before them. *)
let eligible_size t ~passive ~p ~q =
  match Exchange.book t.exchange t.sim_symbol with
  | None -> Size.zero
  | Some book ->
    let orders =
      Order_book.orders book
      |> List.filter ~f:(fun (o : Order_book.Resting_order.t) ->
        Side.equal o.side passive
        && (not (is_anchor t o.owner))
        && at_or_through ~passive ~p o.px)
      |> List.sort ~compare:(fun (a : Order_book.Resting_order.t) b ->
        Comparable.lexicographic
          [ (fun (a : Order_book.Resting_order.t) b ->
              Int.compare a.timestamp b.timestamp)
          ; (fun a b -> Order_id.compare a.oid b.oid)
          ]
          a
          b)
    in
    let used_at_p = ref Size.zero in
    List.fold orders ~init:Size.zero ~f:(fun acc (o : Order_book.Resting_order.t) ->
      if not (Price.equal o.px p)
      then Size.( + ) acc o.remaining
      else (
        let ahead =
          match Queue_ahead.find t.queue o.oid with
          | Some ahead -> ahead
          | None -> Shadow.shown t.shadow passive p
        in
        let left = Size.( - ) (Size.( - ) q ahead) !used_at_p in
        let e = Size.min o.remaining (Size.max Size.zero left) in
        used_at_p := Size.( + ) !used_at_p e;
        Size.( + ) acc e))
;;

(* Trade replay: passive simulated orders fill when the reference trades at or through
   them. Cancel the shadow levels in the way, take the eligible size with an IOC from the
   taker account, re-place the shadow levels; all in one call, so no simulated client sees
   the book in between. *)
let replay_trade t (trade : Market_data.Trade.t) =
  match Scale.price t.scale trade.price ~direction:Nearest with
  | None -> ()
  | Some p ->
    let q = Scale.size t.scale trade.size in
    let passive = Side.flip trade.side in
    refresh_queue t;
    let eligible = eligible_size t ~passive ~p ~q in
    if Size.is_positive eligible
    then (
      Shadow.cancel_where t.shadow ~side:passive ~f:(at_or_through ~passive ~p);
      let request =
        Order_request.limit ~tif:Ioc t.sim_symbol trade.side ~px:p ~size:eligible
      in
      (match Exchange.place t.exchange t.taker_account request with
       | Filled _ -> t.replays <- t.replays + 1
       | Resting _ | Rejected _ -> t.replays_rejected <- t.replays_rejected + 1);
      Shadow.reconcile t.shadow t.book);
    Queue_ahead.trade t.queue ~passive ~px:p ~size:q
;;

let update_funding t (funding : Market_data.Funding.t) =
  t.funding_mark <- Some funding.mark_price;
  let rate = funding.rate *. t.funding_ratio in
  if not ([%equal: float option] (Some rate) t.funding_rate_set)
  then (
    Exchange.set_funding_rate t.exchange t.sim_symbol rate;
    t.funding_rate_set <- Some rate;
    t.funding_rates_set <- t.funding_rates_set + 1)
;;

(* Moves the exchange clock to the event's simulated time and returns the time the event
   is applied at. *)
let advance_clock t (event : Market_data.Event.t) =
  let recv_ns = Time_ns.to_int_ns_since_epoch (Market_data.Event.recv_ts event) in
  let now = Exchange.now t.exchange in
  let offset =
    match t.offset with
    | Some offset -> offset
    | None ->
      let offset = now - recv_ns in
      t.offset <- Some offset;
      t.log [%message "anchor: time offset fixed" (offset : int)];
      offset
  in
  let sim_time = recv_ns + offset in
  if sim_time > now
  then (
    Exchange.step t.exchange ~now:sim_time;
    sim_time)
  else now
;;

let apply t (event : Market_data.Event.t) =
  if not (Market_data.Instrument.equal (Market_data.Event.instrument event) t.reference)
  then t.ignored <- t.ignored + 1
  else (
    t.events <- t.events + 1;
    let now = advance_clock t event in
    (match event with
     | L2 update ->
       t.has_l2 <- true;
       Ref_book.apply_l2 t.book update;
       on_book_update t
     | Bbo bbo ->
       (* With depth available, the L2 book is the better source. *)
       if not t.has_l2
       then (
         Ref_book.apply_bbo t.book bbo;
         on_book_update t)
     | Trade trade ->
       t.last_trade <- Some trade.price;
       replay_trade t trade
     | Funding funding -> update_funding t funding);
    update_mark t ~now)
;;

let step t ~now =
  refresh_queue t;
  update_mark t ~now
;;
