open! Core
open! Exchange_core
open! Exchange_core_engine

module Stale_reason = struct
  type t =
    | Waiting_for_book
    | Silent
    | Gap of Market_data.Gap.Reason.t
  [@@deriving sexp_of, compare, equal]
end

module Stats = struct
  type t =
    { stale : Stale_reason.t option
    ; went_stale : int
    ; stale_ns : int
    ; gaps : int
    ; events : int
    ; ignored : int
    ; marks_set : int
    ; funding_rates_set : int
    ; shadow : Shadow.Stats.t
    ; replays : int
    ; replays_rejected : int
    ; waits : int
    ; resumes : int
    ; clamped : int
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
  ; max_staleness_ns : int option (** [None]: the default, which depends on the source *)
  ; noise_traders : int
  ; half_life : Time_ns.Span.t option
  ; may_engage : unit -> bool
  ; step_clock : bool
  ; venue_last_trade : unit -> Price.t option
  ; clamp : (Price.t -> Price.t) option
  ; funding_ratio : float (** sim interval / reference interval *)
  ; anchor_account : Account.t
  ; taker_account : Account.t
  ; log : Sexp.t -> unit
  ; book : Ref_book.t
  ; shadow : Shadow.t
  ; queue : Queue_ahead.t
  ; mutable has_l2 : bool
  ; mutable has_bbo : bool
  ; mutable series_price : Price.t option
  ; mutable stale : Stale_reason.t option
  ; mutable stale_since : int
  ; mutable stale_ns : int
  ; mutable went_stale : int
  ; mutable gaps : int
  ; mutable last_event_at : int option
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
  ; mutable paused : bool
  ; mutable waits : int
  ; mutable resumes : int
  ; mutable clamped : int
  }

let default_funding_interval = Time_ns.Span.of_hr 8.

let create
  ?price_factor
  ?size_factor
  ?(levels = 5)
  ?(mark_interval = Time_ns.Span.of_int_ms 100)
  ?max_staleness
  ?funding_interval
  ?(noise_traders = 0)
  ?half_life
  ?(may_engage = fun () -> true)
  ?(last_trade = fun () -> None)
  ?(step_clock = true)
  ?clamp
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
  Option.iter half_life ~f:(fun half_life ->
    if Time_ns.Span.( <= ) half_life Time_ns.Span.zero
    then
      raise_s
        [%message
          "Anchor.create: half_life must be positive" (half_life : Time_ns.Span.t)]);
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
    ; max_staleness_ns = Option.map max_staleness ~f:Time_ns.Span.to_int_ns
    ; noise_traders
    ; half_life
    ; may_engage
    ; step_clock
    ; venue_last_trade = last_trade
    ; clamp
    ; funding_ratio
    ; anchor_account
    ; taker_account = name "anchor-taker"
    ; log
    ; book = Ref_book.create ()
    ; shadow =
        Shadow.create exchange ~account:anchor_account ~symbol:sim_symbol ~scale ~levels
    ; queue = Queue_ahead.create ()
    ; has_l2 = false
    ; has_bbo = false
    ; series_price = None
    ; stale = Some Waiting_for_book
    ; stale_since = Exchange.now exchange
    ; stale_ns = 0
    ; went_stale = 1
    ; gaps = 0
    ; last_event_at = None
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
    ; paused = false
    ; waits = 0
    ; resumes = 0
    ; clamped = 0
    }
  in
  Exchange.deposit exchange t.anchor_account balance;
  Exchange.deposit exchange t.taker_account balance;
  Option.iter market_sim ~f:(fun sim ->
    Market_sim.set_noise_traders sim sim_symbol noise_traders);
  t
;;

let stats t =
  { Stats.stale = t.stale
  ; went_stale = t.went_stale
  ; stale_ns =
      (match t.stale with
       | None -> t.stale_ns
       | Some _ -> t.stale_ns + (Exchange.now t.exchange - t.stale_since))
  ; gaps = t.gaps
  ; events = t.events
  ; ignored = t.ignored
  ; marks_set = t.marks_set
  ; funding_rates_set = t.funding_rates_set
  ; shadow = Shadow.stats t.shadow
  ; replays = t.replays
  ; replays_rejected = t.replays_rejected
  ; waits = t.waits
  ; resumes = t.resumes
  ; clamped = t.clamped
  }
;;

let offset t = t.offset
let is_paused t = t.paused
let is_stale t = Option.is_some t.stale
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
  let side_cap (side : Side.t) =
    let levels =
      List.filter full ~f:(fun (l : Shadow.Level.t) -> Side.equal l.side side)
    in
    match List.last levels with
    | None -> fun _ -> None
    | Some worst ->
      let sizes =
        Price.Map.of_alist_reduce
          (List.map levels ~f:(fun (l : Shadow.Level.t) -> l.px, l.size))
          ~f:Size.( + )
      in
      fun px ->
        let covered =
          match side with
          | Buy -> Price.( >= ) px worst.px
          | Sell -> Price.( <= ) px worst.px
        in
        if covered
        then Some (Map.find sizes px |> Option.value ~default:Size.zero)
        else None
  in
  let bids = side_cap Buy
  and asks = side_cap Sell in
  Queue_ahead.cap t.queue ~reference:(fun (side : Side.t) px ->
    match side with
    | Buy -> bids px
    | Sell -> asks px)
;;

let on_book_update t =
  Shadow.reconcile t.shadow t.book;
  refresh_queue t;
  cap_queue t
;;

let raw_reference_price t =
  match Option.first_some t.series_price t.funding_mark with
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

(* The target after the venue's clamp (LULD bands, daily limits), and whether the clamp
   moved it. *)
let clamp_target t px =
  match t.clamp with
  | None -> px, false
  | Some clamp ->
    let clamped = clamp px in
    clamped, not (Price.equal clamped px)
;;

let set_mark t ~now =
  match reference_price t with
  | None -> ()
  | Some px ->
    let px, was_clamped = clamp_target t px in
    let changed = not ([%equal: Price.t option] (Some px) t.mark_set) in
    let due =
      match t.mark_set_at with
      | None -> true
      | Some at -> now - at >= t.mark_interval_ns
    in
    if changed && due
    then (
      (match t.market_sim with
       | Some sim -> Market_sim.set_reference ?half_life:t.half_life sim t.sim_symbol px
       | None -> Exchange.set_mark t.exchange t.sim_symbol px);
      t.mark_set <- Some px;
      t.mark_set_at <- Some now;
      t.marks_set <- t.marks_set + 1;
      if was_clamped then t.clamped <- t.clamped + 1)
;;

(* Whether the venue lets the anchor move the price now. The first refusal hands the
   symbol back to the walk; the next permission restarts the walk from the venue's last
   trade and sets the reference again at once. *)
let may_engage t =
  if t.may_engage ()
  then (
    if t.paused
    then (
      t.paused <- false;
      t.resumes <- t.resumes + 1;
      let restart_from = t.venue_last_trade () in
      (match t.market_sim, restart_from with
       | Some sim, Some px when Price.( > ) px Price.zero ->
         Market_sim.set_reference sim t.sim_symbol px
       | _ -> ());
      t.mark_set <- None;
      t.mark_set_at <- None;
      t.log
        [%message
          "anchor: engaged"
            ~reference:(Market_data.Instrument.to_string t.reference)
            (restart_from : Price.t option)]);
    true)
  else (
    t.waits <- t.waits + 1;
    if not t.paused
    then (
      t.paused <- true;
      Option.iter t.market_sim ~f:(fun sim -> Market_sim.clear_reference sim t.sim_symbol);
      t.log
        [%message
          "anchor: paused" ~reference:(Market_data.Instrument.to_string t.reference)]);
    false)
;;

let update_mark t ~now = if may_engage t then set_mark t ~now

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

(* Stale: no shadow levels, no trade replay, no set_mark; the noise traders' walk takes
   over from the last price if the instrument has any. *)
let go_stale t ~now reason =
  match t.stale with
  | Some _ -> ()
  | None ->
    t.stale <- Some reason;
    t.stale_since <- now;
    t.went_stale <- t.went_stale + 1;
    Shadow.cancel_all t.shadow;
    (match t.market_sim with
     | Some sim when t.noise_traders > 0 -> Market_sim.clear_reference sim t.sim_symbol
     | Some _ | None -> ());
    t.log
      [%message
        "anchor: reference stale"
          ~reference:(Market_data.Instrument.to_string t.reference)
          (reason : Stale_reason.t)
          (now : int)]
;;

let go_fresh t ~now =
  match t.stale with
  | None -> ()
  | Some reason ->
    let stale_for_ns = now - t.stale_since in
    t.stale_ns <- t.stale_ns + stale_for_ns;
    t.stale <- None;
    (* Pin the reference again at once, even at an unchanged price. *)
    t.mark_set <- None;
    t.mark_set_at <- None;
    t.log
      [%message
        "anchor: reference fresh"
          ~reference:(Market_data.Instrument.to_string t.reference)
          ~was:(reason : Stale_reason.t)
          (stale_for_ns : int)
          (now : int)]
;;

let default_max_staleness_ns = Time_ns.Span.to_int_ns (Time_ns.Span.of_sec 5.)

(* A series alone is sparse by nature: by default it never goes silent. *)
let max_staleness_ns t =
  match t.max_staleness_ns with
  | Some _ as explicit -> explicit
  | None ->
    let series_only = Option.is_some t.series_price && not (t.has_l2 || t.has_bbo) in
    if series_only then None else Some default_max_staleness_ns
;;

let check_silence t ~now =
  match t.stale, t.last_event_at, max_staleness_ns t with
  | None, Some last, Some max when now - last > max -> go_stale t ~now Silent
  | _ -> ()
;;

let relevant_gap t (gap : Market_data.Gap.t) =
  Market_data.Venue.equal gap.venue t.reference.venue
  && Option.for_all gap.instrument ~f:(Market_data.Instrument.equal t.reference)
  && Option.for_all gap.stream ~f:(fun (s : Market_data.Stream.t) ->
    match s with
    | Trades | Bbo | L2 | Funding -> true
    | Private -> false)
;;

let gap t (gap : Market_data.Gap.t) =
  if relevant_gap t gap
  then (
    t.gaps <- t.gaps + 1;
    Ref_book.clear t.book;
    go_stale t ~now:(Exchange.now t.exchange) (Gap gap.reason))
;;

(* Moves the exchange clock to the event's simulated time (unless the venue owns it).
   Returns the event's simulated time ([recv_ts + offset]) and the time it is applied at
   (not before the clock). *)
let advance_clock t recv_ts =
  let recv_ns = Time_ns.to_int_ns_since_epoch recv_ts in
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
  if sim_time > now && t.step_clock
  then (
    Exchange.step t.exchange ~now:sim_time;
    sim_time, sim_time)
  else sim_time, now
;;

let apply t (event : Market_data.Event.t) =
  if not (Market_data.Instrument.equal (Market_data.Event.instrument event) t.reference)
  then t.ignored <- t.ignored + 1
  else (
    t.events <- t.events + 1;
    let event_time, now = advance_clock t (Market_data.Event.recv_ts event) in
    check_silence t ~now:event_time;
    t.last_event_at <- Some event_time;
    (match event with
     | L2 update ->
       t.has_l2 <- true;
       Ref_book.apply_l2 t.book update;
       (match update.kind with
        | Snapshot -> go_fresh t ~now
        | Delta -> ());
       if not (is_stale t) then on_book_update t
     | Bbo bbo ->
       (* With depth available, the L2 book is the better source. *)
       t.has_bbo <- true;
       if not t.has_l2
       then (
         Ref_book.apply_bbo t.book bbo;
         go_fresh t ~now;
         on_book_update t)
     | Trade trade ->
       t.last_trade <- Some trade.price;
       if not (is_stale t) then replay_trade t trade
     | Funding funding -> update_funding t funding);
    if not (is_stale t) then update_mark t ~now)
;;

let apply_price t ~recv_ts px =
  t.events <- t.events + 1;
  let event_time, now = advance_clock t recv_ts in
  check_silence t ~now:event_time;
  t.last_event_at <- Some event_time;
  t.series_price <- Some px;
  (* A series is the whole source unless a book arrives too. *)
  if not (t.has_l2 || t.has_bbo) then go_fresh t ~now;
  if not (is_stale t) then update_mark t ~now
;;

let step t ~now =
  check_silence t ~now;
  if not (is_stale t)
  then (
    refresh_queue t;
    update_mark t ~now)
;;
