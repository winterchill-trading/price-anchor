(* Throughput of [Anchor.apply] on a synthetic reference: a 20-level snapshot, then L2
   deltas at the top levels and trades through the touch, against a simulated book with
   [clients] resting client orders. With [churn], the client cancels its oldest order and
   places it again before every event, so the anchor cannot skip its queue refresh; that
   cancel and place are not counted. Uses the wall clock to time the run only. *)

open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let instrument = Market_data.Instrument.create Binance "BTCUSDT"
let btc = Symbol.of_string "BTC"
let px units = Price.of_units (units * 10_000_000)
let sz s = Size.of_string s

let run ~events ~clients ~churn =
  let exchange =
    Exchange.create ~config:{ Exchange.Config.default with min_order_value = Usd.zero } ()
  in
  let anchor = Anchor.create ~exchange ~reference:instrument ~sim_symbol:btc () in
  let base = 1_000_000 (* 100000.0 in ticks of 0.1 *) in
  let ts i =
    Time_ns.of_int_ns_since_epoch (1_760_000_000_000_000_000 + (i * 1_000_000))
  in
  let level i size = { Market_data.L2_update.Level.price = px i; size = sz size } in
  Anchor.apply
    anchor
    (L2
       { instrument
       ; exch_ts = ts 0
       ; recv_ts = ts 0
       ; kind = Snapshot
       ; bids = List.init 20 ~f:(fun i -> level (base - i) "1")
       ; asks = List.init 20 ~f:(fun i -> level (base + 1 + i) "1")
       });
  let client = Account.of_string "client" in
  Exchange.deposit exchange client (Usd.of_int 1_000_000_000);
  let resting = Queue.create () in
  let place side p =
    match
      Exchange.place exchange client (Order_request.limit btc side ~px:(px p) ~size:(sz "0.01"))
    with
    | Resting { order_id; _ } -> Queue.enqueue resting (order_id, side, p)
    | _ -> ()
  in
  for i = 1 to clients do
    let side = if i % 2 = 0 then Side.Buy else Sell in
    let p = if Side.equal side Buy then base - 2 - (i % 10) else base + 3 + (i % 10) in
    place side p
  done;
  let start = Time_ns.now () in
  let words_before = Gc.minor_words () in
  let churn_time = ref Time_ns.Span.zero in
  let churn_words = ref 0 in
  for i = 1 to events do
    if churn
    then (
      let start = Time_ns.now () in
      let words = Gc.minor_words () in
      Option.iter (Queue.dequeue resting) ~f:(fun (order_id, side, p) ->
        ignore (Exchange.cancel exchange client ~symbol:btc order_id : Cancel_status.t);
        place side p);
      churn_words := !churn_words + (Gc.minor_words () - words);
      churn_time := Time_ns.Span.( + ) !churn_time (Time_ns.diff (Time_ns.now ()) start));
    let e : Market_data.Event.t =
      if i % 4 = 0
      then
        Trade
          { instrument
          ; exch_ts = ts i
          ; recv_ts = ts i
          ; price = px (if i % 8 = 0 then base else base + 1)
          ; size = sz "0.05"
          ; side = (if i % 8 = 0 then Sell else Buy)
          ; trade_id = Int.to_string i
          }
      else
        L2
          { instrument
          ; exch_ts = ts i
          ; recv_ts = ts i
          ; kind = Delta
          ; bids = [ level (base - (i % 5)) (sprintf "1.%d" (i % 7)) ]
          ; asks = [ level (base + 1 + (i % 5)) (sprintf "1.%d" (i % 9)) ]
          }
    in
    Anchor.apply anchor e;
    ignore (Exchange.drain_events exchange : Event.t list)
  done;
  (* The client's own cancel and place are the exchange's cost, not the anchor's. *)
  let words_per_event = (Gc.minor_words () - words_before - !churn_words) / events in
  let elapsed = Time_ns.Span.( - ) (Time_ns.diff (Time_ns.now ()) start) !churn_time in
  let per_event = Time_ns.Span.(elapsed / Float.of_int events) in
  print_s
    [%message
      ""
        (events : int)
        (clients : int)
        (churn : bool)
        (elapsed : Time_ns.Span.t)
        (per_event : Time_ns.Span.t)
        (words_per_event : int)
        ~stats:(Anchor.stats anchor : Anchor.Stats.t)]
;;

let () =
  List.iter [ false; true ] ~f:(fun churn ->
    List.iter [ 0; 50; 500 ] ~f:(fun clients -> run ~events:100_000 ~clients ~churn))
;;
