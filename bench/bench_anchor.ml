(* Throughput of [Anchor.apply] on a synthetic reference: a 20-level snapshot, then L2
   deltas at the top levels and trades through the touch, against a simulated book with
   [clients] resting client orders. Uses the wall clock to time the run only. *)

open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let instrument = Market_data.Instrument.create Binance "BTCUSDT"
let btc = Symbol.of_string "BTC"
let px units = Price.of_units (units * 10_000_000)
let sz s = Size.of_string s

let run ~events ~clients =
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
  for i = 1 to clients do
    let side = if i % 2 = 0 then Side.Buy else Sell in
    let p = if Side.equal side Buy then base - 2 - (i % 10) else base + 3 + (i % 10) in
    ignore
      (Exchange.place
         exchange
         client
         (Order_request.limit btc side ~px:(px p) ~size:(sz "0.01"))
       : Order_status.t)
  done;
  let start = Time_ns.now () in
  for i = 1 to events do
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
  let elapsed = Time_ns.diff (Time_ns.now ()) start in
  let per_event = Time_ns.Span.(elapsed / Float.of_int events) in
  print_s
    [%message
      ""
        (events : int)
        (clients : int)
        (elapsed : Time_ns.Span.t)
        (per_event : Time_ns.Span.t)
        ~stats:(Anchor.stats anchor : Anchor.Stats.t)]
;;

let () = List.iter [ 0; 50; 500 ] ~f:(fun clients -> run ~events:100_000 ~clients)
