open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

(* Words per call, averaged over [runs] calls after one warm-up call. Events share one
   [recv_ts], so the exchange clock does not move and [Exchange.step] is not counted. *)
let words_per_call f =
  let runs = 1_000 in
  f ();
  let before = Gc.minor_words () in
  for _ = 1 to runs do
    f ()
  done;
  Float.iround_nearest_exn (Float.of_int (Gc.minor_words () - before) /. Float.of_int runs)
;;

let client = Account.of_string "client"

(* An anchor with [clients] simulated orders resting two to eleven ticks away from the
   reference touch at 100000 / 100001. *)
let setup ~clients ~first =
  let exchange = Exchange.create ~config:Harness.config () in
  let anchor =
    Anchor.create ~exchange ~reference:Helpers.instrument ~sim_symbol:Harness.btc ()
  in
  Anchor.apply anchor first;
  Exchange.deposit exchange client (Usd.of_int 1_000_000_000);
  for i = 1 to clients do
    let side = if i % 2 = 0 then Side.Buy else Sell in
    let px =
      match side with
      | Buy -> 100_000 - 2 - (i % 10)
      | Sell -> 100_001 + 2 + (i % 10)
    in
    ignore
      (Exchange.place
         exchange
         client
         (Order_request.limit
            Harness.btc
            side
            ~px:(Price.of_int px)
            ~size:(Size.of_string "0.01"))
       : Order_status.t)
  done;
  ignore (Exchange.drain_events exchange : Event.t list);
  exchange, anchor
;;

let%expect_test "words per apply" =
  let report name ~clients ~first event =
    let exchange, anchor = setup ~clients ~first in
    let words =
      words_per_call (fun () ->
        Anchor.apply anchor event;
        ignore (Exchange.drain_events exchange : Event.t list))
    in
    print_s [%message name (clients : int) (words : int)]
  in
  let bbo = Helpers.bbo ~at:0 ~bid:("100000", "1") ~ask:("100001", "1") in
  let snapshot =
    Helpers.l2 ~at:0 ~bids:[ "100000", "1"; "99999", "2" ] ~asks:[ "100001", "1" ] ()
  in
  (* A buyer lifts the reference ask: no simulated sell order rests at or under it. *)
  let trade = Helpers.trade ~at:0 Buy ~px:"100001" ~sz:"0.1" in
  List.iter [ 0; 500 ] ~f:(fun clients ->
    report "unchanged BBO" ~clients ~first:bbo bbo;
    report "trade, no eligible fills" ~clients ~first:snapshot trade);
  [%expect {|
    ("unchanged BBO" (clients 0) (words 586))
    ("trade, no eligible fills" (clients 0) (words 58))
    ("unchanged BBO" (clients 500) (words 1586))
    ("trade, no eligible fills" (clients 500) (words 4058))
    |}]
;;
