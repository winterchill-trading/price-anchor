open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let anchor ?market_sim ?noise_traders ~exchange () =
  Anchor.create
    ?market_sim
    ?noise_traders
    ~exchange
    ~reference:Helpers.instrument
    ~sim_symbol:Harness.btc
    ()
;;

let show_mark (h : Harness.t) label =
  print_s
    [%message
      label
        ~now_ms:(Exchange.now h.exchange / 1_000_000 : int)
        ~mark:(Exchange.mark h.exchange Harness.btc : Price.t option)]
;;

let%expect_test "mark precedence: funding mark, then mid, then last trade" =
  let h = Harness.create (fun ~exchange ~market_sim:_ -> anchor ~exchange ()) in
  let apply label e =
    Anchor.apply h.anchor e;
    show_mark h label
  in
  (* A one-sided book makes the reference fresh but has no mid. *)
  apply "one-sided bbo" (Helpers.bbo ~at:0 ~bid:("100010", "1") ~ask:("100020", "0"));
  apply "trade" (Helpers.trade ~at:10 Buy ~px:"100000.04" ~sz:"0.1");
  apply
    "bbo within interval"
    (Helpers.bbo ~at:50 ~bid:("100010", "1") ~ask:("100020", "1"));
  Anchor.step h.anchor ~now:(Exchange.now h.exchange);
  show_mark h "step too early";
  Exchange.step h.exchange ~now:110_000_000;
  Anchor.step h.anchor ~now:110_000_000;
  show_mark h "step after interval";
  apply "trade under a book" (Helpers.trade ~at:300 Sell ~px:"99000" ~sz:"0.1");
  apply "funding mark" (Helpers.funding ~at:400 ~rate:0.0001 ~mark:"100100.06");
  apply "book moves" (Helpers.bbo ~at:500 ~bid:("100500", "1") ~ask:("100600", "1"));
  print_s [%sexp (Anchor.stats h.anchor : Anchor.Stats.t)];
  [%expect
    {|
    ("one-sided bbo" (now_ms 0) (mark ()))
    (trade (now_ms 10) (mark (100000)))
    ("bbo within interval" (now_ms 50) (mark (100000)))
    ("step too early" (now_ms 50) (mark (100000)))
    ("step after interval" (now_ms 110) (mark (100015)))
    ("trade under a book" (now_ms 300) (mark (100015)))
    ("funding mark" (now_ms 400) (mark (100100.1)))
    ("book moves" (now_ms 500) (mark (100100.1)))
    ((stale ()) (went_stale 1) (stale_ns 0) (gaps 0) (events 6) (ignored 0)
     (marks_set 3) (funding_rates_set 1)
     (shadow ((placed 4) (cancelled 2) (skipped 0) (consumed 0))) (replays 0)
     (replays_rejected 0))
    |}]
;;

let%expect_test "funding rates convert to the simulator's interval" =
  let h = Harness.create (fun ~exchange ~market_sim:_ -> anchor ~exchange ()) in
  Anchor.apply h.anchor (Helpers.funding ~at:0 ~rate:0.0008 ~mark:"100000");
  print_s [%sexp (Exchange.funding_rate h.exchange Harness.btc : float)];
  [%expect {| 0.0001 |}];
  let h =
    Harness.create (fun ~exchange ~market_sim:_ ->
      Anchor.create
        ~funding_interval:(Time_ns.Span.of_hr 1.)
        ~exchange
        ~reference:Helpers.instrument
        ~sim_symbol:Harness.btc
        ())
  in
  Anchor.apply h.anchor (Helpers.funding ~at:0 ~rate:0.0008 ~mark:"100000");
  print_s [%sexp (Exchange.funding_rate h.exchange Harness.btc : float)];
  [%expect {| 0.0008 |}]
;;

let%expect_test "events of another instrument are ignored" =
  let h = Harness.create (fun ~exchange ~market_sim:_ -> anchor ~exchange ()) in
  Anchor.apply
    h.anchor
    (Market_data.Event.Trade
       { instrument = Market_data.Instrument.create Bybit "BTCUSDT"
       ; exch_ts = Helpers.ms 0
       ; recv_ts = Helpers.ms 0
       ; price = Price.of_string "1"
       ; size = Size.of_string "1"
       ; side = Buy
       ; trade_id = "x"
       });
  print_s [%sexp (Anchor.stats h.anchor : Anchor.Stats.t)];
  [%expect
    {|
    ((stale (Waiting_for_book)) (went_stale 1) (stale_ns 0) (gaps 0) (events 0)
     (ignored 1) (marks_set 0) (funding_rates_set 0)
     (shadow ((placed 0) (cancelled 0) (skipped 0) (consumed 0))) (replays 0)
     (replays_rejected 0))
    |}]
;;

(* A reference that drifts up then down, with trades and funding. *)
let reference_events =
  List.concat_map (List.range 0 60) ~f:(fun i ->
    let at = i * 47 in
    let mid = 100_000. +. (Float.of_int (if i < 30 then i else 60 - i) *. 7.3) in
    let px x = sprintf "%.1f" x in
    [ Helpers.bbo ~at ~bid:(px (mid -. 0.5), "1.5") ~ask:(px (mid +. 0.5), "2")
    ; Helpers.trade ~at:(at + 5) (if i % 2 = 0 then Buy else Sell) ~px:(px mid) ~sz:"0.3"
    ]
    @
    if i % 25 = 0
    then
      [ Helpers.funding
          ~at:(at + 9)
          ~rate:(0.0001 *. Float.of_int (1 + (i / 25)))
          ~mark:(px mid)
      ]
    else [])
;;

let%expect_test "an anchored run with noise traders replays bit for bit from its journal" =
  let h =
    Harness.create
      ~sim_config:{ Market_sim.Config.default with seed = 3; noise_traders = 4 }
      (fun ~exchange ~market_sim -> anchor ?market_sim ~noise_traders:2 ~exchange ())
  in
  Harness.run ~extra_ticks:5 h reference_events;
  print_s
    [%message
      ""
        ~mark:(Exchange.mark h.exchange Harness.btc : Price.t option)
        ~reference:(Anchor.reference_price h.anchor : Price.t option)
        ~stats:(Anchor.stats h.anchor : Anchor.Stats.t)];
  Harness.check_replay h;
  [%expect
    {|
    ((mark (100073)) (reference (100073))
     (stats
      ((stale ()) (went_stale 1) (stale_ns 0) (gaps 0) (events 123) (ignored 0)
       (marks_set 3) (funding_rates_set 3)
       (shadow ((placed 118) (cancelled 116) (skipped 5) (consumed 2)))
       (replays 4) (replays_rejected 0))))
    (replay (inputs 641) (events 388) (same_events true) (same_mark true))
    |}]
;;

let%expect_test "a reference series alone makes the anchor fresh and sets the mark" =
  let h = Harness.create (fun ~exchange ~market_sim:_ -> anchor ~exchange ()) in
  Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 0) (Price.of_string "5000.25");
  show_mark h "series";
  Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 200) (Price.of_string "5001.33");
  show_mark h "series";
  print_s [%sexp (Anchor.is_stale h.anchor : bool)];
  [%expect
    {|
    (series (now_ms 0) (mark (5000.3)))
    (series (now_ms 200) (mark (5001.3)))
    false
    |}]
;;
