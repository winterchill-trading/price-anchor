open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let btc = Harness.btc

let log sexp =
  (* Times are noisy in expectations; keep the message and the reason. *)
  match sexp with
  | Sexp.List (msg :: _ :: reason :: _) -> print_s (Sexp.List [ msg; reason ])
  | s -> print_s s
;;

let show (h : Harness.t) label =
  let stats = Anchor.stats h.anchor in
  print_s
    [%message
      label
        ~stale:(stats.stale : Anchor.Stale_reason.t option)
        ~shadow_orders:(List.length (Anchor.shadow_orders h.anchor) : int)
        ~mark:(Exchange.mark h.exchange btc : Price.t option)]
;;

let snapshot ~at =
  Helpers.l2 ~at ~bids:[ "100000", "2"; "99999", "1" ] ~asks:[ "100001", "1" ] ()
;;

let%expect_test "stale until the first snapshot, after silence and after a gap" =
  let h =
    Harness.create (fun ~exchange ~market_sim:_ ->
      Anchor.create ~log ~exchange ~reference:Helpers.instrument ~sim_symbol:btc ())
  in
  let apply label e =
    Anchor.apply h.anchor e;
    show h label
  in
  apply
    "delta before any snapshot"
    (Helpers.l2 ~kind:Delta ~at:0 ~bids:[ "100000", "1" ] ~asks:[] ());
  apply "trade" (Helpers.trade ~at:5 Sell ~px:"100000" ~sz:"1");
  apply "snapshot" (snapshot ~at:10);
  [%expect
    {|
    ("anchor: time offset fixed" (offset -1760000000000000000))
    ("delta before any snapshot" (stale (Waiting_for_book)) (shadow_orders 0)
     (mark ()))
    (trade (stale (Waiting_for_book)) (shadow_orders 0) (mark ()))
    ("anchor: reference fresh" (was Waiting_for_book))
    (snapshot (stale ()) (shadow_orders 3) (mark (100000.5)))
    |}];
  Exchange.step h.exchange ~now:4_000_000_000;
  Anchor.step h.anchor ~now:4_000_000_000;
  show h "4s later";
  Exchange.step h.exchange ~now:6_000_000_000;
  Anchor.step h.anchor ~now:6_000_000_000;
  show h "6s later";
  apply
    "delta while stale"
    (Helpers.l2 ~kind:Delta ~at:6_100 ~bids:[ "100000", "5" ] ~asks:[] ());
  apply "snapshot again" (snapshot ~at:6_200);
  [%expect
    {|
    ("4s later" (stale ()) (shadow_orders 3) (mark (100000.5)))
    ("anchor: reference stale" (reason Silent))
    ("6s later" (stale (Silent)) (shadow_orders 0) (mark (100000.5)))
    ("delta while stale" (stale (Silent)) (shadow_orders 0) (mark (100000.5)))
    ("anchor: reference fresh" (was Silent))
    ("snapshot again" (stale ()) (shadow_orders 3) (mark (100000.5)))
    |}];
  Anchor.gap
    h.anchor
    { recv_ts = Helpers.ms 6_300
    ; venue = Binance
    ; instrument = Some Helpers.instrument
    ; stream = Some L2
    ; reason = Disconnect
    };
  show h "gap";
  Anchor.gap
    h.anchor
    { recv_ts = Helpers.ms 6_300
    ; venue = Bybit
    ; instrument = None
    ; stream = None
    ; reason = Disconnect
    };
  apply
    "bbo does not refresh an L2 source"
    (Helpers.bbo ~at:6_400 ~bid:("100000", "1") ~ask:("100001", "1"));
  apply "snapshot after the gap" (snapshot ~at:7_000);
  print_s [%sexp (Anchor.stats h.anchor : Anchor.Stats.t)];
  [%expect
    {|
    ("anchor: reference stale" (reason (Gap Disconnect)))
    (gap (stale ((Gap Disconnect))) (shadow_orders 0) (mark (100000.5)))
    ("bbo does not refresh an L2 source" (stale ((Gap Disconnect)))
     (shadow_orders 0) (mark (100000.5)))
    ("anchor: reference fresh" (was (Gap Disconnect)))
    ("snapshot after the gap" (stale ()) (shadow_orders 3) (mark (100000.5)))
    ((stale ()) (went_stale 3) (stale_ns 1010000000) (gaps 1) (events 7)
     (ignored 0) (marks_set 3) (funding_rates_set 0)
     (shadow ((placed 9) (cancelled 6) (skipped 0) (consumed 0))) (replays 0)
     (replays_rejected 0))
    |}]
;;

let%expect_test "while stale the noise traders' walk takes over, and the anchor pins it \
                 again"
  =
  let h =
    Harness.create
      ~sim_config:{ Market_sim.Config.default with seed = 5; vol_bps = 50. }
      (fun ~exchange ~market_sim ->
         Anchor.create
           ?market_sim
           ~noise_traders:2
           ~max_staleness:(Time_ns.Span.of_sec 1.)
           ~exchange
           ~reference:Helpers.instrument
           ~sim_symbol:btc
           ())
  in
  let sim = Option.value_exn h.market_sim in
  let reference () = Market_sim.reference_price sim btc in
  Harness.run h [ snapshot ~at:0 ];
  print_s [%message "fresh" ~reference:(reference () : Price.t option)];
  (* Five silent seconds of ticks. *)
  Harness.run h [ Helpers.trade ~at:5_000 Buy ~px:"100000.5" ~sz:"0.1" ];
  print_s
    [%message
      "stale"
        ~reference:(reference () : Price.t option)
        ~stats:((Anchor.stats h.anchor).stale : Anchor.Stale_reason.t option)];
  Harness.run h [ snapshot ~at:5_100 ];
  print_s
    [%message
      "fresh again"
        ~reference:(reference () : Price.t option)
        ~mark:(Exchange.mark h.exchange btc : Price.t option)];
  Harness.check_replay h;
  [%expect
    {|
    (fresh (reference (100000.5)))
    (stale (reference (99712.6)) (stats (Silent)))
    ("fresh again" (reference (100000.5)) (mark (100000.5)))
    (replay (inputs 698) (events 714) (same_events true) (same_mark true))
    |}]
;;
