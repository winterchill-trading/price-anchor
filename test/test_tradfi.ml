open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let tick_ns = 100_000_000

let sim_config =
  { Market_sim.Config.default with
    seed = 7
  ; symbols = [ Harness.btc ]
  ; initial_prices = [ Harness.btc, Price.of_string "100000" ]
  ; vol_bps = 1.
  ; noise_traders = 3
  }
;;

let tradfi ?half_life ?may_engage ?last_trade ?max_staleness () =
  Harness.create ~sim_config (fun ~exchange ~market_sim ->
    Anchor.create
      ?half_life
      ?may_engage
      ?last_trade
      ?max_staleness
      ?market_sim
      ~noise_traders:3
      ~exchange
      ~reference:Helpers.instrument
      ~sim_symbol:Harness.btc
      ())
;;

let sim (h : Harness.t) = Option.value_exn h.market_sim

(* One venue tick: the noise traders, then the clock, then the anchor. *)
let tick (h : Harness.t) =
  Market_sim.tick (sim h);
  let now = Exchange.now h.exchange + tick_ns in
  Exchange.step h.exchange ~now;
  Anchor.step h.anchor ~now
;;

let walk (h : Harness.t) = Market_sim.reference_price (sim h) Harness.btc

let%expect_test "a half-life pulls the walk towards the reference" =
  let h = tradfi ~half_life:Time_ns.Span.second () in
  Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 0) (Price.of_string "101000");
  let show label = print_s [%message label ~walk:(walk h : Price.t option)] in
  show "start";
  for second = 1 to 10 do
    for _ = 1 to 10 do
      tick h
    done;
    if List.mem [ 1; 2; 3; 5; 10 ] second ~equal:Int.equal
    then show (sprintf "after %ds" second)
  done;
  [%expect
    {|
    (start (walk (100000)))
    ("after 1s" (walk (100507.6)))
    ("after 2s" (walk (100761.7)))
    ("after 3s" (walk (100886.8)))
    ("after 5s" (walk (100978.3)))
    ("after 10s" (walk (100991)))
    |}]
;;

let%expect_test "a phase toggle pauses the anchor and restarts from the last trade" =
  let open_ = ref true in
  let last_trade = ref None in
  let h =
    tradfi
      ~half_life:Time_ns.Span.second
      ~may_engage:(fun () -> !open_)
      ~last_trade:(fun () -> !last_trade)
      ~max_staleness:(Time_ns.Span.of_min 1.)
      ()
  in
  Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 0) (Price.of_string "101000");
  let show label =
    let stats = Anchor.stats h.anchor in
    print_s
      [%message
        label
          ~walk:(walk h : Price.t option)
          ~paused:(Anchor.is_paused h.anchor : bool)
          ~waits:(stats.waits : int)
          ~resumes:(stats.resumes : int)]
  in
  let ticks n =
    for _ = 1 to n do
      tick h
    done
  in
  ticks 30;
  show "core session";
  open_ := false;
  ticks 1;
  show "halted";
  (* The venue trades on its own while halted (e.g. a reopening auction). *)
  last_trade := Some (Price.of_string "99000");
  ticks 19;
  show "still halted";
  open_ := true;
  ticks 1;
  show "reopened";
  ticks 10;
  show "1s later";
  ticks 40;
  show "5s later";
  [%expect
    {|
    ("core session" (walk (100886.8)) (paused false) (waits 0) (resumes 0))
    (halted (walk (100900.1)) (paused true) (waits 1) (resumes 0))
    ("still halted" (walk (100909.9)) (paused true) (waits 20) (resumes 0))
    (reopened (walk (99000)) (paused false) (waits 20) (resumes 1))
    ("1s later" (walk (100012.1)) (paused false) (waits 20) (resumes 1))
    ("5s later" (walk (100947.1)) (paused false) (waits 20) (resumes 1))
    |}]
;;

let%expect_test "without step_clock the venue keeps its clock" =
  let run ~step_clock =
    let h =
      Harness.create (fun ~exchange ~market_sim:_ ->
        Anchor.create
          ~step_clock
          ~exchange
          ~reference:Helpers.instrument
          ~sim_symbol:Harness.btc
          ())
    in
    Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 0) (Price.of_string "100000");
    Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 2_500) (Price.of_string "100100");
    print_s
      [%message
        ""
          (step_clock : bool)
          ~now_ms:(Exchange.now h.exchange / 1_000_000 : int)
          ~mark:(Exchange.mark h.exchange Harness.btc : Price.t option)];
    (* The venue's own clock moves on; the held-back mark follows. *)
    let now = Exchange.now h.exchange + 200_000_000 in
    Exchange.step h.exchange ~now;
    Anchor.step h.anchor ~now;
    print_s
      [%message
        "venue step" ~mark:(Exchange.mark h.exchange Harness.btc : Price.t option)]
  in
  run ~step_clock:true;
  run ~step_clock:false;
  [%expect
    {|
    ((step_clock true) (now_ms 2500) (mark (100100)))
    ("venue step" (mark (100100)))
    ((step_clock false) (now_ms 0) (mark (100000)))
    ("venue step" (mark (100100)))
    |}]
;;

let%expect_test "a series is not stale between sparse points, a feed is" =
  let run ?max_staleness label apply =
    let h =
      Harness.create (fun ~exchange ~market_sim:_ ->
        Anchor.create
          ?max_staleness
          ~exchange
          ~reference:Helpers.instrument
          ~sim_symbol:Harness.btc
          ())
    in
    apply h;
    let now = Exchange.now h.exchange + Time_ns.Span.to_int_ns (Time_ns.Span.of_min 1.) in
    Exchange.step h.exchange ~now;
    Anchor.step h.anchor ~now;
    print_s
      [%message
        label ~stale:((Anchor.stats h.anchor).stale : Anchor.Stale_reason.t option)]
  in
  let series (h : Harness.t) =
    Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms 0) (Price.of_string "100000")
  in
  let feed (h : Harness.t) =
    Anchor.apply h.anchor (Helpers.bbo ~at:0 ~bid:("99990", "1") ~ask:("100010", "1"))
  in
  run "series" series;
  run "series, explicit max_staleness" ~max_staleness:(Time_ns.Span.of_sec 5.) series;
  run "feed" feed;
  [%expect
    {|
    (series (stale ()))
    ("series, explicit max_staleness" (stale (Silent)))
    (feed (stale (Silent)))
    |}]
;;

let%expect_test "a clamp keeps the target inside the venue's bands" =
  let upper = ref (Price.of_string "100500") in
  let h =
    Harness.create (fun ~exchange ~market_sim:_ ->
      Anchor.create
        ~clamp:(fun px -> Price.min px !upper)
        ~exchange
        ~reference:Helpers.instrument
        ~sim_symbol:Harness.btc
        ())
  in
  let point ~ms px =
    Anchor.apply_price h.anchor ~recv_ts:(Helpers.ms ms) (Price.of_string px);
    let now = Exchange.now h.exchange + tick_ns in
    Exchange.step h.exchange ~now;
    Anchor.step h.anchor ~now;
    print_s
      [%message
        px
          ~mark:(Exchange.mark h.exchange Harness.btc : Price.t option)
          ~clamped:((Anchor.stats h.anchor).clamped : int)]
  in
  point ~ms:0 "100000";
  point ~ms:1_000 "101000";
  (* The venue's band moves up with its own trades; the clamp reads it each time. *)
  upper := Price.of_string "100800";
  point ~ms:2_000 "101000";
  point ~ms:3_000 "100200";
  [%expect
    {|
    (100000 (mark (100000)) (clamped 0))
    (101000 (mark (100500)) (clamped 1))
    (101000 (mark (100800)) (clamped 2))
    (100200 (mark (100200)) (clamped 2))
    |}]
;;
