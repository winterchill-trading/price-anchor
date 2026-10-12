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
    ("after 10s" (walk (100979.3)))
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
