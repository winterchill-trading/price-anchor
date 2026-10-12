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

let tradfi ?half_life () =
  Harness.create ~sim_config (fun ~exchange ~market_sim ->
    Anchor.create
      ?half_life
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
