open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let%expect_test "within_bands keeps the target 30 bps inside" =
  let exchange = Exchange.create ~config:Harness.config () in
  let instrument = Option.value_exn (Exchange.instrument exchange Harness.btc) in
  let clamp lower upper px =
    let px = Price.of_string px in
    let clamped =
      Clamp.within_bands
        instrument
        ~lower:(Price.of_string lower)
        ~upper:(Price.of_string upper)
        px
    in
    print_s [%message "" (px : Price.t) (clamped : Price.t)]
  in
  clamp "95000" "105000" "100000";
  clamp "95000" "105000" "110000";
  clamp "95000" "105000" "90000";
  (* Bands narrower than twice the margin leave the target alone. *)
  clamp "99900" "100100" "110000";
  [%expect
    {|
    ((px 100000) (clamped 100000))
    ((px 110000) (clamped 104685))
    ((px 90000) (clamped 95285))
    ((px 110000) (clamped 110000))
    |}]
;;
