open! Core
open! Exchange_core
open Price_anchor

let btc =
  Instrument.create
    (Symbol.of_string "BTC")
    ~size_decimals:5
    ~price_tick:(Price.of_string "0.1")
    ~max_leverage:40
;;

let hl_like =
  Instrument.create
    ~max_significant_figures:5
    (Symbol.of_string "SOL")
    ~size_decimals:2
    ~price_tick:(Price.of_string "0.0001")
    ~max_leverage:20
;;

let show scale px =
  let px = Price.of_string px in
  List.iter
    Scale.Direction.[ Down; Up; Nearest ]
    ~f:(fun direction ->
      print_s
        [%message
          ""
            ~_:(direction : Scale.Direction.t)
            ~_:(Scale.price scale px ~direction : Price.t option)])
;;

let%expect_test "tick rounding by direction" =
  let scale = Scale.create btc in
  show scale "100000.15";
  [%expect {|
    (Down (100000.1))
    (Up (100000.2))
    (Nearest (100000.2))
    |}];
  show scale "100000.2";
  [%expect {|
    (Down (100000.2))
    (Up (100000.2))
    (Nearest (100000.2))
    |}];
  show scale "0.04";
  [%expect {|
    (Down ())
    (Up (0.1))
    (Nearest ())
    |}]
;;

let%expect_test "significant figures" =
  let scale = Scale.create hl_like in
  show scale "151.23456";
  [%expect {|
    (Down (151.23))
    (Up (151.24))
    (Nearest (151.23))
    |}];
  show scale "123456.7";
  [%expect {|
    (Down (123456))
    (Up (123457))
    (Nearest (123457))
    |}]
;;

let%expect_test "factors" =
  (* A reference quoted per 1000 tokens against a simulator quoting per token. *)
  let scale = Scale.create ~price_factor:0.001 ~size_factor:1000. btc in
  show scale "12345.6";
  [%expect {|
    (Down (12.3))
    (Up (12.4))
    (Nearest (12.3))
    |}];
  print_s [%sexp (Scale.size scale (Size.of_string "1.234567891") : Size.t)];
  [%expect {| 1234.56789 |}];
  print_s [%sexp (Scale.size (Scale.create btc) (Size.of_string "0.000009") : Size.t)];
  [%expect {| 0 |}]
;;

let%expect_test "rounded bids never above, asks never below the scaled price" =
  let scale = Scale.create ~price_factor:1.37 hl_like in
  Quickcheck.test
    (Int.gen_incl 1 (Price.to_units (Price.of_string "1000000")))
    ~f:(fun units ->
      let px = Price.of_units units in
      let exact = Float.of_int units *. 1.37 in
      (match Scale.price scale px ~direction:Down with
       | None -> ()
       | Some p ->
         assert (Instrument.valid_price hl_like p);
         assert (Float.( <= ) (Float.of_int (Price.to_units p)) (exact +. 1.)));
      match Scale.price scale px ~direction:Up with
      | None -> assert false
      | Some p ->
        assert (Instrument.valid_price hl_like p);
        assert (Float.( >= ) (Float.of_int (Price.to_units p)) (exact -. 1.)))
;;
