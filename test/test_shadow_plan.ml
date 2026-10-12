open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let level side px size =
  { Shadow.Level.side; px = Price.of_string px; size = Size.of_string size }
;;

let order id side px size =
  { Shadow.Order.order_id = Order_id.of_int id; level = level side px size }
;;

let%expect_test "plan keeps exact matches, cancels the rest, places the missing" =
  let desired =
    [ level Buy "100" "1"
    ; level Buy "99" "2"
    ; level Sell "101" "1"
    ; level Sell "102" "3"
    ]
  in
  let resting =
    [ order 1 Buy "100" "1"
    ; order 2 Buy "99" "1.5" (* partially consumed elsewhere: wrong size *)
    ; order 3 Sell "101" "1"
    ; order 4 Sell "105" "1"
    ; order 5 Buy "100" "1" (* duplicate *)
    ]
  in
  print_s [%sexp (Shadow.plan ~desired ~resting : Shadow.Plan.t)];
  [%expect
    {|
    ((cancel
      (((order_id 2) (level ((side Buy) (px 99) (size 1.5))))
       ((order_id 4) (level ((side Sell) (px 105) (size 1))))
       ((order_id 5) (level ((side Buy) (px 100) (size 1))))))
     (place (((side Buy) (px 99) (size 2)) ((side Sell) (px 102) (size 3))))
     (keep
      (((order_id 1) (level ((side Buy) (px 100) (size 1))))
       ((order_id 3) (level ((side Sell) (px 101) (size 1)))))))
    |}]
;;

let%expect_test "full levels: scaled, merged on equal prices, zero sizes dropped" =
  let book = Ref_book.create () in
  (match
     Helpers.l2
       ~at:0
       ~bids:[ "100.04", "1"; "100.01", "2"; "99.9", "0.000001"; "99.5", "3" ]
       ~asks:[ "100.06", "1"; "100.09", "1"; "100.3", "2" ]
       ()
   with
   | L2 u -> Ref_book.apply_l2 book u
   | _ -> assert false);
  let instrument =
    Instrument.create
      (Symbol.of_string "BTC")
      ~size_decimals:3
      ~price_tick:(Price.of_string "0.1")
      ~max_leverage:20
  in
  let scale = Scale.create instrument in
  print_s [%sexp (Shadow.full_levels book scale ~levels:3 : Shadow.Level.t list)];
  [%expect
    {|
    (((side Buy) (px 100) (size 3)) ((side Sell) (px 100.1) (size 2))
     ((side Sell) (px 100.3) (size 2)))
    |}]
;;

let%expect_test "plan converges: replanning after applying a plan changes nothing" =
  let gen_level =
    let open Quickcheck.Let_syntax in
    let%map side = Quickcheck.Generator.of_list Side.all
    and px = Int.gen_incl 1 6
    and size = Int.gen_incl 1 3 in
    level side (Int.to_string px) (Int.to_string size)
  in
  Quickcheck.test
    (Quickcheck.Generator.both
       (List.gen_non_empty gen_level)
       (List.gen_non_empty gen_level))
    ~f:(fun (desired, resting_levels) ->
      let resting =
        List.mapi resting_levels ~f:(fun i level ->
          { Shadow.Order.order_id = Order_id.of_int i; level })
      in
      let p = Shadow.plan ~desired ~resting in
      let after =
        p.keep
        @ List.mapi p.place ~f:(fun i level ->
          { Shadow.Order.order_id = Order_id.of_int (1000 + i); level })
      in
      let p2 = Shadow.plan ~desired ~resting:after in
      assert (List.is_empty p2.cancel && List.is_empty p2.place);
      assert (
        List.length p.cancel + List.length p.keep = List.length resting
        && List.length p.place + List.length p.keep = List.length desired))
;;
