open! Core
open! Exchange_core
open Price_anchor

let at s = Time_ns.of_string_with_utc_offset s
let px = Price.of_string
let sec = Time_ns.Span.of_sec

let show path elapsed =
  List.iter elapsed ~f:(fun e ->
    print_s
      [%message
        "" ~elapsed:(e : Time_ns.Span.t) ~price:(Path.price_at path ~elapsed:e : Price.t)])
;;

let%expect_test "a series is a step function from its first point" =
  let path =
    Path.series
      [ at "2026-01-02 14:30:10Z", px "101"
      ; at "2026-01-02 14:30:00Z", px "100"
      ; at "2026-01-02 14:30:30Z", px "102"
      ]
    |> ok_exn
  in
  show path [ sec 0.; sec 9.; sec 10.; sec 29.; sec 30.; sec 3600. ];
  [%expect
    {|
    ((elapsed 0s) (price 100))
    ((elapsed 9s) (price 100))
    ((elapsed 10s) (price 101))
    ((elapsed 29s) (price 101))
    ((elapsed 30s) (price 102))
    ((elapsed 1h) (price 102))
    |}]
;;

let%expect_test "a fixed price compounds its drift per day" =
  let path = Path.fixed_of_string "100,10" |> ok_exn in
  show path [ sec 0.; Time_ns.Span.day; Time_ns.Span.(scale day 2.) ];
  [%expect
    {|
    ((elapsed 0s) (price 100))
    ((elapsed 1d) (price 100.1))
    ((elapsed 2d) (price 100.2001))
    |}]
;;

let%expect_test "bad inputs" =
  let show r = print_s [%sexp (r : Path.t Or_error.t)] in
  show (Path.series []);
  show (Path.series [ at "2026-01-02 14:30:00Z", px "0" ]);
  show (Path.fixed_of_string "100,abc");
  show (Path.fixed_of_string "-1");
  show (Path.fixed_of_string "1,2,3");
  [%expect
    {|
    (Error "empty reference series")
    (Error ("reference price must be positive" (px 0)))
    (Error
     (("fixed reference" 100,abc) (Invalid_argument "Float.of_string abc")))
    (Error (("fixed reference" -1) ("reference price must be positive" (px -1))))
    (Error (("fixed reference" 1,2,3) "expected PRICE[,DRIFT_BPS_PER_DAY]"))
    |}]
;;
