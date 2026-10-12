open! Core
open! Exchange_core
open Price_anchor

let apply book = function
  | Market_data.Event.L2 u -> Ref_book.apply_l2 book u
  | Bbo b -> Ref_book.apply_bbo book b
  | Trade _ | Funding _ -> ()
;;

let show book =
  print_s
    [%message
      ""
        ~bids:(Ref_book.top book Buy ~n:3 : (Price.t * Size.t) list)
        ~asks:(Ref_book.top book Sell ~n:3 : (Price.t * Size.t) list)
        ~mid:(Ref_book.mid book : Price.t option)]
;;

let%expect_test "snapshot, delta, bbo" =
  let book = Ref_book.create () in
  show book;
  [%expect {| ((bids ()) (asks ()) (mid ())) |}];
  apply
    book
    (Helpers.l2
       ~at:0
       ~bids:[ "100", "1"; "99", "2"; "98", "3"; "97", "4" ]
       ~asks:[ "101", "1"; "102", "2" ]
       ());
  show book;
  [%expect
    {|
    ((bids ((100 1) (99 2) (98 3))) (asks ((101 1) (102 2))) (mid (100.5)))
    |}];
  apply
    book
    (Helpers.l2
       ~kind:Delta
       ~at:1
       ~bids:[ "100", "0"; "99.5", "7" ]
       ~asks:[ "102", "5" ]
       ());
  show book;
  [%expect
    {|
    ((bids ((99.5 7) (99 2) (98 3))) (asks ((101 1) (102 5))) (mid (100.25)))
    |}];
  print_s [%sexp (Ref_book.size_at book Sell (Price.of_string "102") : Size.t)];
  [%expect {| 5 |}];
  apply book (Helpers.bbo ~at:2 ~bid:("99", "3") ~ask:("100", "0"));
  show book;
  [%expect {| ((bids ((99 3))) (asks ()) (mid ())) |}]
;;
