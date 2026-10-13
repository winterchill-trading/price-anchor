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

let%test_unit "mid_or_null is the mid of the best levels after any sequence of updates" =
  let level = [%quickcheck.generator: int * int] in
  let update =
    [%quickcheck.generator:
      bool
      * [%custom List.gen_with_length 3 level]
      * [%custom List.gen_with_length 3 level]]
  in
  Quickcheck.test
    (List.quickcheck_generator update)
    ~sexp_of:[%sexp_of: (bool * (int * int) list * (int * int) list) list]
    ~f:(fun updates ->
      let book = Ref_book.create () in
      let levels l =
        List.map l ~f:(fun (px, size) ->
          { Market_data.L2_update.Level.price = Price.of_int (1 + (abs px % 20))
          ; size = Size.of_int (abs size % 3)
          })
      in
      List.iter updates ~f:(fun (snapshot, bids, asks) ->
        Ref_book.apply_l2
          book
          { instrument = Helpers.instrument
          ; exch_ts = Helpers.epoch
          ; recv_ts = Helpers.epoch
          ; kind = (if snapshot then Snapshot else Delta)
          ; bids = levels bids
          ; asks = levels asks
          };
        let expect =
          match Ref_book.best book Buy, Ref_book.best book Sell with
          | Some (bid, _), Some (ask, _) ->
            Some (Price.of_units ((Price.to_units bid + Price.to_units ask) / 2))
          | _ -> None
        in
        let or_null =
          match Ref_book.mid_or_null book with
          | Null -> None
          | This px -> Some px
        in
        [%test_result: Price.t option] or_null ~expect))
;;
