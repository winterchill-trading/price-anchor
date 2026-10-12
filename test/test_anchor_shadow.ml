open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let btc = Harness.btc
let client = Account.of_string "client"

let setup () =
  let h =
    Harness.create (fun ~exchange ~market_sim:_ ->
      Anchor.create ~levels:3 ~exchange ~reference:Helpers.instrument ~sim_symbol:btc ())
  in
  Exchange.deposit h.exchange client (Usd.of_int 1_000_000);
  h
;;

let show_book (h : Harness.t) =
  let l2 = Option.value_exn (Exchange.l2 ~n_levels:4 h.exchange btc) in
  let side levels =
    List.map levels ~f:(fun (l : L2.Level.t) ->
      sprintf "%s x %s" (Price.to_string l.px) (Size.to_string l.sz))
  in
  print_s
    [%message "" ~bids:(side l2.bids : string list) ~asks:(side l2.asks : string list)]
;;

let place (h : Harness.t) request =
  print_s [%sexp (Exchange.place h.exchange client request : Order_status.t)]
;;

let snapshot =
  Helpers.l2
    ~at:0
    ~bids:[ "100000", "2"; "99999.5", "1"; "99999", "3"; "99998", "9" ]
    ~asks:[ "100001", "1"; "100001.5", "2"; "100002", "3" ]
    ()
;;

let%expect_test "top levels rest in the simulated book; consumed size stays consumed" =
  let h = setup () in
  Anchor.apply h.anchor snapshot;
  show_book h;
  [%expect
    {|
    ((bids ("100000 x 2" "99999.5 x 1" "99999 x 3"))
     (asks ("100001 x 1" "100001.5 x 2" "100002 x 3")))
    |}];
  place
    h
    (Order_request.limit
       ~tif:Ioc
       btc
       Buy
       ~px:(Price.of_string "100001")
       ~size:(Size.of_string "0.4"));
  [%expect {| (Filled (order_id 7) (total 0.4) (avg_px 100001) (client_order_id ())) |}];
  (* Another level changes: the consumed 0.4 is not shown again. *)
  Anchor.apply
    h.anchor
    (Helpers.l2 ~kind:Delta ~at:10 ~bids:[ "99999.5", "1.5" ] ~asks:[] ());
  show_book h;
  [%expect
    {|
    ((bids ("100000 x 2" "99999.5 x 1.5" "99999 x 3"))
     (asks ("100001 x 0.6" "100001.5 x 2" "100002 x 3")))
    |}];
  (* The reference size at that price changes: shown in full again. *)
  Anchor.apply
    h.anchor
    (Helpers.l2 ~kind:Delta ~at:20 ~bids:[] ~asks:[ "100001", "1.1" ] ());
  show_book h;
  [%expect
    {|
    ((bids ("100000 x 2" "99999.5 x 1.5" "99999 x 3"))
     (asks ("100001 x 1.1" "100001.5 x 2" "100002 x 3")))
    |}];
  print_s [%sexp ((Anchor.stats h.anchor).shadow : Shadow.Stats.t)];
  [%expect {| ((placed 8) (cancelled 2) (skipped 0) (consumed 1)) |}]
;;

let%expect_test "a shadow level that would cross a simulated order is skipped" =
  let h = setup () in
  Anchor.apply h.anchor snapshot;
  place
    h
    (Order_request.limit
       ~tif:Alo
       btc
       Sell
       ~px:(Price.of_string "100000.5")
       ~size:(Size.of_string "0.1"));
  [%expect {| (Resting (order_id 7) (client_order_id ())) |}];
  (* The reference bid moves above the client's ask. *)
  Anchor.apply
    h.anchor
    (Helpers.l2 ~kind:Delta ~at:10 ~bids:[ "100000.8", "1" ] ~asks:[] ());
  show_book h;
  [%expect
    {|
    ((bids ("100000 x 2" "99999.5 x 1"))
     (asks ("100000.5 x 0.1" "100001 x 1" "100001.5 x 2" "100002 x 3")))
    |}];
  print_s [%sexp ((Anchor.stats h.anchor).shadow : Shadow.Stats.t)];
  [%expect {| ((placed 6) (cancelled 1) (skipped 1) (consumed 0)) |}]
;;

let%expect_test "queue_ahead: behind the shown size, capped when the reference drops" =
  let h = setup () in
  Anchor.apply h.anchor snapshot;
  let id =
    match
      Exchange.place
        h.exchange
        client
        (Order_request.limit
           btc
           Buy
           ~px:(Price.of_string "100000")
           ~size:(Size.of_string "0.5"))
    with
    | Resting { order_id; _ } -> order_id
    | s -> raise_s [%sexp (s : Order_status.t)]
  in
  Anchor.step h.anchor ~now:(Exchange.now h.exchange);
  print_s [%sexp (Anchor.queue_ahead h.anchor id : Size.t option)];
  [%expect {| (2) |}];
  Anchor.apply
    h.anchor
    (Helpers.l2 ~kind:Delta ~at:10 ~bids:[ "100000", "1.5" ] ~asks:[] ());
  print_s [%sexp (Anchor.queue_ahead h.anchor id : Size.t option)];
  [%expect {| (1.5) |}];
  Anchor.apply
    h.anchor
    (Helpers.l2 ~kind:Delta ~at:20 ~bids:[ "100000", "4" ] ~asks:[] ());
  print_s [%sexp (Anchor.queue_ahead h.anchor id : Size.t option)];
  [%expect {| (1.5) |}];
  Anchor.apply
    h.anchor
    (Helpers.l2 ~kind:Delta ~at:30 ~bids:[ "100000", "0" ] ~asks:[] ());
  print_s [%sexp (Anchor.queue_ahead h.anchor id : Size.t option)];
  [%expect {| (0) |}]
;;
