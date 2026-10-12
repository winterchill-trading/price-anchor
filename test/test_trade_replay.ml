open! Core
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let btc = Harness.btc
let client = Account.of_string "client"
let px = Price.of_string
let sz = Size.of_string

let setup () =
  let h =
    Harness.create (fun ~exchange ~market_sim:_ ->
      Anchor.create ~levels:2 ~exchange ~reference:Helpers.instrument ~sim_symbol:btc ())
  in
  Exchange.deposit h.exchange client (Usd.of_int 1_000_000);
  Anchor.apply
    h.anchor
    (Helpers.l2
       ~at:0
       ~bids:[ "100000", "2"; "99999", "3" ]
       ~asks:[ "100001", "1"; "100002", "2" ]
       ());
  h
;;

let rest (h : Harness.t) side ~at ~size =
  match
    Exchange.place
      h.exchange
      client
      (Order_request.limit btc side ~px:(px at) ~size:(sz size))
  with
  | Resting { order_id; _ } ->
    Anchor.step h.anchor ~now:(Exchange.now h.exchange);
    order_id
  | s -> raise_s [%sexp (s : Order_status.t)]
;;

let show (h : Harness.t) id =
  let fills =
    Exchange.fills h.exchange client
    |> List.rev_map ~f:(fun (e : Execution.t) ->
      sprintf "%s @ %s" (Size.to_string e.fill.sz) (Price.to_string e.fill.px))
  in
  let l2 = Option.value_exn (Exchange.l2 ~n_levels:3 h.exchange btc) in
  let side levels =
    List.map levels ~f:(fun (l : L2.Level.t) ->
      sprintf "%s x %s" (Price.to_string l.px) (Size.to_string l.sz))
  in
  let stats = Anchor.stats h.anchor in
  print_s
    [%message
      ""
        ~client_fills:(fills : string list)
        ~queue_ahead:(Anchor.queue_ahead h.anchor id : Size.t option)
        ~bids:(side l2.bids : string list)
        ~asks:(side l2.asks : string list)
        ~replays:(stats.replays : int)]
;;

let%expect_test "an order through the trade price fills in full" =
  let h = setup () in
  let id = rest h Buy ~at:"100000.5" ~size:"0.3" in
  Anchor.apply h.anchor (Helpers.trade ~at:10 Sell ~px:"100000" ~sz:"0.1");
  show h id;
  [%expect
    {|
    ((client_fills ("0.3 @ 100000.5")) (queue_ahead (0))
     (bids ("100000 x 2" "99999 x 3")) (asks ("100001 x 1" "100002 x 2"))
     (replays 1))
    |}]
;;

let%expect_test "an order at the trade price fills only beyond its queue" =
  let h = setup () in
  let id = rest h Buy ~at:"100000" ~size:"1" in
  show h id;
  [%expect
    {|
    ((client_fills ()) (queue_ahead (2)) (bids ("100000 x 3" "99999 x 3"))
     (asks ("100001 x 1" "100002 x 2")) (replays 0))
    |}];
  Anchor.apply h.anchor (Helpers.trade ~at:10 Sell ~px:"100000" ~sz:"1.5");
  show h id;
  [%expect
    {|
    ((client_fills ()) (queue_ahead (0.5)) (bids ("100000 x 3" "99999 x 3"))
     (asks ("100001 x 1" "100002 x 2")) (replays 0))
    |}];
  Anchor.apply h.anchor (Helpers.trade ~at:20 Sell ~px:"100000" ~sz:"1");
  show h id;
  [%expect
    {|
    ((client_fills ("0.5 @ 100000")) (queue_ahead (0))
     (bids ("100000 x 2.5" "99999 x 3")) (asks ("100001 x 1" "100002 x 2"))
     (replays 1))
    |}]
;;

let%expect_test "asks fill on buy trades; orders behind the trade price do not" =
  let h = setup () in
  let through = rest h Sell ~at:"100000.8" ~size:"0.2" in
  let behind = rest h Sell ~at:"100002" ~size:"0.2" in
  Anchor.apply h.anchor (Helpers.trade ~at:10 Buy ~px:"100001" ~sz:"0.5");
  show h through;
  show h behind;
  [%expect
    {|
    ((client_fills ("0.2 @ 100000.8")) (queue_ahead (0))
     (bids ("100000 x 2" "99999 x 3")) (asks ("100001 x 1" "100002 x 2.2"))
     (replays 1))
    ((client_fills ("0.2 @ 100000.8")) (queue_ahead (2))
     (bids ("100000 x 2" "99999 x 3")) (asks ("100001 x 1" "100002 x 2.2"))
     (replays 1))
    |}]
;;
