open! Core
open! Async
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

(* A toy venue whose frames are ["book <bid> <bid size> <ask> <ask size>"] (a one-level
   snapshot), ["trade <px> <size> <B|S>"] and ["ack"]. *)
module Toy_normalizer : Market_data.Normalizer.S = struct
  type t = { mutable frames : int }

  let venue = Market_data.Venue.Binance
  let create () = { frames = 0 }
  let instrument = Helpers.instrument

  let frame t ~recv_ts payload =
    t.frames <- t.frames + 1;
    let level px size =
      { Market_data.L2_update.Level.price = Price.of_string px
      ; size = Size.of_string size
      }
    in
    match String.split payload ~on:' ' with
    | [ "ack" ] -> Ok []
    | [ "book"; bid; bid_size; ask; ask_size ] ->
      Ok
        [ Market_data.Event.L2
            { instrument
            ; exch_ts = recv_ts
            ; recv_ts
            ; kind = Snapshot
            ; bids = [ level bid bid_size ]
            ; asks = [ level ask ask_size ]
            }
        ]
    | [ "trade"; px; size; side ] ->
      Ok
        [ Market_data.Event.Trade
            { instrument
            ; exch_ts = recv_ts
            ; recv_ts
            ; price = Price.of_string px
            ; size = Size.of_string size
            ; side = (if String.equal side "B" then Buy else Sell)
            ; trade_id = Int.to_string t.frames
            }
        ]
    | _ -> Or_error.error_s [%message "toy: bad frame" payload]
  ;;

  let disconnect t = t.frames <- 0
end

let with_dir name f =
  let%bind () = Unix.system_exn (sprintf "rm -rf %s" name) in
  let%bind () = Unix.mkdir ~p:() name in
  let%bind result = f name in
  let%map () = Unix.system_exn (sprintf "rm -rf %s" name) in
  result
;;

let print_item (item : Source.Item.t) =
  let ts =
    Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc (Source.Item.recv_ts item)
  in
  let what =
    match item with
    | Event e ->
      Sexp.to_string [%sexp (Market_data.Event.stream e : Market_data.Stream.t)]
    | Gap g -> Sexp.to_string [%sexp (g.reason : Market_data.Gap.Reason.t)]
    | Price _ -> "price"
  in
  print_endline (ts ^ " " ^ what)
;;

let write_minute dir ~stream lines =
  let minute_dir = dir ^/ "raw/binance" ^/ stream ^/ "BTCUSDT/2025/10/09" in
  let%bind () = Unix.mkdir ~p:() minute_dir in
  Writer.save
    (minute_dir ^/ "08-53.ndjson")
    ~contents:(String.concat lines ~sep:"\n" ^ "\n")
;;

let%expect_test "raw minute files through a normalizer, with a disconnect" =
  with_dir "raw-source-test" (fun dir ->
    let%bind () =
      write_minute
        dir
        ~stream:"l2"
        [ "2025-10-09T08:53:20.000000Z ack"
        ; "2025-10-09T08:53:20.100000Z book 100000 2 100001 1"
        ; ""
        ; "2025-10-09T08:53:22.000000Z book 100002 2 100003 1"
        ; "2025-10-09T08:53:22.500000Z garbage"
        ]
    in
    let%bind () =
      write_minute
        dir
        ~stream:"trades"
        [ "2025-10-09T08:53:20.200000Z trade 100001 0.5 B"
        ; "2025-10-09T08:53:22.100000Z trade 100002 0.1 S"
        ]
    in
    let h =
      Harness.create (fun ~exchange ~market_sim:_ ->
        Anchor.create ~exchange ~reference:Helpers.instrument ~sim_symbol:Harness.btc ())
    in
    let source =
      Source.of_raw
        ~on_error:(fun e -> print_s [%message "on_error" ~_:(Error.sexp_of_t e : Sexp.t)])
        ~dir
        ~symbol:"BTCUSDT"
        ~from:(Time_ns.of_string_with_utc_offset "2025-10-09T08:53:00Z")
        ~to_:(Time_ns.of_string_with_utc_offset "2025-10-09T08:54:00Z")
        ~normalizer:(module Toy_normalizer)
        ()
    in
    let%map () = Source.run source ~anchor:h.anchor ~on_item:print_item in
    print_s [%sexp (Anchor.stats h.anchor : Anchor.Stats.t)];
    [%expect
      {|
      2025-10-09T08:53:20.100000000Z L2
      2025-10-09T08:53:20.100000000Z Disconnect
      2025-10-09T08:53:20.200000000Z Trades
      2025-10-09T08:53:22.000000000Z L2
      2025-10-09T08:53:22.100000000Z Trades
      (on_error
       (((stream L2) (recv_ts "2025-10-09 08:53:22.5Z"))
        ("toy: bad frame" garbage)))
      ((stale ()) (went_stale 2) (stale_ns 1900000000) (gaps 1) (events 4)
       (ignored 0) (marks_set 2) (funding_rates_set 0)
       (shadow ((placed 4) (cancelled 2) (skipped 0) (consumed 0))) (replays 0)
       (replays_rejected 0) (waits 0) (resumes 0) (clamped 0))
      |}])
;;

let%expect_test "event logs, merged in recv_ts order" =
  with_dir "norm-source-test" (fun dir ->
    let writer = Market_data.Event_log.Writer.create ~dir () in
    List.iter
      [ Helpers.l2 ~at:0 ~bids:[ "100000", "1" ] ~asks:[ "100001", "1" ] ()
      ; Helpers.trade ~at:10 Buy ~px:"100001" ~sz:"0.1"
      ; Helpers.funding ~at:15 ~rate:0.0001 ~mark:"100000.5"
      ; Helpers.l2 ~kind:Delta ~at:20 ~bids:[ "100000", "2" ] ~asks:[] ()
      ]
      ~f:(Market_data.Event_log.Writer.append writer);
    let%bind () = Market_data.Event_log.Writer.close writer in
    let date = Date.of_string "2025-10-09" in
    let%bind source =
      Source.of_norm ~dir Helpers.instrument ~from:date ~to_:date >>| ok_exn
    in
    let%bind () = Pipe.iter_without_pushback source ~f:print_item in
    let%map missing =
      Source.of_norm
        ~dir
        Helpers.instrument
        ~from:(Date.add_days date 1)
        ~to_:(Date.add_days date 2)
    in
    print_s [%sexp (Result.is_error missing : bool)];
    [%expect
      {|
      2025-10-09T08:53:20.000000000Z L2
      2025-10-09T08:53:20.010000000Z Trades
      2025-10-09T08:53:20.015000000Z Funding
      2025-10-09T08:53:20.020000000Z L2
      true
      |}])
;;
