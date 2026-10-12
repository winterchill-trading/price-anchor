open! Core
open! Async
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let btc = Harness.btc

let new_anchor () =
  Harness.create (fun ~exchange ~market_sim:_ ->
    Anchor.create ~exchange ~reference:Helpers.instrument ~sim_symbol:btc ())
;;

let print_item (item : Source.Item.t) =
  let ts =
    Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc (Source.Item.recv_ts item)
  in
  let what =
    match item with
    | Event (Trade t) ->
      sprintf "trade %s %s" (Price.to_string t.price) (Size.to_string t.size)
    | Event (Bbo b) ->
      sprintf "bbo %s %s" (Price.to_string b.bid_price) (Price.to_string b.ask_price)
    | Event (L2 u) -> sprintf "l2 %d+%d levels" (List.length u.bids) (List.length u.asks)
    | Event (Funding f) -> sprintf "funding %g" f.rate
    | Gap g -> sprintf !"gap %{sexp: Market_data.Gap.Reason.t}" g.reason
    | Price p -> sprintf "price %s" (Price.to_string p.price)
  in
  print_endline (ts ^ " " ^ what)
;;

let%expect_test "series lines" =
  List.iter
    [ "1760000000000000000 100.5"
    ; "2026-10-12T09:15:00Z 4200"
    ; "  2026-10-12T09:15:00.5+01:00   4200.25 "
    ; "yesterday 1"
    ; "1760000000000000000"
    ]
    ~f:(fun line ->
      match Source.parse_series_line line with
      | Ok (time, px) ->
        print_s
          [%message
            ""
              ~_:(Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc time : string)
              ~_:(px : Price.t)]
      | Error _ -> print_s [%message "error" (line : string)]);
  [%expect
    {|
    (2025-10-09T08:53:20.000000000Z 100.5)
    (2026-10-12T09:15:00.000000000Z 4200)
    (2026-10-12T08:15:00.500000000Z 4200.25)
    (error (line "yesterday 1"))
    (error (line 1760000000000000000))
    |}];
  return ()
;;

let with_dir name f =
  let%bind () = Unix.mkdir ~p:() name in
  let%bind result = f name in
  let%map () = Unix.system_exn (sprintf "rm -rf %s" name) in
  result
;;

let%expect_test "a series file drives the mark" =
  with_dir "series-test" (fun dir ->
    let path = dir ^/ "points.txt" in
    let%bind () =
      Writer.save
        path
        ~contents:
          "# time price\n\
           2025-10-09T08:53:20Z 100000\n\n\
           2025-10-09T08:53:21Z 100012.34\n\
           2025-10-09T08:53:25Z 99990\n"
    in
    let h = new_anchor () in
    let%bind source = Source.of_series path >>| ok_exn in
    let%map () =
      Source.run source ~anchor:h.anchor ~on_item:(fun item ->
        print_item item;
        print_s [%message "" ~mark:(Exchange.mark h.exchange btc : Price.t option)])
    in
    [%expect
      {|
      2025-10-09T08:53:20.000000000Z price 100000
      (mark (100000))
      2025-10-09T08:53:21.000000000Z price 100012.34
      (mark (100012.3))
      2025-10-09T08:53:25.000000000Z price 99990
      (mark (99990))
      |}])
;;

let%expect_test "a missing or malformed series file is an error, not an exception" =
  with_dir "series-errors" (fun dir ->
    let show ?(details = true) path =
      let%map result = Source.of_series path in
      let error =
        Result.error result
        |> Option.map ~f:(fun e ->
          if details
          then Error.sexp_of_t e
          else (
            (* Keep the tag only: the OS error text differs between systems. *)
            match Error.sexp_of_t e with
            | Sexp.List (tag :: _) -> tag
            | sexp -> sexp))
      in
      print_s [%message "" ~ok:(Result.is_ok result : bool) (error : Sexp.t option)]
    in
    let%bind () = show ~details:false (dir ^/ "missing.txt") in
    let bad = dir ^/ "bad.txt" in
    let%bind () =
      Writer.save bad ~contents:"2025-10-09T08:53:20Z 100000\nnot a point\n"
    in
    let%map () = show bad in
    [%expect
      {|
      ((ok false) (error (("series file" (path series-errors/missing.txt)))))
      ((ok false)
       (error
        ((("series file" (path series-errors/bad.txt))
          (("series line" (line "not a point")) "expected <time> <price>")))))
      |}])
;;

let%expect_test "speed paces recorded time on the time source" =
  let time_source = Time_source.create ~now:Helpers.epoch () in
  let h = new_anchor () in
  let source =
    Pipe.of_list
      (List.map [ 0; 1_000; 3_000 ] ~f:(fun at ->
         Source.Item.Price { recv_ts = Helpers.ms at; price = Price.of_string "100" }))
  in
  let finished =
    Source.run
      ~speed:10.
      ~time_source:(Time_source.read_only time_source)
      source
      ~anchor:h.anchor
      ~on_item:(fun _ ->
        print_s
          [%message
            "applied"
              ~after:
                (Time_ns.diff (Time_source.now time_source) Helpers.epoch
                 : Time_ns.Span.t)])
  in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  let%bind () =
    Time_source.advance_by_alarms_by time_source (Time_ns.Span.of_int_ms 400)
  in
  let%bind () = finished in
  [%expect
    {|
    (applied (after 0s))
    (applied (after 100ms))
    (applied (after 300ms))
    |}];
  return ()
;;
