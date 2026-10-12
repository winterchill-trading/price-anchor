open! Core
open! Async
open! Exchange_core
open Price_anchor

let px = Price.of_string

let%expect_test "a recorded source becomes a path of book mids, else trades" =
  let items : Source.Item.t list =
    [ Event (Helpers.trade ~at:0 Buy ~px:"100000" ~sz:"1")
    ; Event (Helpers.bbo ~at:50 ~bid:("99990", "1") ~ask:("100010", "1"))
    ; Event (Helpers.bbo ~at:100 ~bid:("99990", "1") ~ask:("100010", "2"))
    ; Event (Helpers.bbo ~at:120 ~bid:("100010", "1") ~ask:("100030", "1"))
    ; Event (Helpers.bbo ~at:250 ~bid:("100030", "1") ~ask:("100050", "1"))
    ; Price { recv_ts = Helpers.ms 400; price = px "100100" }
    ]
  in
  let run ?min_interval () =
    let%map path = Source.to_path ?min_interval (Pipe.of_list items) in
    match path with
    | Error e -> print_s [%sexp (e : Error.t)]
    | Ok (Fixed _) -> print_endline "fixed"
    | Ok (Series points) ->
      Array.iter points ~f:(fun (ts, px) ->
        printf
          "%s %s\n"
          (Time_ns.to_string_iso8601_basic ~zone:Time_float.Zone.utc ts)
          (Price.to_string px));
      print_endline "--"
  in
  let%bind () = run () in
  let%map () = run ~min_interval:(Time_ns.Span.of_int_ms 200) () in
  [%expect
    {|
    2025-10-09T08:53:20.000000000Z 100000
    2025-10-09T08:53:20.120000000Z 100020
    2025-10-09T08:53:20.250000000Z 100040
    2025-10-09T08:53:20.400000000Z 100100
    --
    2025-10-09T08:53:20.000000000Z 100000
    2025-10-09T08:53:20.250000000Z 100040
    --
    |}]
;;

let%expect_test "an empty source has no path" =
  let%map path = Source.to_path (Pipe.of_list []) in
  print_s [%sexp (path : Path.t Or_error.t)];
  [%expect {| (Error ("no reference price in the source" "empty reference series")) |}]
;;
