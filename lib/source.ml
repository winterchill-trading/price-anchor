open! Core
module Md_stream = Market_data.Stream
open! Async

module Item = struct
  type t =
    | Event of Market_data.Event.t
    | Gap of Market_data.Gap.t
    | Price of
        { recv_ts : Time_ns.t
        ; price : Market_data.Price.t
        }
  [@@deriving sexp_of]

  let recv_ts = function
    | Event e -> Market_data.Event.recv_ts e
    | Gap g -> g.recv_ts
    | Price p -> p.recv_ts
  ;;
end

type t = Item.t Pipe.Reader.t

let of_feed_with_gaps ((module F) : Market_data.Feed.With_gaps.t) =
  let event f pipe = Pipe.map pipe ~f:(fun x -> Item.Event (f x)) in
  Pipe.interleave
    [ event (fun x -> Market_data.Event.Trade x) F.trades
    ; event (fun x -> Market_data.Event.Bbo x) F.bbo
    ; event (fun x -> Market_data.Event.L2 x) F.l2
    ; event (fun x -> Market_data.Event.Funding x) F.funding
    ; Pipe.map F.gaps ~f:(fun g -> Item.Gap g)
    ]
;;

let of_feed feed = of_feed_with_gaps (Market_data.Feed.With_gaps.of_feed feed)

let of_raw
  ?(on_error = fun e -> Log.Global.error_s (Error.sexp_of_t e))
  ~dir
  ~symbol
  ~from
  ~to_
  ~normalizer:(module N : Market_data.Normalizer.S)
  ()
  =
  let instrument = Market_data.Instrument.create N.venue symbol in
  let states = Md_stream.Table.create () in
  let state stream = Hashtbl.find_or_add states stream ~default:N.create in
  Market_data.Raw_reader.read
    ~on_error
    ~data_dir:dir
    ~venue:N.venue
    ~symbol
    ~start:from
    ~stop:to_
    ()
  |> Pipe.concat_map_list ~f:(function
    | Market_data.Raw_reader.Item.Frame { recv_ts; stream; frame } ->
      (match N.frame (state stream) ~recv_ts frame with
       | Ok events -> List.map events ~f:(fun e -> Item.Event e)
       | Error e ->
         on_error
           (Error.tag_s
              e
              ~tag:
                [%message "" (stream : Md_stream.t) (recv_ts : Time_ns.Alternate_sexp.t)]);
         [])
    | Gap { recv_ts; stream } ->
      N.disconnect (state stream);
      [ Item.Gap
          { recv_ts
          ; venue = N.venue
          ; instrument = Some instrument
          ; stream = Some stream
          ; reason = Disconnect
          }
      ])
;;

let of_norm ~dir instrument ~from ~to_ =
  let paths =
    List.concat_map (Date.dates_between ~min:from ~max:to_) ~f:(fun date ->
      List.map [ Md_stream.Trades; Bbo; L2; Funding ] ~f:(fun stream ->
        Market_data.Event_log.path ~dir instrument stream date))
  in
  let%map existing =
    Deferred.List.filter paths ~how:`Sequential ~f:(fun path ->
      match%map Sys.file_exists path with
      | `Yes -> true
      | `No | `Unknown -> false)
  in
  match existing with
  | [] ->
    Or_error.error_s
      [%message
        "no event log files"
          (dir : string)
          ~instrument:(Market_data.Instrument.to_string instrument)
          (from : Date.t)
          (to_ : Date.t)]
  | files ->
    Ok (Pipe.map (Market_data.Event_log.read_merged files) ~f:(fun e -> Item.Event e))
;;

let parse_series_line line =
  Or_error.try_with_join (fun () ->
    match
      String.split (String.strip line) ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    with
    | [ time; price ] ->
      let time =
        match Int.of_string_opt time with
        | Some ns -> Time_ns.of_int_ns_since_epoch ns
        | None -> Time_ns.of_string_with_utc_offset time
      in
      Ok (time, Market_data.Price.of_string price)
    | _ -> Or_error.error_s [%message "expected <time> <price>"])
  |> Or_error.tag_s ~tag:[%message "series line" (line : string)]
;;

let of_series path =
  let%map lines = Reader.file_lines path in
  List.filter lines ~f:(fun line ->
    let line = String.strip line in
    not (String.is_empty line || String.is_prefix line ~prefix:"#"))
  |> List.map ~f:parse_series_line
  |> Or_error.combine_errors
  |> Or_error.tag_s ~tag:[%message "series file" (path : string)]
  |> Or_error.map ~f:(fun points ->
    Pipe.of_list
      (List.map points ~f:(fun (recv_ts, price) -> Item.Price { recv_ts; price })))
;;

let apply anchor : Item.t -> unit = function
  | Event e -> Anchor.apply anchor e
  | Gap g -> Anchor.gap anchor g
  | Price { recv_ts; price } -> Anchor.apply_price anchor ~recv_ts price
;;

let run ?(speed = 0.) ?time_source ?(on_item = ignore) source ~anchor =
  let time_source =
    match time_source with
    | Some ts -> ts
    | None -> Time_source.wall_clock ()
  in
  let start = ref None in
  let wait item =
    if Float.( <= ) speed 0.
    then return ()
    else (
      let ts = Item.recv_ts item in
      let first, began =
        match !start with
        | Some s -> s
        | None ->
          let s = ts, Time_source.now time_source in
          start := Some s;
          s
      in
      let delay = Time_ns.Span.scale (Time_ns.diff ts first) (1. /. speed) in
      Time_source.at time_source (Time_ns.add began delay))
  in
  Pipe.iter source ~f:(fun item ->
    let%map () = wait item in
    apply anchor item;
    on_item item)
;;
