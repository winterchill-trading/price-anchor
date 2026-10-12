(* Runs a recorded reference through an anchor into a fresh simulated exchange, prints the
   anchor's counters, and checks that the exchange's journal replays to the same state. *)

open! Core
open! Async
open! Exchange_core
open! Exchange_core_engine
open Price_anchor

let source_param =
  let%map_open.Command norm =
    flag "-norm" (optional string) ~doc:"DIR event log directory (market-data norm/)"
  and series = flag "-series" (optional string) ~doc:"FILE reference series file"
  and from = flag "-from" (optional date) ~doc:"DATE first UTC day (with -norm)"
  and to_ = flag "-to" (optional date) ~doc:"DATE last UTC day (with -norm)" in
  fun ~reference ->
    match norm, series, from, to_ with
    | Some dir, None, Some from, Some to_ -> Source.of_norm ~dir reference ~from ~to_
    | None, Some path, None, None -> Source.of_series path
    | _ ->
      return
        (Or_error.error_s
           [%message "give either -norm DIR -from DATE -to DATE, or -series FILE"])
;;

let command =
  Command.async_or_error
    ~summary:"Replay a recorded reference market into an anchored simulated exchange"
    (let%map_open.Command source = source_param
     and reference =
       flag
         "-reference"
         (required string)
         ~doc:"VENUE:SYMBOL reference instrument, e.g. binance:BTCUSDT"
     and symbol =
       flag "-symbol" (optional_with_default "BTC" string) ~doc:"SYM simulated symbol"
     and price_factor =
       flag "-price-factor" (optional float) ~doc:"F simulated price per reference price"
     and size_factor =
       flag "-size-factor" (optional float) ~doc:"F simulated size per reference size"
     and levels = flag "-levels" (optional int) ~doc:"N shadow levels per side"
     and speed =
       flag
         "-anchor-speed"
         (optional_with_default 0. float)
         ~doc:"X pace recorded time (1 is real time, 0 as fast as possible)"
     and journal =
       flag "-journal" (optional string) ~doc:"FILE write the exchange journal"
     and verbose = flag "-verbose" no_arg ~doc:" print the anchor's log lines" in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let reference = Market_data.Instrument.of_string reference in
       let%bind source = source ~reference in
       let config = Exchange.Config.default in
       let exchange = Exchange.create ~config () in
       let inputs = Queue.create () in
       Exchange.record_inputs exchange ~f:(Queue.enqueue inputs);
       let journal_writer = Option.map journal ~f:Journal.create in
       Option.iter journal_writer ~f:(fun j -> Journal.attach j exchange);
       let anchor =
         Anchor.create
           ?price_factor
           ?size_factor
           ?levels
           ~log:(if verbose then fun s -> Core.print_s s else ignore)
           ~exchange
           ~reference
           ~sim_symbol:(Symbol.of_string symbol)
           ()
       in
       let%map () =
         Source.run ~speed source ~anchor ~on_item:(fun _ ->
           Anchor.step anchor ~now:(Exchange.now exchange))
         |> Deferred.ok
       in
       Option.iter journal_writer ~f:Journal.close;
       let replayed = Journal.replay ~config (Queue.to_list inputs) in
       let sym = Symbol.of_string symbol in
       Core.print_s
         [%message
           ""
             ~stats:(Anchor.stats anchor : Anchor.Stats.t)
             ~inputs:(Queue.length inputs : int)
             ~mark:(Exchange.mark exchange sym : Price.t option)
             ~replay_matches:
               ([%equal: Price.t option]
                  (Exchange.mark exchange sym)
                  (Exchange.mark replayed sym)
                && [%equal: Sexp.t]
                     [%sexp (Exchange.l2 exchange sym : L2.t option)]
                     [%sexp (Exchange.l2 replayed sym : L2.t option)]
                : bool)])
;;

let () =
  Command_unix.run (Command.group ~summary:"Price anchor tools" [ "replay", command ])
;;
