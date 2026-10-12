open! Core
open! Exchange_core
open! Exchange_core_engine

type t =
  { exchange : Exchange.t
  ; market_sim : Market_sim.t option
  ; anchor : Price_anchor.Anchor.t
  ; inputs : Input.t Queue.t
  ; events : Event.t Queue.t
  }

let btc = Symbol.of_string "BTC"
let config = { Exchange.Config.default with min_order_value = Usd.zero }
let tick_ns = 100_000_000

let create ?(config = config) ?sim_config make_anchor =
  let exchange = Exchange.create ~config () in
  let inputs = Queue.create () in
  Exchange.record_inputs exchange ~f:(Queue.enqueue inputs);
  let market_sim = Option.map sim_config ~f:(fun c -> Market_sim.create c exchange) in
  let anchor = make_anchor ~exchange ~market_sim in
  { exchange; market_sim; anchor; inputs; events = Queue.create () }
;;

let run ?(extra_ticks = 0) ?(after_tick = ignore) t events =
  let recv e = Time_ns.to_int_ns_since_epoch (Market_data.Event.recv_ts e) in
  match events with
  | [] -> ()
  | first :: _ ->
    (* Events land at the anchor's simulated time once its offset is fixed. *)
    let start =
      match Price_anchor.Anchor.offset t.anchor with
      | Some offset -> -offset
      | None -> recv first - Exchange.now t.exchange
    in
    let last = recv (List.last_exn events) - start in
    let pending = ref events in
    let ticks = ((last - Exchange.now t.exchange) / tick_ns) + 1 + extra_ticks in
    for k = 1 to ticks do
      let now = Exchange.now t.exchange + tick_ns in
      let rec feed () =
        match !pending with
        | e :: rest when recv e - start <= now ->
          Price_anchor.Anchor.apply t.anchor e;
          pending := rest;
          feed ()
        | _ -> ()
      in
      feed ();
      ignore k;
      Option.iter t.market_sim ~f:Market_sim.tick;
      let now = Int.max now (Exchange.now t.exchange) in
      Exchange.step t.exchange ~now;
      Price_anchor.Anchor.step t.anchor ~now;
      Queue.enqueue_all t.events (Exchange.drain_events t.exchange);
      after_tick now
    done
;;

let check_replay ?(config = config) t =
  Queue.enqueue_all t.events (Exchange.drain_events t.exchange);
  let replayed = Journal.replay ~config (Queue.to_list t.inputs) in
  let theirs = Exchange.drain_events replayed in
  let sexps l = List.map l ~f:Event.sexp_of_t in
  let ours = Queue.to_list t.events in
  let same_events = [%equal: Sexp.t list] (sexps ours) (sexps theirs) in
  let same_mark =
    [%equal: Price.t option] (Exchange.mark t.exchange btc) (Exchange.mark replayed btc)
  in
  print_s
    [%message
      "replay"
        ~inputs:(Queue.length t.inputs : int)
        ~events:(List.length ours : int)
        (same_events : bool)
        (same_mark : bool)]
;;
