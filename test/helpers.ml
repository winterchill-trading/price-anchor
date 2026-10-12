open! Core
open! Exchange_core

let epoch = Time_ns.of_int_ns_since_epoch 1_760_000_000_000_000_000
let ms n = Time_ns.add epoch (Time_ns.Span.of_int_ms n)
let instrument = Market_data.Instrument.create Binance "BTCUSDT"

let levels l =
  List.map l ~f:(fun (px, sz) ->
    { Market_data.L2_update.Level.price = Price.of_string px; size = Size.of_string sz })
;;

let l2 ?(kind = Market_data.L2_update.Kind.Snapshot) ~at ~bids ~asks () =
  Market_data.Event.L2
    { instrument
    ; exch_ts = ms at
    ; recv_ts = ms at
    ; kind
    ; bids = levels bids
    ; asks = levels asks
    }
;;

let bbo ~at ~bid:(bid_price, bid_size) ~ask:(ask_price, ask_size) =
  Market_data.Event.Bbo
    { instrument
    ; exch_ts = ms at
    ; recv_ts = ms at
    ; bid_price = Price.of_string bid_price
    ; bid_size = Size.of_string bid_size
    ; ask_price = Price.of_string ask_price
    ; ask_size = Size.of_string ask_size
    }
;;

let trade_seq = ref 0

let trade ~at side ~px ~sz =
  incr trade_seq;
  Market_data.Event.Trade
    { instrument
    ; exch_ts = ms at
    ; recv_ts = ms at
    ; price = Price.of_string px
    ; size = Size.of_string sz
    ; side
    ; trade_id = Int.to_string !trade_seq
    }
;;

let funding ~at ~rate ~mark =
  Market_data.Event.Funding
    { instrument
    ; exch_ts = ms at
    ; recv_ts = ms at
    ; rate
    ; next_funding_time = ms (at + 3_600_000)
    ; mark_price = Price.of_string mark
    ; index_price = Price.of_string mark
    }
;;
