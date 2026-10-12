(** Builders for reference market data events in tests. Times are milliseconds after
    [epoch]. *)

open! Core
open! Exchange_core

val epoch : Time_ns.t
val ms : int -> Time_ns.t
val instrument : Market_data.Instrument.t

val l2
  :  ?kind:Market_data.L2_update.Kind.t
  -> at:int
  -> bids:(string * string) list
  -> asks:(string * string) list
  -> unit
  -> Market_data.Event.t

val bbo : at:int -> bid:string * string -> ask:string * string -> Market_data.Event.t
val trade : at:int -> Side.t -> px:string -> sz:string -> Market_data.Event.t
val funding : at:int -> rate:float -> mark:string -> Market_data.Event.t
