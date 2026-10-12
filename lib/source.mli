(** Where an anchor's reference data comes from, as one pipe of items in [recv_ts] order,
    and the driver that applies them to an {!Anchor.t}.

    - {!of_feed}: a live {!Market_data.Feed.t} (a client's [Live_feed] or a [*-feed]
      repository), items in arrival order.
    - {!of_raw}: the recorder's raw minute files through a venue's
      {!Market_data.Normalizer.S}, the system of record; disconnects become gaps.
    - {!of_norm}: normalized {!Market_data.Event_log} files.
    - {!of_series}: a file of [(time, price)] points, for venues with no data source. *)

open! Core
open! Async

module Item : sig
  type t =
    | Event of Market_data.Event.t
    | Gap of Market_data.Gap.t
    | Price of
        { recv_ts : Time_ns.t
        ; price : Market_data.Price.t
        } (** a reference series point *)
  [@@deriving sexp_of]

  val recv_ts : t -> Time_ns.t
end

type t = Item.t Pipe.Reader.t

(** Every event of the feed's pipes, in the order they arrive, and its gaps. Closes once
    every pipe has closed. *)
val of_feed : Market_data.Feed.t -> t

val of_feed_with_gaps : Market_data.Feed.With_gaps.t -> t

(** The raw minute files of [symbol] on [normalizer]'s venue under [dir] (the recorder's
    data directory, containing [raw/]) with [recv_ts] in [[from, to_)], all streams merged
    by [recv_ts] ({!Market_data.Raw_reader.read}) and normalized with one state per
    stream. An empty raw line resets that stream's state and becomes a
    {!Market_data.Gap.Reason.Disconnect} gap. Frames the normalizer rejects go to
    [on_error] (default: logged) and are skipped. *)
val of_raw
  :  ?on_error:(Error.t -> unit)
  -> dir:string
  -> symbol:string
  -> from:Time_ns.t
  -> to_:Time_ns.t
  -> normalizer:Market_data.Normalizer.t
  -> unit
  -> t

(** The {!Market_data.Event_log} files of [instrument] (trades, bbo, l2, funding) for the
    UTC days [[from, to_]] under [dir], merged by [recv_ts]. Unlike
    {!Market_data.Replay_feed.of_range}, which splits events into one pipe per stream, the
    items keep their exact [recv_ts] order. Fails when no file exists. *)
val of_norm
  :  dir:string
  -> Market_data.Instrument.t
  -> from:Date.t
  -> to_:Date.t
  -> t Or_error.t Deferred.t

(** Parses one series line: [<time> <price>], the time either nanoseconds since the epoch
    or an ISO 8601 time with a zone ([2026-10-12T09:15:00Z]). *)
val parse_series_line : string -> (Time_ns.t * Market_data.Price.t) Or_error.t

(** A series file: one {!parse_series_line} point per line, in time order; blank lines and
    lines starting with [#] are skipped. Returns [Error] for a file that cannot be read or
    has a bad line, or a point whose time is before the previous point's (equal times are
    allowed), naming the line; never raises. *)
val of_series : string -> t Or_error.t Deferred.t

(** Reads the whole source into a {!Path.t} for a venue that pulls its reference on its
    own clock: series points as they are; for events, the mid of the reference book (L2,
    or BBO), else the last trade price. A gap drops the book. A point is kept when the
    price changed and at least [min_interval] (default zero) of [recv_ts] passed since the
    last kept one. Fails when the source has no price. *)
val to_path : ?min_interval:Time_ns.Span.t -> t -> Path.t Or_error.t Deferred.t

(** Applies every item to [anchor] ([Anchor.apply], [Anchor.gap], [Anchor.apply_price]),
    calling [on_item] after each. With [speed > 0.] recorded time is paced against
    [time_source] (default the wall clock): an item [d] after the first is applied
    [d / speed] after the first one, so [1.] is real time. With [speed = 0.] (the default,
    and the choice for live sources) items are applied as they come. Determined when the
    source closes. *)
val run
  :  ?speed:float
  -> ?time_source:Time_source.t
  -> ?on_item:(Item.t -> unit)
  -> t
  -> anchor:Anchor.t
  -> unit Deferred.t
