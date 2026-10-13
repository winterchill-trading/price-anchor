open! Core
open! Exchange_core

(* Entries are indexed by order id and, per side, by price: each price holds an intrusive
   doubly linked list of its entries in order id order, so a reference trade or a cap at
   one price touches only that price's orders, and forgetting an order is O(1).

   The exchange numbers orders in the order it accepts them, and a book level keeps its
   orders in time priority, so a level's orders come in increasing id order. [refresh]
   walks the book against these lists with a cursor: a resting order is either the
   cursor's entry, new (an id above every id seen before), or an order that was excluded
   when it was new. Only the last case, and the rare order out of id order, needs a table
   lookup. *)
type entry =
  { key : int (** the order id as an int *)
  ; side : Side.t
  ; px : Price.t
  ; mutable ahead : Size.t
  ; mutable seen : int (** the last [refresh] that found the order resting *)
  ; level : level
  ; mutable prev : entry or_null
  ; mutable next : entry or_null
  }

and level =
  { level_side : Side.t
  ; level_px : Price.t
  ; mutable head : entry or_null
  ; mutable tail : entry or_null
  ; mutable count : int
  ; mutable level_seen : int (** the last [refresh] that found an order at this price *)
  }

type t =
  { entries : entry Int.Table.t (** keyed by [key_of] the order id *)
  ; bids : level Int.Table.t (** keyed by [Price.to_units] *)
  ; asks : level Int.Table.t
  ; mutable generation : int
  ; mutable max_seen : int (** the highest order id any [refresh] visited *)
  ; mutable current : level or_null (** the level [refresh] is walking *)
  ; mutable cursor : entry or_null (** the next entry of [current] not yet matched *)
  }

let create () =
  { entries = Int.Table.create ()
  ; bids = Int.Table.create ()
  ; asks = Int.Table.create ()
  ; generation = 0
  ; max_seen = 0
  ; current = Null
  ; cursor = Null
  }
;;

(* Order ids are small positive integers the exchange counts up from one. *)
let key_of oid = Int64.to_int_trunc (Order_id.to_int64 oid)

let levels t (side : Side.t) =
  match side with
  | Buy -> t.bids
  | Sell -> t.asks
;;

let add t ~key ~side ~px ~ahead ~seen =
  let level =
    Hashtbl.find_or_add (levels t side) (Price.to_units px) ~default:(fun () ->
      { level_side = side
      ; level_px = px
      ; head = Null
      ; tail = Null
      ; count = 0
      ; level_seen = seen
      })
  in
  let entry = { key; side; px; ahead; seen; level; prev = level.tail; next = Null } in
  (match level.tail with
   | Null -> level.head <- This entry
   | This last -> last.next <- This entry);
  level.tail <- This entry;
  level.count <- level.count + 1;
  Hashtbl.set t.entries ~key ~data:entry;
  level
;;

let remove t entry =
  let level = entry.level in
  (match entry.prev with
   | Null -> level.head <- entry.next
   | This prev -> prev.next <- entry.next);
  (match entry.next with
   | Null -> level.tail <- entry.prev
   | This next -> next.prev <- entry.prev);
  level.count <- level.count - 1;
  if level.count = 0 then Hashtbl.remove (levels t entry.side) (Price.to_units entry.px);
  Hashtbl.remove t.entries entry.key
;;

let rec iter_level (e : entry or_null) ~f =
  match e with
  | Null -> ()
  | This e ->
    let next = e.next in
    f e;
    iter_level next ~f
;;

(* Forgets the entries of [level] the current [refresh] did not find. *)
let sweep_level t level =
  let generation = t.generation in
  iter_level level.head ~f:(fun e -> if e.seen <> generation then remove t e)
;;

let finish_current t =
  (match t.current with
   | Null -> ()
   | This level -> sweep_level t level);
  t.current <- Null;
  t.cursor <- Null
;;

(* Makes the level of [side] at [px] current, if it is not already. *)
let enter_level t (side : Side.t) px =
  let same =
    match t.current with
    | This level -> Side.equal level.level_side side && Price.equal level.level_px px
    | Null -> false
  in
  if not same
  then (
    finish_current t;
    let levels = levels t side in
    let key = Price.to_units px in
    if Hashtbl.mem levels key
    then (
      let level = Hashtbl.find_exn levels key in
      level.level_seen <- t.generation;
      t.current <- This level;
      t.cursor <- level.head))
;;

let rec skip_to t key =
  match t.cursor with
  | This e when e.key < key ->
    t.cursor <- e.next;
    skip_to t key
  | This _ | Null -> ()
;;

let visit t (o : Order_book.Resting_order.t) ~exclude ~shown =
  let generation = t.generation in
  let key = key_of o.oid in
  enter_level t o.side o.px;
  skip_to t key;
  match t.cursor with
  | This e when e.key = key ->
    e.seen <- generation;
    t.cursor <- e.next
  | This _ | Null ->
    if key > t.max_seen
    then (
      if not (exclude o.owner)
      then (
        let level =
          add t ~key ~side:o.side ~px:o.px ~ahead:(shown o.side o.px) ~seen:generation
        in
        (* The level may have just been created. *)
        match t.current with
        | Null -> t.current <- This level
        | This _ -> ()))
    else if Hashtbl.mem t.entries key
    then (Hashtbl.find_exn t.entries key).seen <- generation
;;

let refresh t book ~exclude ~shown =
  t.generation <- t.generation + 1;
  let generation = t.generation in
  let max_seen = ref t.max_seen in
  Order_book.iter book ~f:(fun (o : Order_book.Resting_order.t) ->
    visit t o ~exclude ~shown;
    let key = key_of o.oid in
    if key > !max_seen then max_seen := key)
  [@nontail];
  finish_current t;
  t.max_seen <- !max_seen;
  let stale = ref [] in
  let collect level = if level.level_seen <> generation then stale := level :: !stale in
  Hashtbl.iter t.bids ~f:collect;
  Hashtbl.iter t.asks ~f:collect;
  List.iter !stale ~f:(fun level -> iter_level level.head ~f:(remove t))
;;

let cap t ~reference =
  let cap_side side =
    Hashtbl.iter (levels t side) ~f:(fun level ->
      match reference side level.level_px with
      | None -> ()
      | Some size ->
        iter_level level.head ~f:(fun e -> if Size.( < ) size e.ahead then e.ahead <- size))
  in
  cap_side Buy;
  cap_side Sell
;;

let trade t ~passive ~px ~size =
  match Hashtbl.find (levels t passive) (Price.to_units px) with
  | None -> ()
  | Some level ->
    iter_level level.head ~f:(fun e -> e.ahead <- Size.( - ) e.ahead (Size.min e.ahead size))
;;

let find t oid = Hashtbl.find t.entries (key_of oid) |> Option.map ~f:(fun e -> e.ahead)
let length t = Hashtbl.length t.entries
