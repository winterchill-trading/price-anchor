open! Core
open! Exchange_core

(* Entries are indexed by order id and, per side, by price: each price holds an intrusive
   doubly linked list of its entries, so a reference trade or a cap at one price touches
   only that price's orders, and forgetting an order is O(1). *)
type entry =
  { oid : Order_id.t
  ; side : Side.t
  ; px : Price.t
  ; mutable ahead : Size.t
  ; mutable seen : int (** the last [refresh] that found the order resting *)
  ; level : level
  ; mutable prev : entry or_null
  ; mutable next : entry or_null
  }

and level =
  { level_px : Price.t
  ; mutable head : entry or_null
  ; mutable count : int
  }

type t =
  { entries : entry Order_id.Table.t
  ; bids : level Int.Table.t (** keyed by [Price.to_units] *)
  ; asks : level Int.Table.t
  ; mutable generation : int
  }

let create () =
  { entries = Order_id.Table.create ()
  ; bids = Int.Table.create ()
  ; asks = Int.Table.create ()
  ; generation = 0
  }
;;

let levels t (side : Side.t) =
  match side with
  | Buy -> t.bids
  | Sell -> t.asks
;;

let add t ~oid ~side ~px ~ahead ~seen =
  let level =
    Hashtbl.find_or_add (levels t side) (Price.to_units px) ~default:(fun () ->
      { level_px = px; head = Null; count = 0 })
  in
  let entry = { oid; side; px; ahead; seen; level; prev = Null; next = level.head } in
  (match level.head with
   | Null -> ()
   | This first -> first.prev <- This entry);
  level.head <- This entry;
  level.count <- level.count + 1;
  Hashtbl.set t.entries ~key:oid ~data:entry
;;

let remove t entry =
  let level = entry.level in
  (match entry.prev with
   | Null -> level.head <- entry.next
   | This prev -> prev.next <- entry.next);
  (match entry.next with
   | Null -> ()
   | This next -> next.prev <- entry.prev);
  level.count <- level.count - 1;
  if level.count = 0 then Hashtbl.remove (levels t entry.side) (Price.to_units entry.px);
  Hashtbl.remove t.entries entry.oid
;;

let rec iter_level (e : entry or_null) ~f =
  match e with
  | Null -> ()
  | This e ->
    let next = e.next in
    f e;
    iter_level next ~f
;;

let refresh t book ~exclude ~shown =
  t.generation <- t.generation + 1;
  let generation = t.generation in
  let removed = ref (Hashtbl.length t.entries) in
  Order_book.iter book ~f:(fun (o : Order_book.Resting_order.t) ->
    if not (exclude o.owner)
    then (
      match Hashtbl.find t.entries o.oid with
      | Some e ->
        e.seen <- generation;
        decr removed
      | None ->
        add t ~oid:o.oid ~side:o.side ~px:o.px ~ahead:(shown o.side o.px) ~seen:generation))
  [@nontail];
  if !removed > 0
  then (
    let gone =
      Hashtbl.fold t.entries ~init:[] ~f:(fun ~key:_ ~data:e acc ->
        if e.seen = generation then acc else e :: acc)
    in
    List.iter gone ~f:(remove t))
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

let find t oid = Hashtbl.find t.entries oid |> Option.map ~f:(fun e -> e.ahead)
let length t = Hashtbl.length t.entries
