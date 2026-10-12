open! Core
open! Exchange_core

type entry =
  { side : Side.t
  ; px : Price.t
  ; mutable ahead : Size.t
  ; mutable seen : int (** the last [refresh] that found the order resting *)
  }

type t =
  { entries : entry Order_id.Table.t
  ; mutable generation : int
  }

let create () = { entries = Order_id.Table.create (); generation = 0 }

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
        Hashtbl.set
          t.entries
          ~key:o.oid
          ~data:{ side = o.side; px = o.px; ahead = shown o.side o.px; seen = generation }))
  [@nontail];
  if !removed > 0 then Hashtbl.filter_inplace t.entries ~f:(fun e -> e.seen = generation)
;;

let cap t ~reference =
  Hashtbl.iter t.entries ~f:(fun e ->
    match reference e.side e.px with
    | Some size when Size.( < ) size e.ahead -> e.ahead <- size
    | Some _ | None -> ())
;;

let trade t ~passive ~px ~size =
  Hashtbl.iter t.entries ~f:(fun e ->
    if Side.equal e.side passive && Price.equal e.px px
    then e.ahead <- Size.( - ) e.ahead (Size.min e.ahead size))
;;

let find t oid = Hashtbl.find t.entries oid |> Option.map ~f:(fun e -> e.ahead)
let length t = Hashtbl.length t.entries
