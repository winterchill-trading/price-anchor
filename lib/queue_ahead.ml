open! Core
open! Exchange_core

type entry =
  { side : Side.t
  ; px : Price.t
  ; mutable ahead : Size.t
  }

type t = entry Order_id.Table.t

let create () = Order_id.Table.create ()

let refresh t book ~exclude ~shown =
  let seen = Order_id.Hash_set.create () in
  List.iter (Order_book.orders book) ~f:(fun (o : Order_book.Resting_order.t) ->
    if not (exclude o.owner)
    then (
      Hash_set.add seen o.oid;
      if not (Hashtbl.mem t o.oid)
      then
        Hashtbl.set
          t
          ~key:o.oid
          ~data:{ side = o.side; px = o.px; ahead = shown o.side o.px }));
  Hashtbl.filter_keys_inplace t ~f:(Hash_set.mem seen)
;;

let cap t ~reference =
  Hashtbl.iter t ~f:(fun e ->
    match reference e.side e.px with
    | Some size when Size.( < ) size e.ahead -> e.ahead <- size
    | Some _ | None -> ())
;;

let trade t ~passive ~px ~size =
  Hashtbl.iter t ~f:(fun e ->
    if Side.equal e.side passive && Price.equal e.px px
    then e.ahead <- Size.( - ) e.ahead (Size.min e.ahead size))
;;

let find t oid = Hashtbl.find t oid |> Option.map ~f:(fun e -> e.ahead)
let length = Hashtbl.length
