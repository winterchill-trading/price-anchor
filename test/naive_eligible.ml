open! Core
open! Exchange_core

let at_or_through ~(passive : Side.t) ~p px =
  match passive with
  | Buy -> Price.( >= ) px p
  | Sell -> Price.( <= ) px p
;;

let size book ~passive ~p ~q ~exclude ~ahead =
  let orders =
    Order_book.orders book
    |> List.filter ~f:(fun (o : Order_book.Resting_order.t) ->
      Side.equal o.side passive
      && (not (exclude o.owner))
      && at_or_through ~passive ~p o.px)
    |> List.sort ~compare:(fun (a : Order_book.Resting_order.t) b ->
      Comparable.lexicographic
        [ (fun (a : Order_book.Resting_order.t) b -> Int.compare a.timestamp b.timestamp)
        ; (fun a b -> Order_id.compare a.oid b.oid)
        ]
        a
        b)
  in
  let used_at_p = ref Size.zero in
  List.fold orders ~init:Size.zero ~f:(fun acc (o : Order_book.Resting_order.t) ->
    if not (Price.equal o.px p)
    then Size.( + ) acc o.remaining
    else (
      let left = Size.( - ) (Size.( - ) q (ahead o)) !used_at_p in
      let e = Size.min o.remaining (Size.max Size.zero left) in
      used_at_p := Size.( + ) !used_at_p e;
      Size.( + ) acc e))
;;
