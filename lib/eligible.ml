open! Core
open! Exchange_core

(* [px] is at or through [p] for a resting order of [passive] side. *)
let at_or_through ~(passive : Side.t) ~p px =
  match passive with
  | Buy -> Price.( >= ) px p
  | Sell -> Price.( <= ) px p
;;

let size book ~(passive : Side.t) ~p ~q ~exclude ~ahead =
  let best =
    match passive with
    | Buy -> Order_book.best_bid_or_null book
    | Sell -> Order_book.best_ask_or_null book
  in
  match best with
  | Null -> Size.zero
  | This best when not (at_or_through ~passive ~p best) -> Size.zero
  | This _ ->
    (* [iter] visits a level in time priority, which is the oldest-first order the
       orders at [p] share [q] in. *)
    let total = ref Size.zero in
    let used_at_p = ref Size.zero in
    Order_book.iter book ~f:(fun (o : Order_book.Resting_order.t) ->
      if Side.equal o.side passive
         && at_or_through ~passive ~p o.px
         && not (exclude o.owner)
      then
        if not (Price.equal o.px p)
        then total := Size.( + ) !total o.remaining
        else (
          let left = Size.( - ) (Size.( - ) q (ahead o)) !used_at_p in
          let e = Size.min o.remaining (Size.max Size.zero left) in
          used_at_p := Size.( + ) !used_at_p e;
          total := Size.( + ) !total e))
    [@nontail];
    !total
;;
