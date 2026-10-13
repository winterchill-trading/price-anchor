open! Core
open! Exchange_core
open Price_anchor

module Op = struct
  type t =
    | Add of
        { owner : int
        ; buy : bool
        ; px : int
        ; size : int
        }
    | Cancel of int
    | Refresh
    | Cap of int
    | Trade of
        { buy : bool
        ; px : int
        ; size : int
        }
  [@@deriving sexp_of, quickcheck]
end

let owners = [| Account.of_string "anchor"; Account.of_string "a"; Account.of_string "b" |]

(* Small grids so ops hit the same prices often. Bids rest at 1..5 and asks at 6..10, so
   no order crosses. *)
let price ~buy px = Price.of_int (if buy then 1 + (abs px % 5) else 6 + (abs px % 5))
let size n = Size.of_int (1 + (abs n % 4))
let shown side px = Size.of_int (((Price.to_units px / Price.units_per_one) + if Side.equal side Buy then 1 else 2) % 4)

(* Reference sizes depend on the price and the op's seed; some prices are not covered. *)
let reference seed side px =
  let k = ((Price.to_units px / Price.units_per_one) * 7) + seed + if Side.equal side Buy then 0 else 3 in
  if k % 5 = 0 then None else Some (Size.of_int (abs k % 3))
;;

let%test_unit "indexed queue_ahead matches the full walk" =
  Quickcheck.test
    (List.quickcheck_generator Op.quickcheck_generator)
    ~sexp_of:[%sexp_of: Op.t list]
    ~trials:500
    ~f:(fun ops ->
      let book = Order_book.create Harness.btc in
      let next_oid = ref 1 in
      let fast = Queue_ahead.create () in
      let naive = Naive_queue_ahead.create () in
      let exclude = Account.equal owners.(0) in
      let check () =
        for oid = 1 to !next_oid do
          let oid = Order_id.of_int oid in
          [%test_result: Size.t option]
            (Queue_ahead.find fast oid)
            ~expect:(Naive_queue_ahead.find naive oid)
        done;
        [%test_result: int] (Queue_ahead.length fast) ~expect:(Naive_queue_ahead.length naive)
      in
      List.iter ops ~f:(fun (op : Op.t) ->
        (match op with
         | Add { owner; buy; px; size = n } ->
           let oid = Order_id.of_int !next_oid in
           incr next_oid;
           ignore
             (Order_book.add_limit
                book
                ~oid
                ~owner:owners.(abs owner % 3)
                ~side:(if buy then Buy else Sell)
                ~px:(price ~buy px)
                ~sz:(size n)
                ~tif:Gtc
                ~time:!next_oid
                ~next_tid:(fun () -> Trade_id.of_int 0)
                ()
              : Order_book.Result.t)
         | Cancel n ->
           ignore
             (Order_book.cancel book (Order_id.of_int (1 + (abs n % !next_oid)))
              : Order_book.Resting_order.t option)
         | Refresh ->
           Queue_ahead.refresh fast book ~exclude ~shown;
           Naive_queue_ahead.refresh naive book ~exclude ~shown
         | Cap seed ->
           Queue_ahead.cap fast ~reference:(reference seed);
           Naive_queue_ahead.cap naive ~reference:(reference seed)
         | Trade { buy; px; size = n } ->
           let passive : Side.t = if buy then Buy else Sell in
           let px = price ~buy px in
           Queue_ahead.trade fast ~passive ~px ~size:(size n);
           Naive_queue_ahead.trade naive ~passive ~px ~size:(size n));
        check ()))
;;
