open! Core
open! Exchange_core
open Price_anchor

let owners = [| Account.of_string "anchor"; Account.of_string "a"; Account.of_string "b" |]

module Order = struct
  type t =
    { owner : int
    ; buy : bool
    ; px : int
    ; size : int
    ; cancel : bool
    }
  [@@deriving sexp_of, quickcheck]
end

module Case = struct
  type t =
    { orders : Order.t list
    ; buy_aggressor : bool
    ; px : int
    ; q : int
    }
  [@@deriving sexp_of, quickcheck]
end

let%test_unit "one walk matches the filtered, sorted list" =
  Quickcheck.test Case.quickcheck_generator ~sexp_of:Case.sexp_of_t ~trials:2000 ~f:(fun c ->
    let book = Order_book.create Harness.btc in
    (* Bids 1..5, asks 6..10: nothing crosses. Some orders are cancelled again so levels
       have holes in their queues. *)
    List.iteri c.orders ~f:(fun i (o : Order.t) ->
      let oid = Order_id.of_int (i + 1) in
      ignore
        (Order_book.add_limit
           book
           ~oid
           ~owner:owners.(abs o.owner % 3)
           ~side:(if o.buy then Buy else Sell)
           ~px:(Price.of_int (if o.buy then 1 + (abs o.px % 5) else 6 + (abs o.px % 5)))
           ~sz:(Size.of_int (1 + (abs o.size % 4)))
           ~tif:Gtc
           ~time:(i / 3)
           ~next_tid:(fun () -> Trade_id.of_int 0)
           ()
         : Order_book.Result.t);
      if o.cancel
      then ignore (Order_book.cancel book oid : Order_book.Resting_order.t option));
    let passive : Side.t = if c.buy_aggressor then Sell else Buy in
    let p = Price.of_int (abs c.px % 11) in
    let q = Size.of_int (abs c.q % 12) in
    let exclude = Account.equal owners.(0) in
    let ahead (o : Order_book.Resting_order.t) =
      Size.of_int (Order_id.to_int_exn o.oid % 3)
    in
    [%test_result: Size.t]
      (Eligible.size book ~passive ~p ~q ~exclude ~ahead)
      ~expect:(Naive_eligible.size book ~passive ~p ~q ~exclude ~ahead))
;;
