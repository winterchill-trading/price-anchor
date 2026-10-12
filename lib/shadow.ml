open! Core
open! Exchange_core
open! Exchange_core_engine

module Level = struct
  type t =
    { side : Side.t
    ; px : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Order = struct
  type t =
    { order_id : Order_id.t
    ; level : Level.t
    }
  [@@deriving sexp_of]
end

module Plan = struct
  type t =
    { cancel : Order.t list
    ; place : Level.t list
    ; keep : Order.t list
    }
  [@@deriving sexp_of]
end

let plan ~desired ~resting =
  let unused = ref resting in
  let place, keep =
    List.partition_map desired ~f:(fun (level : Level.t) ->
      let rec take acc = function
        | [] -> None
        | (o : Order.t) :: rest when Level.equal o.level level ->
          Some (o, List.rev_append acc rest)
        | o :: rest -> take (o :: acc) rest
      in
      match take [] !unused with
      | Some (o, rest) ->
        unused := rest;
        Second o
      | None -> First level)
  in
  { Plan.cancel = !unused; place; keep }
;;

let full_levels book scale ~levels =
  let side_levels (side : Side.t) =
    let direction : Scale.Direction.t =
      match side with
      | Buy -> Down
      | Sell -> Up
    in
    Ref_book.top book side ~n:levels
    |> List.filter_map ~f:(fun (px, size) ->
      match Scale.price scale px ~direction with
      | None -> None
      | Some px -> Some (px, Scale.size scale size))
    (* Levels are best first and rounding keeps the order, so equal prices are adjacent. *)
    |> List.group ~break:(fun (a, _) (b, _) -> not (Price.equal a b))
    |> List.filter_map ~f:(fun group ->
      let px = fst (List.hd_exn group) in
      let size = Size.sum (List.map group ~f:snd) in
      if Size.is_positive size then Some { Level.side; px; size } else None)
  in
  side_levels Buy @ side_levels Sell
;;

module Stats = struct
  type t =
    { placed : int
    ; cancelled : int
    ; skipped : int
    ; consumed : int
    }
  [@@deriving sexp_of]
end

module Key = struct
  module T = struct
    type t = Side.t * Price.t [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
end

(* A shadow order the anchor placed: [full] is the level's size before consumption when it
   was placed, [known] its remaining size at the last [sync_fills]. *)
type tracked =
  { side : Side.t
  ; px : Price.t
  ; full : Size.t
  ; mutable known : Size.t
  }

(* Size consumed at a price while the full reference size there was [full]. *)
type consumed =
  { full : Size.t
  ; mutable size : Size.t
  }

type t =
  { exchange : Exchange.t
  ; account : Account.t
  ; symbol : Symbol.t
  ; scale : Scale.t
  ; levels : int
  ; orders : tracked Order_id.Table.t
  ; consumed : consumed Key.Table.t
  ; mutable placed : int
  ; mutable cancelled : int
  ; mutable skipped : int
  ; mutable fills : int
  }

let create exchange ~account ~symbol ~scale ~levels =
  { exchange
  ; account
  ; symbol
  ; scale
  ; levels
  ; orders = Order_id.Table.create ()
  ; consumed = Key.Table.create ()
  ; placed = 0
  ; cancelled = 0
  ; skipped = 0
  ; fills = 0
  }
;;

let stats t =
  { Stats.placed = t.placed
  ; cancelled = t.cancelled
  ; skipped = t.skipped
  ; consumed = t.fills
  }
;;

let sync_fills t =
  let remaining = Order_id.Table.create () in
  List.iter (Exchange.open_orders t.exchange t.account) ~f:(fun (o : Open_order.t) ->
    if Symbol.equal o.symbol t.symbol
    then Hashtbl.set remaining ~key:o.order_id ~data:o.size);
  Hashtbl.filteri_inplace t.orders ~f:(fun ~key:order_id ~data:(o : tracked) ->
    let now = Hashtbl.find remaining order_id |> Option.value ~default:Size.zero in
    let filled = Size.( - ) o.known now in
    if Size.is_positive filled
    then (
      t.fills <- t.fills + 1;
      let key = o.side, o.px in
      match Hashtbl.find t.consumed key with
      | Some c when Size.equal c.full o.full -> c.size <- Size.( + ) c.size filled
      | _ -> Hashtbl.set t.consumed ~key ~data:{ full = o.full; size = filled });
    o.known <- now;
    Size.is_positive now)
;;

let desired_of_full t full =
  List.filter_map full ~f:(fun (level : Level.t) ->
    let key = level.side, level.px in
    match Hashtbl.find t.consumed key with
    | Some c when Size.equal c.full level.size ->
      let size = Size.( - ) level.size c.size in
      if Size.is_positive size then Some { level with size } else None
    | Some _ ->
      (* The reference size changed: the level is shown in full again. *)
      Hashtbl.remove t.consumed key;
      Some level
    | None -> Some level)
;;

let desired t book = desired_of_full t (full_levels book t.scale ~levels:t.levels)

let orders t =
  Hashtbl.to_alist t.orders
  |> List.sort ~compare:(fun (a, _) (b, _) -> Order_id.compare a b)
  |> List.map ~f:(fun (order_id, (o : tracked)) ->
    { Order.order_id; level = { side = o.side; px = o.px; size = o.known } })
;;

let cancel t order_id =
  Hashtbl.remove t.orders order_id;
  match Exchange.cancel t.exchange t.account ~symbol:t.symbol order_id with
  | Cancelled -> t.cancelled <- t.cancelled + 1
  | Not_found -> ()
;;

let reconcile t book =
  sync_fills t;
  let full = full_levels book t.scale ~levels:t.levels in
  let desired = desired_of_full t full in
  let plan = plan ~desired ~resting:(orders t) in
  List.iter plan.cancel ~f:(fun o -> cancel t o.order_id);
  List.iter plan.place ~f:(fun (level : Level.t) ->
    let request =
      Order_request.limit ~tif:Alo t.symbol level.side ~px:level.px ~size:level.size
    in
    match Exchange.place t.exchange t.account request with
    | Resting { order_id; _ } ->
      t.placed <- t.placed + 1;
      let full =
        List.find full ~f:(fun (f : Level.t) ->
          Side.equal f.side level.side && Price.equal f.px level.px)
        |> Option.value_map ~default:level.size ~f:(fun (f : Level.t) -> f.size)
      in
      Hashtbl.set
        t.orders
        ~key:order_id
        ~data:{ side = level.side; px = level.px; full; known = level.size }
    | Filled _ | Rejected _ -> t.skipped <- t.skipped + 1)
;;

let cancel_where t ~side ~f =
  sync_fills t;
  List.iter (orders t) ~f:(fun o ->
    if Side.equal o.level.side side && f o.level.px then cancel t o.order_id)
;;

let cancel_all t =
  sync_fills t;
  List.iter (orders t) ~f:(fun o -> cancel t o.order_id);
  Hashtbl.clear t.consumed
;;

let shown t side px =
  Hashtbl.fold t.orders ~init:Size.zero ~f:(fun ~key:_ ~data:(o : tracked) acc ->
    if Side.equal o.side side && Price.equal o.px px then Size.( + ) acc o.known else acc)
;;
