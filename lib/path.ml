open! Core
open! Exchange_core

type t =
  | Series of (Time_ns.t * Price.t) array
  | Fixed of
      { price : Price.t
      ; drift_bps_per_day : float
      }
[@@deriving sexp_of]

let positive px =
  if Price.is_positive px
  then Ok ()
  else Or_error.error_s [%message "reference price must be positive" (px : Price.t)]
;;

let series points =
  let open Or_error.Let_syntax in
  let%bind () = Or_error.all_unit (List.map points ~f:(fun (_, px) -> positive px)) in
  let%bind () =
    (* Equal times are a step; an earlier time than the previous point's is refused. *)
    List.foldi points ~init:(Ok None) ~f:(fun index acc (time, _) ->
      let%bind prev = acc in
      match prev with
      | Some previous when Time_ns.( < ) time previous ->
        Or_error.error_s
          [%message
            "reference point goes back in time"
              (index : int)
              ~time:(time : Time_ns.Alternate_sexp.t)
              (previous : Time_ns.Alternate_sexp.t)]
      | _ -> Ok (Some time))
    |> Or_error.ignore_m
  in
  match points with
  | [] -> Or_error.error_string "empty reference series"
  | points -> Ok (Series (Array.of_list points))
;;

let fixed ?(drift_bps_per_day = 0.) price =
  let open Or_error.Let_syntax in
  let%bind () = positive price in
  if Float.is_finite drift_bps_per_day
  then Ok (Fixed { price; drift_bps_per_day })
  else Or_error.error_s [%message "drift must be finite" (drift_bps_per_day : float)]
;;

let fixed_of_string s =
  Or_error.try_with_join (fun () ->
    match String.split s ~on:',' with
    | [ px ] -> fixed (Price.of_string px)
    | [ px; drift ] ->
      fixed ~drift_bps_per_day:(Float.of_string drift) (Price.of_string px)
    | _ -> Or_error.error_string "expected PRICE[,DRIFT_BPS_PER_DAY]")
  |> Or_error.tag_s ~tag:[%message "fixed reference" s]
;;

let price_at t ~elapsed =
  match t with
  | Fixed { price; drift_bps_per_day } ->
    if Float.equal drift_bps_per_day 0.
    then price
    else (
      let days = Time_ns.Span.to_day elapsed in
      let factor = (1. +. (drift_bps_per_day /. 10_000.)) **. days in
      Price.of_units
        (Float.iround_nearest_exn (Float.of_int (Price.to_units price) *. factor)))
  | Series points ->
    let first, _ = points.(0) in
    let at = Time_ns.add first elapsed in
    (match
       Array.binary_search
         points
         ~compare:(fun (time, _) at -> Time_ns.compare time at)
         `Last_less_than_or_equal_to
         at
     with
     | Some i -> snd points.(i)
     | None -> snd points.(0))
;;
