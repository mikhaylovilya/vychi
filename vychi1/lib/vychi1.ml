(* #12 *)

(* type expr =
  | Const of int
  | Plus of expr * expr
  | Slash of expr * expr
  | Asterisk of expr * expr
  | Var of string

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    (* A synonym for >>= *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type MONADERROR = sig
  include MONAD

  val fail : string -> 'a t
end

module Eval (M : MONADERROR) = struct
  open M
  open M.Syntax

  let eval from_env : expr -> int M.t =
    let rec helper = function
      | Const n -> return n
      | Plus (l, r) ->
        let* l = helper l in
        let* r = helper r in
        return (l + r)
      | Asterisk (l, r) ->
        (* helper l >>= fun l -> *)
        let* l = helper l in
        let* r = helper r in
        return (l * r)
      | Slash (l, r) ->
        let* r = helper r in
        if r = 0
        then fail "division by zero"
        else
          let* l = helper l in
          return (l / r)
      | Var s -> from_env s
    in
    helper
  ;;
end *)

open Base

module Eval = struct
  type f = float -> float

  type interval =
    { left_b : float
    ; right_b : float
    }

  type answer =
    { x : float
    ; delta : float
    }

  type conf =
    { f : float -> float
    ; interval : interval
    ; split_count : int
    ; epsilon : float
    }

  let get_middle interval = (interval.right_b -. interval.left_b) /. 2.

  let root_separation f interval split_count =
    let h = (interval.right_b -. interval.left_b) /. Int.to_float split_count in
    let rec loop f interval h acc =
      match interval.left_b +. h with
      | new_lb when Float.( <= ) new_lb interval.right_b ->
        let new_acc =
          if Float.( <= ) (f interval.left_b *. f new_lb) 0.
          then { interval with right_b = new_lb } :: acc
          else acc
        in
        loop f { interval with left_b = new_lb } h new_acc
      | _ -> List.rev acc
    in
    loop f interval h []
  ;;

  let bisect f interval epsilon =
    (* let halve_interval interval =
      let middle = get_middle interval in
      { interval with right_b = middle }, { interval with left_b = middle }
    in *)
    let rec loop f interval epsilon =
      let middle = get_middle interval in
      let halved_interval =
        if Float.( <= ) (f interval.left_b *. f middle) 0.
        then { interval with right_b = middle }
        else { interval with left_b = middle }
      in
      match halved_interval with
      | { left_b; right_b } when Float.( < ) (right_b -. left_b) (2. *. epsilon) ->
        { x = (left_b +. right_b) /. 2.; delta = (right_b -. left_b) /. 2. }
      | _ -> loop f halved_interval epsilon
    in
    loop f interval epsilon
  ;;

  let bisect_method conf =
    let partition = root_separation conf.f conf.interval conf.split_count in
    partition |> List.map ~f:(fun int -> bisect conf.f int conf.epsilon)
  ;;
end

module V_Pprint = struct
  let log_interv_list debug (interval_list : Eval.interval list) =
    if debug
    then
      interval_list
      |> List.iter ~f:(fun ({ left_b; right_b } : Eval.interval) ->
           Caml.Format.printf "(%.8f, %.8f) " left_b right_b)
  ;;

  let log_ans debug (answer : Eval.answer) =
    if debug then Caml.Format.printf "x: %.8f, delta: %.8f\n" answer.x answer.delta
  ;;
end

let float_equal_precision f1 f2 precision =
  if Float.( < ) (Float.abs (f1 -. f2)) (1. **. (-1. *. Int.to_float precision))
  then true
  else Float.equal f1 f2
;;

let debug = false

let%test "root_sep_test1" =
  let conf : Eval.conf =
    { f = (fun x -> (x **. 2.) -. 5.)
    ; interval = { left_b = -5.; right_b = 5. }
    ; split_count = 50
    ; epsilon = 1.E-8
    }
  in
  let res = Eval.root_separation conf.f conf.interval conf.split_count in
  let expected_res : Eval.interval list =
    [ { left_b = -2.400000; right_b = -2.200000 }
    ; { left_b = 2.200000; right_b = 2.400000 }
    ]
  in
  let () =
    match res with
    | [] -> Caml.Format.printf "No roots\n"
    | lst -> lst |> V_Pprint.log_interv_list debug
  in
  List.equal
    (fun (int1 : Eval.interval) (int2 : Eval.interval) ->
      float_equal_precision int1.left_b int2.left_b 8
      && float_equal_precision int1.right_b int2.right_b 8)
    res
    expected_res
;;

let%test "bisect_test1" =
  let conf : Eval.conf =
    { f = (fun x -> (x **. 2.) -. 5.)
    ; interval = { left_b = -5.; right_b = 5. }
    ; split_count = 50
    ; epsilon = 1.E-8
    }
  in
  let res = Eval.bisect conf.f { left_b = -2.400000; right_b = -2.200000 } conf.epsilon in
  let expected_res : Eval.answer = { x = 2.18437500; delta = -0.07187500 } in
  let () = V_Pprint.log_ans debug res in
  let () = V_Pprint.log_ans debug expected_res in
  float_equal_precision res.x expected_res.x 8
  && float_equal_precision res.delta expected_res.delta 8
;;

let%test "float_record_test1" =
  let res : Eval.interval list =
    [ { left_b = -2.400000; right_b = -2.200000 }
    ; { left_b = 2.200000; right_b = 2.400000 }
    ]
  in
  let expected_res : Eval.interval list =
    [ { left_b = -2.400000; right_b = -2.200000 }
    ; { left_b = 2.200000; right_b = 2.400000 }
    ]
  in
  List.equal
    (fun (int1 : Eval.interval) (int2 : Eval.interval) ->
      Float.equal int1.left_b int2.left_b && Float.equal int1.right_b int2.right_b)
    res
    expected_res
;;
