open Base

let debug = false

type interval =
  { left_b : float
  ; right_b : float
  }

type meth =
  | Bisect
  | Newton
  | Newton_Mod
  | Secant

type answer =
  { meth : meth
  ; initial_root : float
  ; iters : int
  ; approx_root : float
  ; last_interv : interval option
  ; delta : float
  }

type conf =
  { f : float -> float
  ; df : (float -> float) option
  ; interval : interval
  ; split_count : int
  ; epsilon : float
  }

(* why the hell should i do that? *)
let ( <. ) = Float.( < )
let ( >. ) = Float.( > )
let ( <=. ) = Float.( <= )
let ( >=. ) = Float.( >= )

module Eval = struct
  type f = float -> float
  type df = float -> float

  let get_middle interval = (interval.right_b +. interval.left_b) /. 2.

  let root_separation f interval split_count =
    let h = (interval.right_b -. interval.left_b) /. Int.to_float split_count in
    let rec loop f interval h acc =
      match interval.left_b +. h with
      | new_lb when new_lb <=. interval.right_b ->
        let new_acc =
          if f interval.left_b *. f new_lb <=. 0.
          then { interval with right_b = new_lb } :: acc
          else acc
        in
        loop f { interval with left_b = new_lb } h new_acc
      | _ -> List.rev acc
    in
    loop f interval h []
  ;;

  let bisect_step f interval epsilon =
    let rec loop f interval epsilon iters =
      let middle = get_middle interval in
      let halved_interval =
        if f interval.left_b *. f middle <. 0.
        then { interval with right_b = middle }
        else { interval with left_b = middle }
      in
      match halved_interval with
      | { left_b; right_b } when right_b -. left_b <. 2. *. epsilon ->
        let last_interv, approx_root, delta =
          Some { left_b; right_b }, (left_b +. right_b) /. 2., (right_b -. left_b) /. 2.
        in
        iters, last_interv, approx_root, delta
      | _ -> loop f halved_interval epsilon (iters + 1)
    in
    let iters, last_interv, approx_root, delta = loop f interval epsilon 0 in
    { meth = Bisect
    ; initial_root = get_middle interval
    ; iters
    ; last_interv
    ; approx_root
    ; delta
    }
  ;;

  let bisect_method conf =
    let partition = root_separation conf.f conf.interval conf.split_count in
    partition |> List.map ~f:(fun int -> bisect_step conf.f int conf.epsilon)
  ;;

  let newton_step f df interval epsilon =
    let df =
      match df with
      | Some x -> x
      | None ->
        let exception No_derivative of string in
        raise (No_derivative "No derivative for function was provided")
    in
    let initial_root = get_middle interval in
    let rec loop f df epsilon prev_root iters =
      match prev_root -. (f prev_root /. df prev_root) with
      | curr_root when Float.abs (curr_root -. prev_root) <. epsilon ->
        let last_interv, approx_root, delta =
          None, curr_root, Float.abs (curr_root -. prev_root) /. 2.
        in
        last_interv, approx_root, delta, iters
      | root -> loop f df epsilon root (iters + 1)
    in
    let last_interv, approx_root, delta, iters = loop f df epsilon initial_root 0 in
    { meth = Newton; initial_root; iters; last_interv; approx_root; delta }
  ;;

  let newton_method conf =
    let partition = root_separation conf.f conf.interval conf.split_count in
    partition
    |> List.map ~f:(fun interval -> newton_step conf.f conf.df interval conf.epsilon)
  ;;

  let mod_newton_step = ()
  let mod_newton_method = ()
  let secant_step = ()
  let secant_method = ()
end

module Log = struct
  let str text = if debug then Caml.Format.printf "%s\n" text

  let interv_list debug (interval_list : interval list) =
    if debug
    then (
      let _ = Caml.Format.printf "Log.interv_list: \n" in
      let _ =
        interval_list
        |> List.iter ~f:(fun ({ left_b; right_b } : interval) ->
          Caml.Format.printf "  (%.9f, %.9f)\n" left_b right_b)
      in
      Caml.Format.printf "\n")
  ;;

  let ans debug (answer : answer) =
    if debug
    then (
      let last_interv_log =
        match answer.last_interv with
        | None -> "None"
        | Some interv -> Caml.Format.sprintf "(%.9f, %.9f)" interv.left_b interv.right_b
      in
      Caml.Format.printf
        "initial_root: %.9f; iterations: %d; last interval: %s; approximate root: %.9f; \
         delta: %.9f \n"
        answer.initial_root
        answer.iters
        last_interv_log
        answer.approx_root
        answer.delta)
  ;;

  let ans_list debug (answer_list : answer list) =
    if debug
    then (
      let _ = Caml.Format.printf "Log.ans_list: \n" in
      let _ =
        answer_list
        |> List.iter ~f:(fun a ->
          let _ = Caml.Format.printf "  " in
          ans true a)
      in
      Caml.Format.printf "\n")
  ;;
end

module Eq = struct
  let float_precision f1 f2 precision =
    if Float.abs (f1 -. f2) <. 1. **. (-1. *. Int.to_float precision)
    then true
    else Float.equal f1 f2
  ;;

  let interval int1 int2 =
    float_precision int1.left_b int2.left_b 9
    && float_precision int1.right_b int2.right_b 9
  ;;

  let ans ans1 ans2 =
    let last_interv_flag =
      match ans1.last_interv, ans2.last_interv with
      | None, None -> true
      | Some li1, Some li2 -> interval li1 li2
      | _ -> false
    in
    last_interv_flag
    && float_precision ans1.initial_root ans2.initial_root 9
    && Int.equal ans1.iters ans2.iters
    && float_precision ans1.approx_root ans2.approx_root 9
    && float_precision ans1.delta ans2.delta 9
  ;;

  let ans_list al1 al2 = List.equal ans al1 al2
end

let conf : conf =
  { f = (fun x -> (x **. 2.) -. 5.)
  ; df = Some (fun x -> 2. *. x)
  ; interval = { left_b = -5.; right_b = 5. }
  ; split_count = 50
  ; epsilon = 1.E-8
  }
;;

let%test "root_sep_test1" =
  let () = Log.str "root_sep_test1:" in
  let res = Eval.root_separation conf.f conf.interval conf.split_count in
  let expected_res : interval list =
    [ { left_b = -2.400000; right_b = -2.200000 }
    ; { left_b = 2.200000; right_b = 2.400000 }
    ]
  in
  let () =
    let () =
      match res with
      | [] -> Caml.Format.printf "No roots\n"
      | lst -> lst |> Log.interv_list debug
    in
    expected_res |> Log.interv_list debug
  in
  List.equal Eq.interval res expected_res
;;

let%test "bisect_test1" =
  let () = Log.str "bisect_test1:" in
  let interval = { left_b = -2.400000; right_b = -2.200000 } in
  let res = Eval.bisect_step conf.f interval conf.epsilon in
  let expected_res : answer =
    { meth = Bisect
    ; initial_root = -2.300000000
    ; iters = 23
    ; last_interv = Some { left_b = -2.236067986; right_b = -2.236067975 }
    ; approx_root = -2.236067981
    ; delta = 0.000000006
    }
  in
  let () = Log.ans_list debug [ res ] in
  let () = Log.ans_list debug [ expected_res ] in
  Eq.ans res expected_res
;;

let%test "bisect_method_test1" =
  (* let conf = { conf with epsilon = 1.E-3 } in *)
  let () = Log.str "bisect_method_test1:" in
  let res = Eval.bisect_method conf in
  let expected_res : answer list =
    [ { meth = Bisect
      ; initial_root = -2.300000000
      ; iters = 23
      ; last_interv = Some { left_b = -2.236067986; right_b = -2.236067975 }
      ; approx_root = -2.236067981
      ; delta = 0.000000006
      }
    ; { meth = Bisect
      ; initial_root = 2.300000000
      ; iters = 23
      ; last_interv = Some { left_b = 2.236067975; right_b = 2.236067986 }
      ; approx_root = 2.236067981
      ; delta = 0.000000006
      }
    ]
  in
  let () =
    let () =
      match res with
      | [] -> Caml.Format.printf "No roots\n"
      | lst -> lst |> Log.ans_list debug
    in
    expected_res |> Log.ans_list debug
  in
  Eq.ans_list res expected_res
;;

let%test "newton_test1" =
  let () = Log.str "newton_test1:" in
  let interval = { left_b = -2.400000; right_b = -2.200000 } in
  let res = Eval.newton_step conf.f conf.df interval conf.epsilon in
  let expected_res : answer =
    { meth = Newton
    ; initial_root = -2.300000000
    ; iters = 3
    ; last_interv = None
    ; approx_root = -2.236067977
    ; delta = 0.000000000
    }
  in
  let () = Log.ans_list debug [ res ] in
  let () = Log.ans_list debug [ expected_res ] in
  Eq.ans res expected_res
;;

let%test "newton_method_test1" =
  (* let conf = { conf with epsilon = 1.E-3 } in *)
  let () = Log.str "newton_method_test1:" in
  let res = Eval.newton_method conf in
  let expected_res : answer list =
    [ { meth = Bisect
      ; initial_root = -2.300000000
      ; iters = 3
      ; last_interv = None
      ; approx_root = -2.236067977
      ; delta = 0.000000000
      }
    ; { meth = Bisect
      ; initial_root = 2.300000000
      ; iters = 3
      ; last_interv = None
      ; approx_root = 2.236067977
      ; delta = 0.000000000
      }
    ]
  in
  let () =
    let () =
      match res with
      | [] -> Caml.Format.printf "No roots\n"
      | lst -> lst |> Log.ans_list debug
    in
    expected_res |> Log.ans_list debug
  in
  Eq.ans_list res expected_res
;;
