open Base

let debug = false

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

  let get_middle interval = (interval.right_b +. interval.left_b) /. 2.

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
      (* let () =
        if debug then Caml.Format.printf "a: %f, b: %f\n" interval.left_b interval.right_b
      in *)
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
           Caml.Format.printf "(%.9f, %.9f) " left_b right_b)
  ;;

  let log_ans debug (answer : Eval.answer) =
    if debug then Caml.Format.printf "x: %.9f; delta: %.9f\n" answer.x answer.delta
  ;;

  let log_ans_list debug (answer_list : Eval.answer list) =
    if debug
    then
      answer_list
      |> List.iter ~f:(fun ({ x; delta } : Eval.answer) ->
           Caml.Format.printf "(x: %.9f; delta: %.9f) " x delta)
  ;;
end

let float_equal_precision f1 f2 precision =
  if Float.( < ) (Float.abs (f1 -. f2)) (1. **. (-1. *. Int.to_float precision))
  then true
  else Float.equal f1 f2
;;

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
      float_equal_precision int1.left_b int2.left_b 9
      && float_equal_precision int1.right_b int2.right_b 9)
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
  let expected_res : Eval.answer = { x = -2.236067981; delta = 0.000000006 } in
  let () = V_Pprint.log_ans debug res in
  let () = V_Pprint.log_ans debug expected_res in
  float_equal_precision res.x expected_res.x 9
  && float_equal_precision res.delta expected_res.delta 9
;;

let%test "bisect_method_test1" =
  let conf : Eval.conf =
    { f = (fun x -> (x **. 2.) -. 5.)
    ; interval = { left_b = -5.; right_b = 5. }
    ; split_count = 2
    ; epsilon = 1.E-3
    }
  in
  let res = Eval.bisect_method conf in
  let expected_res : Eval.answer list =
    [ { x = -2.235717773; delta = 0.000610352 }
    ; { x = 2.235717773; delta = 0.000610352 }
    ]
  in
  let () =
    match res with
    | [] -> Caml.Format.printf "No roots\n"
    | lst -> lst |> V_Pprint.log_ans_list debug
  in
  List.equal
    (fun (int1 : Eval.answer) (int2 : Eval.answer) ->
      float_equal_precision int1.x int2.x 8
      && float_equal_precision int1.delta int2.delta 8)
    res
    expected_res
;;
