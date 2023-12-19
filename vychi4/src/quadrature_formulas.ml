open Core

type interval =
  { left_b : float
  ; right_b : float
  }

type meth =
  | Left_Rect_Method
  | Right_Rect_Method
  | Mid_Rect_Method
  | Trapezoid_Method
  | Simpsons_Method
  | Three_Eights_Method
[@@deriving show { with_path = false }]

type c_meth =
  | Compound_Left_Rect_Method
  | Compound_Right_Rect_Method
  | Compound_Mid_Rect_Method
  | Compound_Trapezoid_Method
  | Compound_Simpsons_Method
[@@deriving show { with_path = false }]

type qf_conf =
  { f : float -> float
  ; f_str : string
  ; integral : interval -> float
  ; interval : interval
  }

type cqf_conf =
  { f : float -> float
  ; f_str : string
  ; integral : interval -> float
  ; der_list : (float -> float) list
  ; interval : interval
  ; m : int
  ; l : int
  ; h : float
  }

type qf_ans =
  { meth : meth
  ; result : float
  ; abs_discrepancy : float
  ; relative_discrepancy : float
  }

type cqf_ans =
  { c_meth : c_meth
  ; result : float
  ; abs_discrepancy : float
  ; relative_discrepancy : float
  }

let print_qf_ans (qf_ans : qf_ans) =
  Printf.printf
    "meth = %s\nresult = %f\nabs_discrepancy = %f\nrelative_discrepancy = %f\n\n"
    (show_meth qf_ans.meth)
    qf_ans.result
    qf_ans.abs_discrepancy
    qf_ans.relative_discrepancy
;;

let print_cqf_ans (cqf_ans : cqf_ans) =
  Printf.printf
    "meth = %s\nresult = %f\nabs_discrepancy = %f\nrelative_discrepancy = %f\n"
    (show_c_meth cqf_ans.c_meth)
    cqf_ans.result
    cqf_ans.abs_discrepancy
    cqf_ans.relative_discrepancy
;;

let left_rect_meth (qf_conf : qf_conf) =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) * f a
;;

(* let sum_in_range low up = List.range low up |> List.iter ~f:(Printf.printf "%d ")

   let%expect_test "sum_in_range" =
   let () = sum_in_range (-1) 5 in
   [%expect {|erhm|}]
   ;; *)

let compound_left_rect_meth cqf_conf =
  let open Float in
  let a, _, f, m, h =
    ( cqf_conf.interval.left_b
    , cqf_conf.interval.right_b
    , cqf_conf.f
    , cqf_conf.m
    , cqf_conf.h )
  in
  h
  * (List.range 0 m
     |> List.map ~f:(fun i -> f (a + (of_int i * h)))
     |> List.fold ~f:(fun acc x -> x + acc) ~init:0.)
;;

let right_rect_meth (qf_conf : qf_conf) =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) * f b
;;

let compound_right_rect_meth cqf_conf =
  let open Float in
  let a, _, f, m, h =
    ( cqf_conf.interval.left_b
    , cqf_conf.interval.right_b
    , cqf_conf.f
    , cqf_conf.m
    , cqf_conf.h )
  in
  h
  * (List.range 1 Int.(m + 1)
     |> List.map ~f:(fun i -> f (a + (of_int i * h)))
     |> List.fold ~f:(fun acc x -> x + acc) ~init:0.)
;;

let middle_rect_meth (qf_conf : qf_conf) =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) * f ((a + b) / 2.)
;;

let compound_middle_rect_meth cqf_conf =
  let open Float in
  let a, _, f, m, h =
    ( cqf_conf.interval.left_b
    , cqf_conf.interval.right_b
    , cqf_conf.f
    , cqf_conf.m
    , cqf_conf.h )
  in
  h
  * (List.range 0 m
     |> List.map ~f:(fun i -> f (a + (h * of_int i) + (h / 2.)))
     |> List.fold ~f:(fun acc x -> x + acc) ~init:0.)
;;

let trapezoid_meth (qf_conf : qf_conf) =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) / 2. * (f a + f b)
;;

let compound_trapezoid_meth cqf_conf =
  let open Float in
  (compound_left_rect_meth cqf_conf + compound_right_rect_meth cqf_conf) / 2.
;;

let simpsons_meth (qf_conf : qf_conf) =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) / 6. * (f a + (4. * f ((a + b) / 2.)) + f b)
;;

let compound_simpsons_meth cqf_conf =
  let open Float in
  (compound_left_rect_meth cqf_conf
   + compound_right_rect_meth cqf_conf
   + (4. * compound_middle_rect_meth cqf_conf))
  / 6.
;;

let three_eights_meth (qf_conf : qf_conf) =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  let h = (b - a) / 3. in
  (b - a)
  * ((f a / 8.) + (3. * f (a + h) / 8.) + (3. * f (a + (2. * h)) / 8.) + (f b / 8.))
;;
