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

(* type c_meth =
   | Left_Rect_Method
   | Right_Rect_Method
   | Mid_Rect_Method
   | Trapezoid_Method
   | Simpsons_Method
   | Three_Eights_Method
   [@@deriving show { with_path = false }] *)

type qf_conf =
  { f : float -> float
  ; f_str : string
  ; integral : interval -> float
  ; interval : interval
  }

type qf_ans =
  { meth : meth
  ; result : float
  ; abs_discrepancy : float
  ; relative_discrepancy : float
  }

let print_qf_ans qf_ans =
  Printf.printf
    "meth = %s\nresult = %f\nabs_discrepancy = %f\nrelative_discrepancy = %f\n\n"
    (show_meth qf_ans.meth)
    qf_ans.result
    qf_ans.abs_discrepancy
    qf_ans.relative_discrepancy
;;

let left_rect_meth qf_conf =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) * f a
;;

let right_rect_meth qf_conf =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) * f b
;;

let middle_rect_meth qf_conf =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) * f ((a + b) / 2.)
;;

let trapezoid_meth qf_conf =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) / 2. * (f a + f b)
;;

let simpsons_meth qf_conf =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  (b - a) / 6. * (f a + (4. * f ((a + b) / 2.)) + f b)
;;

let three_eights_meth qf_conf =
  let open Float in
  let a, b, f = qf_conf.interval.left_b, qf_conf.interval.right_b, qf_conf.f in
  let h = (b - a) / 3. in
  (b - a)
  * ((f a / 8.) + (3. * f (a + h) / 8.) + (3. * f (a + (2. * h)) / 8.) + (f b / 8.))
;;
