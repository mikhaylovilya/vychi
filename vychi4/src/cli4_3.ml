open Core
module QF = Vychi4lib.Quadrature_formulas

let print_ans_list (conf : QF.cqf_conf) =
  let open QF in
  let open Float in
  let meth_list =
    [ compound_left_rect_meth, Compound_Left_Rect_Method, 1. / 2., 0
    ; compound_right_rect_meth, Compound_Right_Rect_Method, 1. / 2., 0
    ; compound_middle_rect_meth, Compound_Mid_Rect_Method, 1. / 24., 1
    ; compound_trapezoid_meth, Compound_Trapezoid_Method, 1. / 12., 1
    ; compound_simpsons_meth, Compound_Simpsons_Method, 1. / 2880., 3
    ]
  in
  let () =
    Printf.printf
      "function : %s\nanalytical result = %f\n\n"
      conf.f_str
      (conf.integral conf.interval)
  in
  meth_list
  |> List.iter ~f:(fun (meth_f, c_meth, const, ast) ->
    let _ = const in
    let a, b, m, l, _ =
      conf.interval.left_b, conf.interval.right_b, conf.m, conf.l, conf.h
    in
    let initial_result = conf.integral conf.interval in
    let result = meth_f conf in
    let conf_m_l = { conf with m = Int.(m * l); h = (b - a) / of_int Int.(m * l) } in
    let result_m_l = meth_f conf_m_l in
    let abs_discrepancy = abs (initial_result - result_m_l) in
    let relative_discrepancy =
      match initial_result with
      | 0. -> nan
      | _ -> abs (initial_result - result_m_l) / abs initial_result
    in
    let runge_result =
      (((of_int l ** of_int Int.(ast + 1)) * result_m_l) - result)
      / ((of_int l ** of_int Int.(ast + 1)) - 1.)
    in
    let abs_discrepancy_runge = abs (runge_result - initial_result) in
    let ans = { c_meth; result = result_m_l; abs_discrepancy; relative_discrepancy } in
    let () = ans |> print_cqf_ans in
    Printf.printf
      "runge's approximated result = %f\nrunge absolute discrepancy = %f\n\n"
      runge_result
      abs_discrepancy_runge)
;;

let eventloop func_list =
  let oc = Out_channel.stdout in
  let ic = In_channel.stdin in
  let print_prereq () =
    Printf.printf
      "Приближенное вычисление интеграла по составным квадратурным формулам 4.2\n\
       (Вариант 12)\n"
  in
  let print_s s =
    let () = Printf.fprintf oc "%s: " s in
    Out_channel.flush oc
  in
  let print_q q =
    let () = Printf.fprintf oc "%s?: " q in
    Out_channel.flush oc
  in
  let () = print_prereq () in
  let rec loop () =
    let () = print_s "Enter the interval of integration (a)" in
    let a = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let () = print_s "Enter the interval of integration (b)" in
    let b = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let () = print_s "Enter partition rate (m)" in
    let m = In_channel.input_line_exn ic |> Int.of_string in
    let () = print_s "Enter partition rate (l)" in
    let l = In_channel.input_line_exn ic |> Int.of_string in
    let h = (b -. a) /. Float.of_int m in
    (*  *)
    let interval : QF.interval = { left_b = a; right_b = b } in
    let () =
      func_list
      |> List.iter ~f:(fun (f, integral, f_str, der_list) ->
        let (conf : QF.cqf_conf) = { f; f_str; integral; der_list; interval; m; l; h } in
        print_ans_list conf)
    in
    (* let _ = m, interval, func_list, print_ans_list in *)
    let () = print_q "Continue" in
    match In_channel.input_line_exn ic with
    | "no" | "No" | "n" -> ()
    | "yes" | "Yes" | "y" -> loop ()
    | _ -> ()
  in
  loop ()
;;

let () =
  let open Float in
  let open QF in
  (* let df x = (-1. * sin x) - 2. in *)
  let f x = cos x - (2. * x) in
  let integral_f interval =
    let a, b = interval.left_b, interval.right_b in
    sin b - sin a + (a * a) - (b * b)
  in
  let f_0 x =
    let _ = x in
    1.
  in
  let integral_f_0 interval =
    let a, b = interval.left_b, interval.right_b in
    b - a
  in
  let f_1 x = 2. * x in
  let integral_f_1 interval =
    let a, b = interval.left_b, interval.right_b in
    (b * b) - (a * a)
  in
  let f_2 x = 3. * (x ** 2.) in
  let integral_f_2 interval =
    let a, b = interval.left_b, interval.right_b in
    (b ** 3.) - (a ** 3.)
  in
  let f_3 x = 4. * (x ** 3.) in
  let integral_f_3 interval =
    let a, b = interval.left_b, interval.right_b in
    (b ** 4.) - (a ** 4.)
  in
  let der_f_3 x = 12. * (x ** 2.) in
  let der_2_f_3 x = 24. * x in
  let der_3_f_3 x =
    let _ = x in
    24.
  in
  let der_4_f_3 x =
    let _ = x in
    0.
  in
  let func_list =
    [ f, integral_f, "f(x) = cos(x) - (2 * x)", []
    ; f_0, integral_f_0, "f(x) = 1", []
    ; f_1, integral_f_1, "f(x) = 2 * x", []
    ; f_2, integral_f_2, "f(x) = 3 * (x ** 2)", []
    ; ( f_3
      , integral_f_3
      , "f(x) = 4 * (x ** 3)"
      , [ der_f_3; der_2_f_3; der_3_f_3; der_4_f_3 ] )
    ]
  in
  eventloop func_list
;;
