open Core
module QF = Vychi4lib.Quadrature_formulas

let print_ans_list (conf : QF.qf_conf) =
  let open QF in
  let open Float in
  let meth_list =
    [ left_rect_meth, Left_Rect_Method
    ; right_rect_meth, Right_Rect_Method
    ; middle_rect_meth, Mid_Rect_Method
    ; trapezoid_meth, Trapezoid_Method
    ; simpsons_meth, Simpsons_Method
    ; three_eights_meth, Three_Eights_Method
    ]
  in
  let () =
    Printf.printf
      "function : %s\nanalytical result = %f\n\n"
      conf.f_str
      (conf.integral conf.interval)
  in
  meth_list
  |> List.iter ~f:(fun (meth_f, meth) ->
    let initial_result = conf.integral conf.interval in
    let result = meth_f conf in
    let abs_discrepancy = abs (initial_result - result) in
    let relative_discrepancy =
      match initial_result with
      | 0. -> nan
      | _ -> abs (initial_result - result) / abs initial_result
    in
    let ans = { meth; result; abs_discrepancy; relative_discrepancy } in
    ans |> print_qf_ans)
;;

let eventloop func_list =
  let oc = Out_channel.stdout in
  let ic = In_channel.stdin in
  let print_prereq () =
    Printf.printf
      "Приближенное вычисление интеграла по квадратурным формулам 4.1\n(Вариант 12)\n"
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
    let interval : QF.interval = { left_b = a; right_b = b } in
    let () =
      func_list
      |> List.iter ~f:(fun (f, integral, f_str) ->
        let (conf : QF.qf_conf) = { f; f_str; integral; interval } in
        print_ans_list conf)
    in
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
  let func_list =
    [ f, integral_f, "f(x) = cos(x) - (2 * x)"
    ; f_0, integral_f_0, "f(x) = 1"
    ; f_1, integral_f_1, "f(x) = 2 * x"
    ; f_2, integral_f_2, "f(x) = 3 * (x ** 2)"
    ; f_3, integral_f_3, "f(x) = 4 * (x ** 3)"
    ]
  in
  eventloop func_list
;;
