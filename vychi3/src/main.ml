open Core
module VI = Vychi3lib.Inv_interpolation
module VD = Vychi3lib.Num_differentiation

let workspace_path = "/home/cy/Desktop/ocaml-rep/vychi/vychi3/src/"

let print_prereq () =
  Printf.printf
    "Задача обратного интерполирования 3.1\n\
     Задача численного дифференцирования 3.2\n\
     (Вариант 12)\n"
;;

let print_evaluation conf =
  let lagrange, p_fx, delta_l = VI.lagrange conf in
  let () =
    Printf.printf "Lagrange:P_n(f(x)) = %f\n|f(P_n(f(x))) - f(x)| = %f\n" p_fx delta_l
  in
  (* let _ = lagrange in *)
  let n = 5000 in
  let () = VI.dump_data workspace_path "lagrange" lagrange conf.interval n in
  let () = VI.dump_data workspace_path "f" conf.f conf.interval n in
  let () = VI.plot_both workspace_path "f" "lagrange" in
  ()
;;

let eventloop func =
  let oc = Out_channel.stdout in
  let ic = In_channel.stdin in
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
    let () = print_s "Enter arg-val pairs count (m+1)" in
    let arg_val_pairs_count = In_channel.input_line_exn ic |> Int.of_string in
    (*  *)
    let () = print_s "Enter left border of interval (a)" in
    let a = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let () = print_s "Enter right border of interval (b)" in
    let b = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let interval : VI.interval = { left_b = a; right_b = b } in
    let (conf : VI.conf) =
      { f = func; arg_val_pairs_count; interval; table = None; fx = None; deg = None }
    in
    let table = VI.table conf in
    let () = VI.show_table table |> Printf.printf "%s\n" in
    (*  *)
    let () = print_s "Enter interpolation point (f(x))" in
    let fx = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let () = print_s "Enter interpolation polynomial's degree (n)" in
    let deg = In_channel.input_line_exn ic |> Int.of_string in
    (*  *)
    let conf = { conf with table = Some table; fx = Some fx; deg = Some deg } in
    let inv_table = VI.inverse_table conf in
    let conf = { conf with table = Some inv_table; fx = Some fx; deg = Some deg } in
    let new_table = VI.sort_table conf in
    let () = VI.show_table new_table |> Printf.printf "%s\n" in
    let conf = { conf with table = Some new_table } in
    let _ = conf in
    let () =
      try print_evaluation conf with
      | Failure msg -> Printf.printf "%s\n" msg
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
  let f x = exp (-x) - ((x ** 2.) / 2.) in
  eventloop f
;;
