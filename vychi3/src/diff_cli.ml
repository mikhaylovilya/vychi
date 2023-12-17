open Core
module V = Vychi3lib
module VD = Vychi3lib.Num_differentiation

let workspace_path = "/home/cy/Desktop/ocaml-rep/vychi/vychi3/src/"

let print_ans_list ans_list =
  List.iter ans_list ~f:(fun ans -> Printf.printf "%s\n" (VD.show_ans_3_2 ans))
;;

let eventloop f df ddf =
  let oc = Out_channel.stdout in
  let ic = In_channel.stdin in
  let print_prereq () =
    Printf.printf "Задача численного дифференцирования 3.2\n(Вариант 12)\n"
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
    let () = print_s "Enter arg-val pairs count (m+1)" in
    let arg_val_pairs_count = In_channel.input_line_exn ic |> Int.of_string in
    (*  *)
    let () = print_s "Enter left border of interval (a)" in
    let a = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let () = print_s "Enter step (h)" in
    let h = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    (* let interval : VD.interval = { left_b = a; right_b = b } in *)
    let (conf : VD.conf) = { f; df; ddf; arg_val_pairs_count; a; h; table = None } in
    let table = VD.table conf in
    let () = VD.show_table table |> Printf.printf "%s\n" in
    let conf = { conf with table = Some table } in
    let ans_list = VD.make_ans conf in
    let () = print_ans_list ans_list in
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
  let k = 3. in
  let f x = exp (1.5 * k * x) in
  let df x = 1.5 * k * exp (1.5 * k * x) in
  let ddf x = 1.5 * 1.5 * k * k * exp (1.5 * k * x) in
  eventloop f df ddf
;;
