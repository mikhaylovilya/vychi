open! Core

(* let main conf () =
  (* a = 0, b = 1, m+1 = 21, n = 11 *)
  let _ = conf in
  ()
;;

let () =
  Core.Command.basic_spec
    ~summary:"Compile myfile/Compile myfiles"
    Command.Spec.(
      empty
      +> anon ("arg_val_pairs_count" %: int)
      +> anon ("interv_a" %: float)
      +> anon ("interv_b" %: float)
      +> anon ("x" %: float)
      +> anon ("power" %: int))
    (fun arg_val_pairs_count interv_a interv_b x power () ->
      let open Interpolation in
      let interval = { left_b = interv_a; right_b = interv_b } in
      let f x = Float.exp (-.x) -. ((x **. 2.) /. 2.) in
      main { f; arg_val_pairs_count; interval; x; power } ())
  |> Command_unix.run
;; *)
module L = Vychi2lib
module LI = Vychi2lib.Interpolation

let print_prereq () =
  Printf.printf "Задача алгебраического интерполирования\n (Вариант 12)\n"
;;

let print_table (table : LI.Eval.table) = Printf.printf "%s\n" (LI.Eval.show_table table)
let workspace_path = "/home/cy/Desktop/ocaml-rep/vychi/vychi2/src/"

let print_evaluation (conf : LI.Eval.conf) =
  let lagrange, discrepancy_l, delta_l = LI.Eval.lagrange conf in
  let newton, discrepancy_n, delta_n = LI.Eval.newton conf in
  let n = 5000 in
  let () =
    Printf.printf "Lagrange: Discrepancy = %f\nDelta = %f\n" discrepancy_l delta_l
  in
  let () = Printf.printf "Newton: Discrepancy = %f\nDelta = %f\n" discrepancy_n delta_n in
  let () = LI.Eval.dump_data workspace_path "lagrange" lagrange conf.interval n in
  let () = LI.Eval.dump_data workspace_path "newton" newton conf.interval n in
  let () = LI.Eval.dump_data workspace_path "f" conf.f conf.interval n in
  let () = LI.Eval.plot_both workspace_path "f" "lagrange" in
  let () = LI.Eval.plot_both workspace_path "f" "newton" in
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
    let interval : LI.Eval.interval = { left_b = a; right_b = b } in
    let (conf : LI.Eval.conf) =
      { f = func
      ; arg_val_pairs_count
      ; interval
      ; table = None
      ; x = Some 0.5
      ; deg = Some 11
      }
    in
    let table = LI.Eval.table conf in
    let () = print_table table in
    (*  *)
    let () = print_s "Enter interpolation point (x)" in
    let x = In_channel.input_line_exn ic |> Float.of_string in
    (*  *)
    let () = print_s "Enter interpolation polynomial's degree (n)" in
    let deg = In_channel.input_line_exn ic |> Int.of_string in
    (*  *)
    let conf = { conf with table = Some table; x = Some x; deg = Some deg } in
    let new_table = LI.Eval.sort_table conf in
    let () = print_table new_table in
    let conf = { conf with table = Some new_table } in
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
