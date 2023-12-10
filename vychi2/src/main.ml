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

let print_and_return_table (tbl_conf : LI.tbl_conf) =
  let res = tbl_conf |> LI.Eval.table in
  let () = Printf.printf "%s\n" (LI.show_table res) in
  res
;;

let print_evaluation (conf : LI.conf) =
  let _ = conf in
  ()
;;

let eventloop func =
  let oc = Out_channel.stdout in
  let print_s s =
    let () = Printf.fprintf oc "%s: " s in
    Out_channel.flush oc
  in
  let print_q q =
    let () = Printf.fprintf oc "%s?: " q in
    Out_channel.flush oc
  in
  let ic = In_channel.stdin in
  let () = print_prereq () in
  let rec loop () =
    let () = print_s "Enter arg-val pairs count (m+1)" in
    let arg_val_pairs_count = In_channel.input_line_exn ic |> Int.of_string in
    let () = print_s "Enter left border of interval (a)" in
    let a = In_channel.input_line_exn ic |> Float.of_string in
    let () = print_s "Enter right border of interval (b)" in
    let b = In_channel.input_line_exn ic |> Float.of_string in
    let interval : L.Interpolation.interval = { left_b = a; right_b = b } in
    let _ = print_and_return_table { f = func; arg_val_pairs_count; interval } in
    let () = print_s "Enter interpolation point (x)" in
    let x = In_channel.input_line_exn ic |> Float.of_string in
    let () = print_s "Enter interpolation polynomial's degree (n)" in
    let power = In_channel.input_line_exn ic |> Int.of_string in
    let () = print_evaluation { f = func; arg_val_pairs_count; interval; x; power } in
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
