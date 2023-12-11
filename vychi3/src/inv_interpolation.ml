open Core

type f = float -> float [@@deriving show]
type table = (float * float) list [@@deriving show]
(* type table_list = table list [@@deriving show] *)

type interval =
  { left_b : float
  ; right_b : float
  }

type conf =
  { f : float -> float
  ; arg_val_pairs_count : int
  ; interval : interval
  ; table : table option
  ; x : float option
  ; deg : int option
  }

let table (conf : conf) =
  let open Float in
  let step =
    (conf.interval.right_b - conf.interval.left_b)
    / of_int Int.(conf.arg_val_pairs_count + 1)
  in
  let rec make_table (conf : conf) x step cond (table : table) =
    if cond x conf.interval
    then make_table conf (x - step) step cond ((x, conf.f x) :: table)
    else table
  in
  let cond_decreasing step x interval = x > interval.left_b + step in
  make_table conf (conf.interval.right_b - step) step (cond_decreasing (step / 2.)) []
;;

let sum terms = terms |> List.fold ~f:(fun acc x -> x +. acc) ~init:0.

let%expect_test "sum_test" =
  let () = Printf.printf "%f" (sum [ 1.; 4.; 5.; 1. ]) in
  [%expect {|11.000000|}]
;;

let prod terms = terms |> List.fold ~f:(fun acc x -> x *. acc) ~init:1.

let%expect_test "prod_test" =
  let () = Printf.printf "%f" (prod [ 1.; 4.; 5.; 1. ]) in
  [%expect {|20.000000|}]
;;

let proj_x table = List.map ~f:(fun (fx, _) -> fx) table

let float_precision (f1 : float) (f2 : float) precision =
  let open Float in
  if abs (f1 - f2) < 10. ** (-1. * Int.to_float precision)
  then true (* let _ = Caml.print_endline "this way" in *)
  else Float.equal f1 f2
;;

let lagrange ~rev conf =
  let open Float in
  let precision = 10 in
  let conftable, confx, _ =
    match conf.table, conf.x, conf.deg with
    | Some table, Some x, Some deg -> table, x, deg
    | _, _, _ -> failwith "\n"
  in
  (* let xi = sort_xi confdeg confx conftable in *)
  let xi = proj_x conftable in
  let approx_polyn xi x =
    let term fxk xk xi x =
      (List.filter_map
         ~f:(fun xi -> if float_precision xi xk precision then None else Some (x - xi))
         xi
       |> prod)
      / (List.filter_map
           ~f:(fun xi -> if float_precision xi xk precision then None else Some (xk - xi))
           xi
         |> prod)
      * fxk
    in
    List.map conftable ~f:(fun (xk, fxk) -> term fxk xk xi x) |> sum
  in
  ( approx_polyn xi
  , approx_polyn xi confx
  , if rev
    then abs (conf.f (approx_polyn xi confx) - confx)
    else abs (conf.f confx - approx_polyn xi confx) )
;;

let sort_table conf =
  let conftable, confx, confdeg =
    match conf.table, conf.x, conf.deg with
    | Some table, Some x, Some deg -> table, x, deg
    | _, _, _ -> failwith "\n"
  in
  let open Float in
  let compare greater lesser =
    let gr_diff, _, _ = greater in
    let less_diff, _, _ = lesser in
    if float_precision gr_diff less_diff 9
    then 0
    else if gr_diff > less_diff
    then 1
    else -1
  in
  (* let xi = proj_x conftable in *)
  let diffs = List.map ~f:(fun (xi, fxi) -> abs (confx - xi), xi, fxi) conftable in
  let sorted_diffs = List.sort ~compare diffs in
  List.take (List.map ~f:(fun (_, xi, fxi) -> xi, fxi) sorted_diffs) Int.(confdeg + 1)
;;

let inverse_table conf =
  let conftable =
    match conf.table with
    | None -> failwith "\n"
    | Some table -> table
  in
  conftable |> List.map ~f:(fun (x, fx) -> fx, x)
;;

let dump_data workspace_path name func interval n =
  (* let () = Core_unix.mkdir_p plot_path in *)
  let data_path = String.concat ~sep:"" [ workspace_path; name; "_data.txt" ] in
  let oc = Out_channel.create data_path in
  let () =
    { f = func; arg_val_pairs_count = n; interval; table = None; x = None; deg = None }
    |> table
    |> List.iter ~f:(fun (x, fx) ->
      let line = Printf.sprintf "%f %f\n" x fx in
      let () = Out_channel.output_string oc line in
      Out_channel.flush oc)
  in
  Out_channel.close oc
;;

let plot workspace_path name =
  let data_path = String.concat ~sep:"" [ workspace_path; name; "_data.txt" ] in
  let plot_path = String.concat ~sep:"" [ workspace_path; name; "_plot.png" ] in
  let command =
    Printf.sprintf
      {|gnuplot -e "set terminal png size 1000,800; set output '%s'; plot '%s' with lines"|}
      plot_path
      data_path
  in
  Core_unix.open_process_in command |> In_channel.input_all |> Printf.printf "%s\n"
;;

let plot_both workspace_path f_name poly_name =
  let data_f_path = String.concat ~sep:"" [ workspace_path; f_name; "_data.txt" ] in
  let data_poly_path = String.concat ~sep:"" [ workspace_path; poly_name; "_data.txt" ] in
  let plot_path =
    String.concat ~sep:"" [ workspace_path; f_name; poly_name; "_plot.png" ]
  in
  let command =
    Printf.sprintf
      {|gnuplot -e "set terminal png size 1000,800; set output '%s'; plot '%s' with lines, '%s' with lines"|}
      plot_path
      data_f_path
      data_poly_path
  in
  Core_unix.open_process_in command |> In_channel.input_all |> Printf.printf "%s\n"
;;
