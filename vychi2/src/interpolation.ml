open Core

module Eval = struct
  type f = float -> float [@@deriving show]
  type table = (float * float) list [@@deriving show]
  type table_list = table list [@@deriving show]

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
    let cond_decreasing step x interval = x >= interval.left_b + step in
    make_table conf (conf.interval.right_b - step) step (cond_decreasing step) []
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

  let float_precision f1 f2 precision =
    let open Float in
    if abs (f1 -. f2) < 10. **. (-1. *. Int.to_float precision)
    then true (* let _ = Caml.print_endline "this way" in *)
    else Float.equal f1 f2
  ;;

  let proj_x table = List.map ~f:(fun (x, _) -> x) table

  let sort_table conf =
    let conftable, confx, confdeg =
      match conf.table, conf.x, conf.deg with
      | Some table, Some x, Some deg -> table, x, deg
      | _, _, _ -> failwith "\n"
    in
    (* match confdeg - (conf.arg_val_pairs_count - 1) with
       | 0 -> conftable
       | _ -> *)
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

  let lagrange conf =
    let open Float in
    let conftable, confx, _ =
      match conf.table, conf.x, conf.deg with
      | Some table, Some x, Some deg -> table, x, deg
      | _, _, _ -> failwith "\n"
    in
    (* let xi = sort_xi confdeg confx conftable in *)
    let xi = proj_x conftable in
    let approx_polyn xi x =
      let term fxk xk xi x =
        (List.filter_map ~f:(fun xi -> if xi = xk then None else Some (x - xi)) xi |> prod)
        / (List.filter_map ~f:(fun xi -> if xi = xk then None else Some (xk - xi)) xi
           |> prod)
        * fxk
      in
      List.map conftable ~f:(fun (xk, fxk) -> term fxk xk xi x) |> sum
    in
    approx_polyn xi, approx_polyn xi confx, abs (conf.f confx - approx_polyn xi confx)
  ;;

  let segment_tables tables =
    let rec helper tables acc =
      match tables with
      | [] -> []
      | hd :: tl -> (hd :: acc) :: helper tl (hd :: acc)
    in
    helper tables []
  ;;

  let%expect_test "tables_test" =
    let () = Printf.printf "%s\n" (show_table [ 1., 2.; 2., 4.; 3., 6. ]) in
    let () =
      Printf.printf "%s\n" (show_table_list (segment_tables [ 1., 2.; 2., 4.; 3., 6. ]))
    in
    [%expect {|[ [ 1., 2. ]; [ 1., 2.; 2., 4. ]; [ 1., 2.; 2., 4.; 3., 6. ] ]|}]
  ;;

  let newton conf =
    let open Float in
    let conftable, confx, _ =
      match conf.table, conf.x, conf.deg with
      | Some table, Some x, Some deg -> table, x, deg
      | _, _, _ -> failwith "\n"
    in
    let approx_polyn table x =
      let rec divided_difference table =
        match table with
        | [ (_, fx_0) ] -> fx_0
        | list ->
          let no_head = List.tl_exn list in
          let no_last = List.drop_last_exn list in
          let hd_x, _ = List.hd_exn list in
          let lst_x, _ = List.last_exn list in
          (divided_difference no_head - divided_difference no_last) / (lst_x - hd_x)
      in
      let term table_k x =
        if Int.(List.length table_k = 1)
        then divided_difference table_k
        else
          divided_difference table_k
          * (List.map ~f:(fun (xk, _) -> x - xk) (List.drop_last_exn table_k) |> prod)
      in
      table |> segment_tables |> List.map ~f:(fun segm_tbl -> term segm_tbl x) |> sum
    in
    ( approx_polyn conftable
    , approx_polyn conftable confx
    , abs (conf.f confx - approx_polyn conftable confx) )
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
    let data_poly_path =
      String.concat ~sep:"" [ workspace_path; poly_name; "_data.txt" ]
    in
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
end

let (test_conf : Eval.conf) =
  { f = (fun x -> Float.exp (-.x) -. ((x **. 2.) /. 2.))
  ; arg_val_pairs_count = 21
  ; interval = { left_b = 0.; right_b = 1. }
  ; table = None
  ; x = Some 0.5
  ; deg = Some 11
  }
;;

let%expect_test "table test1" =
  (* let _ = Printf.printf "%s" (Eval.show_table [ 1., 2. ]) in *)
  let () = Printf.printf "%s" (Eval.show_table (test_conf |> Eval.table)) in
  (* let _ = Eval.pp_table Format (Eval.table test_conf) in *)
  [%expect
    {|
    [(0.0454545454545, 0.954529978417); (0.0909090909091, 0.908968484877);
      (0.136363636364, 0.863227772208); (0.181818181818, 0.817223992455);
      (0.227272727273, 0.770877023612); (0.272727272727, 0.724110304052);
      (0.318181818182, 0.676850674314); (0.363636363636, 0.62902822592);
      (0.409090909091, 0.580576156914); (0.454545454545, 0.531430633816);
      (0.5, 0.481530659713); (0.545454545455, 0.430817948206);
      (0.590909090909, 0.379236802971); (0.636363636364, 0.326734002657);
      (0.681818181818, 0.273258690909); (0.727272727273, 0.218762271272);
      (0.772727272727, 0.163198306774); (0.818181818182, 0.106522423958);
      (0.863636363636, 0.0486922211895); (0.909090909091, -0.0103328189667);
      (0.954545454545, -0.0705914134732)]|}]
;;
