open Core
module VI = Inv_interpolation

type f = VI.f
type table = VI.table
type interval = Root_eval.interval

type conf =
  { f : f
  ; df : f
  ; ddf : f
  ; arg_val_pairs_count : int
  ; a : float
  ; h : float
  ; table : table option
  }

type ans_3_2 =
  { xi : float
  ; f_xi : float
  ; df_xi : float
  ; abs_df_discrepancy : float
  ; ddf_xi : float
  ; abs_ddf_discrepancy : float
  }
[@@deriving show { with_path = false }]

(* let table (conf : conf) =
   let open Float in
   match conf.arg_val_pairs_count with
   | 1 ->
   let middle = (conf.interval.right_b - conf.interval.left_b) / 2. in
   [ middle, conf.f middle ]
   | 2 ->
   [ conf.interval.left_b, conf.f conf.interval.left_b
    ; conf.interval.right_b, conf.f conf.interval.right_b
    ]
   | n when Int.( > ) n 0 ->
   let step =
   (conf.interval.right_b - conf.interval.left_b)
   / of_int Int.(conf.arg_val_pairs_count + 1)
   in
   let rec make_table (conf : conf) x step cond (table : table) =
   if cond x conf.interval
   then make_table conf (x - step) step cond ((x, conf.f x) :: table)
   else table
   in
   let cond_decreasing step x (interval : interval) = x > interval.left_b + step in
   make_table conf (conf.interval.right_b - step) step (cond_decreasing (step / 2.)) []
   | _ -> failwith "Can't have table with 0 lines"
   ;; *)
let float_precision = VI.float_precision

let table (conf : conf) =
  let open Float in
  let rec make_table conf x iters table =
    if Int.( > ) iters Int.(conf.arg_val_pairs_count - 1)
    then List.rev table
    else make_table conf (x + conf.h) Int.(iters + 1) ((x, conf.f x) :: table)
  in
  make_table conf conf.a 0 []
;;

let make_ans conf =
  let open Float in
  (* let precision = 10 in *)
  let conf_table =
    match conf.table with
    | None -> failwith "No arg-val table was provided"
    | Some table -> table
  in
  let first_derivative conftable h i =
    (* let ind = Int.of_float (Float.round ((x - a) / h)) in *)
    match i with
    | 0 ->
      let (_, f_x), (_, f_xr), (_, f_xrr) =
        ( List.nth_exn conftable i
        , List.nth_exn conftable Int.(i + 1)
        , List.nth_exn conftable Int.(i + 2) )
      in
      ((-3. * f_x) + (4. * f_xr) - f_xrr) / 2. * h
    | n when Int.( = ) n Int.(List.length conf_table - 1) ->
      let (_, f_x), (_, f_xl), (_, f_xll) =
        ( List.nth_exn conftable i
        , List.nth_exn conftable Int.(i - 1)
        , List.nth_exn conftable Int.(i - 2) )
      in
      ((3. * f_x) - (4. * f_xl) + f_xll) / 2. * h
    | _ ->
      let (_, f_xl), (_, f_xr) =
        List.nth_exn conftable Int.(i - 1), List.nth_exn conftable Int.(i + 1)
      in
      (f_xr - f_xl) / (2. * h)
  in
  let second_derivative conftable h i =
    match i with
    | n when Int.( = ) n Int.(List.length conf_table - 1) || Int.( = ) n 0 -> Float.nan
    | _ ->
      let (_, f_x), (_, f_xl), (_, f_xr) =
        ( List.nth_exn conftable i
        , List.nth_exn conftable Int.(i - 1)
        , List.nth_exn conftable Int.(i + 1) )
      in
      (f_xr - (2. * f_x) + f_xl) / (h * h)
  in
  let first_derivative_tbl_h = first_derivative conf_table conf.h in
  let seconf_derivative_tbl_h = second_derivative conf_table conf.h in
  conf_table
  |> List.mapi ~f:(fun i (xi, f_xi) ->
    let df_xi = first_derivative_tbl_h i in
    let ddf_xi = seconf_derivative_tbl_h i in
    { xi
    ; f_xi
    ; df_xi
    ; abs_df_discrepancy = abs (conf.df xi - df_xi)
    ; ddf_xi
    ; abs_ddf_discrepancy = abs (conf.ddf xi - ddf_xi)
    })
;;

let show_table = VI.show_table
