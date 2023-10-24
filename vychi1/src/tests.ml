open Root_eval
open Base

let conf : conf =
  { f = (fun x -> (x **. 2.) -. 5.)
  ; df = Some (fun x -> 2. *. x)
  ; interval = { left_b = -5.; right_b = 5. }
  ; split_count = 50
  ; epsilon = 1.E-8
  }
;;

let%test "root_sep_test1" =
  let () = Log.str "root_sep_test1:" in
  let res = Eval.root_separation conf.f conf.interval conf.split_count in
  let expected_res : interval list =
    [ { left_b = -2.400000; right_b = -2.200000 }
    ; { left_b = 2.200000; right_b = 2.400000 }
    ]
  in
  let () =
    let () =
      match res with
      | [] -> Caml.Format.printf "No roots\n"
      | lst -> lst |> Log.interv_list debug
    in
    expected_res |> Log.interv_list debug
  in
  List.equal Eq.interval res expected_res
;;

let%test "bisect_test1" =
  let () = Log.str "bisect_test1:" in
  let interval = { left_b = -2.400000; right_b = -2.200000 } in
  let res = Eval.bisect_step conf.f interval conf.epsilon in
  let expected_res : answer =
    { meth = Bisect
    ; initial_root = -2.300000000
    ; iters = 23
    ; last_interv = Some { left_b = -2.236067986; right_b = -2.236067975 }
    ; approx_root = -2.236067981
    ; delta = 0.000000006
    }
  in
  let () = Log.ans_list debug [ res ] in
  let () = Log.ans_list debug [ expected_res ] in
  Eq.ans res expected_res
;;

let%test "bisect_method_test1" =
  (* let conf = { conf with epsilon = 1.E-3 } in *)
  let () = Log.str "bisect_method_test1:" in
  let res = Eval.bisect_method conf in
  let expected_res : answer list =
    [ { meth = Bisect
      ; initial_root = -2.300000000
      ; iters = 23
      ; last_interv = Some { left_b = -2.236067986; right_b = -2.236067975 }
      ; approx_root = -2.236067981
      ; delta = 0.000000006
      }
    ; { meth = Bisect
      ; initial_root = 2.300000000
      ; iters = 23
      ; last_interv = Some { left_b = 2.236067975; right_b = 2.236067986 }
      ; approx_root = 2.236067981
      ; delta = 0.000000006
      }
    ]
  in
  let () =
    let () =
      match res with
      | [] -> Caml.Format.printf "No roots\n"
      | lst -> lst |> Log.ans_list debug
    in
    expected_res |> Log.ans_list debug
  in
  Eq.ans_list res expected_res
;;

let%test "newton_test1" =
  let () = Log.str "newton_test1:" in
  let interval = { left_b = -2.400000; right_b = -2.200000 } in
  let res = Eval.newton_step conf.f conf.df interval conf.epsilon in
  let expected_res : answer =
    { meth = Newton
    ; initial_root = -2.300000000
    ; iters = 3
    ; last_interv = None
    ; approx_root = -2.236067977
    ; delta = 0.000000000
    }
  in
  let () = Log.ans_list debug [ res ] in
  let () = Log.ans_list debug [ expected_res ] in
  Eq.ans res expected_res
;;

let%test "newton_method_test1" =
  let () = Log.str "newton_method_test1:" in
  let res = Eval.newton_method conf in
  let expected_res : answer list =
    [ { meth = Bisect
      ; initial_root = -2.300000000
      ; iters = 3
      ; last_interv = None
      ; approx_root = -2.236067977
      ; delta = 0.000000000
      }
    ; { meth = Bisect
      ; initial_root = 2.300000000
      ; iters = 3
      ; last_interv = None
      ; approx_root = 2.236067977
      ; delta = 0.000000000
      }
    ]
  in
  let () =
    let () =
      match res with
      | [] -> Caml.Format.printf "No roots\n"
      | lst -> lst |> Log.ans_list debug
    in
    expected_res |> Log.ans_list debug
  in
  Eq.ans_list res expected_res
;;
