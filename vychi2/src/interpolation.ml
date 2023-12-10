open Core

type f = float -> float [@@deriving show]
type table = (float * float) list [@@deriving show]

type interval =
  { left_b : float
  ; right_b : float
  }

type conf =
  { f : float -> float
  ; arg_val_pairs_count : int
  ; interval : interval
  ; x : float
  ; power : int
  }

type tbl_conf =
  { f : float -> float
  ; arg_val_pairs_count : int
  ; interval : interval
  }

type method_conf =
  { f : float -> float
  ; table : table
  ; interval : interval
  ; x : float
  ; power : int
  }

module Eval = struct
  let table (conf : tbl_conf) =
    let open Float in
    let step =
      (conf.interval.right_b - conf.interval.left_b) / of_int conf.arg_val_pairs_count
    in
    let rec make_table (conf : tbl_conf) x step cond (table : table) =
      if cond x conf.interval
      then make_table conf (x - step) step cond ((x, conf.f x) :: table)
      else table
    in
    let cond_decreasing x interval = x >= interval.left_b in
    make_table conf conf.interval.right_b step cond_decreasing []
  ;;

  let to_tbl_conf (conf : conf) =
    { f = conf.f
    ; arg_val_pairs_count = conf.arg_val_pairs_count
    ; interval = conf.interval
    }
  ;;

  let lagrange () = ()
end

let test_conf =
  { f = (fun x -> Float.exp (-.x) -. ((x **. 2.) /. 2.))
  ; arg_val_pairs_count = 21
  ; interval = { left_b = 0.; right_b = 1. }
  ; x = 0.5
  ; power = 11
  }
;;

let%expect_test "table test1" =
  (* let _ = Printf.printf "%s" (Eval.show_table [ 1., 2. ]) in *)
  let () =
    Printf.printf "%s" (show_table (test_conf |> Eval.to_tbl_conf |> Eval.table))
  in
  (* let _ = Eval.pp_table Format (Eval.table test_conf) in *)
  [%expect {|hello|}]
;;
