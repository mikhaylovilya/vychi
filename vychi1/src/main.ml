open Base
open Root_eval

let _ =
  let args = Sys.get_argv () in
  match Array.length args with
  | 5 ->
    let (n : int), (a : float), (b : float), (epsilon : float) =
      ( Int.of_string args.(1)
      , Float.of_string args.(2)
      , Float.of_string args.(3)
      , Float.of_string args.(4) )
    in
    (* f @@ Float.of_string arg |> Float.to_string |> Caml.print_endline *)
    let f x = (2. **. -.x) +. (0.5 *. (x **. 2.)) -. 10. in
    let (conf : conf) =
      { f
      ; df = Some (fun x -> 2. *. x)
      ; interval = { left_b = a; right_b = b }
      ; split_count = n
      ; epsilon
      }
    in
    let () =
      Caml.Format.printf
        "Численные методы решения нелинейных уравнений\n\
        \ Функция: f(x) = (2 ** -x) + (0.5 * (x ** 2)) - 10\n\
        \ Параметр задачи N: %d\n\
        \ Отрезок: [%.10f, %.10f]\n\
        \ Епсилон: %.10f\n"
        n
        a
        b
        epsilon
    in
    let root_sep_list = Eval.root_separation conf.f conf.interval conf.split_count in
    let () = Log.interv_list ~msg:"Отрезки перемены знака" true root_sep_list in
    let () = Log.ans_list ~msg:"Метод бисекции" true (Eval.bisect_method conf) in
    let () = Log.ans_list ~msg:"Метод Ньютона" true (Eval.newton_method conf) in
    let () =
      Log.ans_list
        ~msg:"Модифицированный метод Ньютона"
        true
        (Eval.mod_newton_method conf)
    in
    Log.ans_list ~msg:"Метод секущих" true (Eval.secant_method conf)
  | _ -> Caml.Format.printf "invalid args\n"
;;

(* let _ = Caml.print_endline "Hello from main.ml" *)
